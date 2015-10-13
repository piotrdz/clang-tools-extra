//===--- LocalizingVariablesCheck.cpp - clang-tidy-------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "LocalizingVariablesCheck.h"
#include "clang/AST/ASTContext.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"

using namespace clang::ast_matchers;

namespace clang {
namespace tidy {

namespace {

struct ScopeContext {
  const Stmt* ScopeStatement;
  int Depth;
  SourceLocation InsertLocationForDeclarationsBeforeScope;
};

using ScopeContextContainer = llvm::SmallVector<ScopeContext, 5>;

struct DeclarationStatementContext {
  const DeclStmt* DeclarationStatement;
  int ScopeIndex;
  int VariablesDeclaredCount;
  int VariablesToBeRemovedCount;
};

using DeclarationStatementContextContainer = llvm::SmallVector<DeclarationStatementContext, 10>;

enum class VariableInsertType {
  AsDeclaration,
  AsAssignment
};

struct VariableUsageContext {
  const Stmt* UsageStatement;
  int ScopeIndex;
  SourceLocation InsertLocation;
  VariableInsertType InsertType;
};

using ScopeUsageContainer = llvm::SmallVector<VariableUsageContext, 10>;

struct VariableDeclarationContext {
  const VarDecl* VariableDeclaration;
  int DeclarationStatementIndex;
  ScopeUsageContainer ScopeUsages;
};

using VariableDeclarationContextContainer = llvm::SmallVector<VariableDeclarationContext, 10>;

using ScopeIndexContainer = llvm::SmallVector<int, 5>;

class LocalizingVariablesHandler {
public:
  LocalizingVariablesHandler(LocalizingVariablesCheck& Check);

  void onStartOfFunction();

  void processStatement(const Stmt* Statement);

  void onEndOfFunction();

private:
  void processCompoundStatement(const CompoundStmt* CompoundStatement);
  void processIfStatement(const IfStmt* IfStatement);
  void processForStatement(const ForStmt* ForStatement);
  void processWhileStatement(const WhileStmt* WhileStatement);
  void processDoStatement(const DoStmt* DoStatement);
  void processForRangeStatement(const CXXForRangeStmt* ForRangeStatement);
  void processTryStatement(const CXXTryStmt* TryStatement);
  void processCatchStatement(const CXXCatchStmt* CatchStatement);
  void processGenericStatement(const Stmt* GenericStatement);

  void processNestedScope(SourceLocation InsertLocationBeforeScope, const Stmt* NestedScope);
  void processCompoundNestedScope(SourceLocation InsertLocationBeforeScope, const CompoundStmt* CompoundNestedScope);
  void processSingleStatementNestedScope(SourceLocation InsertLocationBeforeScope, const Stmt* SingleStatementNestedScope);
  void processSingleStatement(SourceLocation InsertLocationBeforeStatement, const Stmt* GenericStatement);

  LocalizingVariablesCheck& Check;
  ScopeIndexContainer ScopeStack;
  ScopeContextContainer Scopes;
  DeclarationStatementContextContainer DeclarationStatements;
  VariableDeclarationContextContainer VariableDeclarations;
};

} // anonymous namespace

////////////////////////////////

void LocalizingVariablesCheck::registerMatchers(MatchFinder *Finder) {
  Finder->addMatcher(
    functionDecl(unless(anyOf(isImplicit(),
                              functionTemplateDecl(),
                              ast_matchers::isTemplateInstantiation())),
                 isDefinition())
      .bind("functionDecl"),
    this);
}

void LocalizingVariablesCheck::check(const MatchFinder::MatchResult &Result) {
  const auto *FunctionDeclaration = Result.Nodes.getNodeAs<FunctionDecl>("functionDecl");

  const auto* FunctionBody = llvm::dyn_cast_or_null<CompoundStmt>(FunctionDeclaration->getBody());
  if (FunctionBody == nullptr) {
    return;
  }

  LocalizingVariablesHandler Handler(*this);

  Handler.onStartOfFunction();

  for (const Stmt* BodyStatement : FunctionBody->body()) {
    Handler.processStatement(BodyStatement);
  }

  Handler.onEndOfFunction();
}

////////////////////////////////

LocalizingVariablesHandler::LocalizingVariablesHandler(LocalizingVariablesCheck& Check) : Check(Check) {}

void LocalizingVariablesHandler::onStartOfFunction() {
  // TODO: push primary function scope
}

void LocalizingVariablesHandler::processStatement(const Stmt* Statement) {
  switch (Statement->getStmtClass()) {
    case Stmt::CompoundStmtClass:
      return processCompoundStatement(llvm::cast<CompoundStmt>(Statement));

    case Stmt::IfStmtClass:
      return processIfStatement(llvm::cast<IfStmt>(Statement));

    case Stmt::ForStmtClass:
      return processForStatement(llvm::cast<ForStmt>(Statement));

    case Stmt::WhileStmtClass:
      return processWhileStatement(llvm::cast<WhileStmt>(Statement));

    case Stmt::DoStmtClass:
      return processDoStatement(llvm::cast<DoStmt>(Statement));

    case Stmt::CXXForRangeStmtClass:
      return processForRangeStatement(llvm::cast<CXXForRangeStmt>(Statement));

    case Stmt::CXXTryStmtClass:
      return processTryStatement(llvm::cast<CXXTryStmt>(Statement));

    case Stmt::CXXCatchStmtClass:
      return processCatchStatement(llvm::cast<CXXCatchStmt>(Statement));

    default:
      processGenericStatement(Statement);
  }
}

void LocalizingVariablesHandler::processCompoundStatement(const CompoundStmt* CompoundStatement) {
  processCompoundNestedScope(CompoundStatement->getLocStart(), CompoundStatement);
}

void LocalizingVariablesHandler::processIfStatement(const IfStmt* IfStatement) {
  // TODO: process if expression, body, but how does it work?
}

void LocalizingVariablesHandler::processForStatement(const ForStmt* ForStatement) {
  processSingleStatement(ForStatement->getLocStart(), ForStatement->getInit());
  processSingleStatement(ForStatement->getLocStart(), ForStatement->getCond());
  processSingleStatement(ForStatement->getLocStart(), ForStatement->getInc());
  processNestedScope(ForStatement->getLocStart(), ForStatement->getBody());
}

void LocalizingVariablesHandler::processWhileStatement(const WhileStmt* WhileStatement) {
  processSingleStatement(WhileStatement->getLocStart(), WhileStatement->getCond());
  processNestedScope(WhileStatement->getLocStart(), WhileStatement->getBody());
}

void LocalizingVariablesHandler::processDoStatement(const DoStmt* DoStatement) {
  processSingleStatement(DoStatement->getLocStart(), DoStatement->getCond());
  processNestedScope(DoStatement->getLocStart(), DoStatement->getBody());
}

void LocalizingVariablesHandler::processForRangeStatement(const CXXForRangeStmt* ForRangeStatement) {
  processSingleStatement(ForRangeStatement->getLocStart(), ForRangeStatement->getRangeStmt());
  processNestedScope(ForRangeStatement->getLocStart(), ForRangeStatement->getBody());
}

void LocalizingVariablesHandler::processTryStatement(const CXXTryStmt* TryStatement) {
  processCompoundNestedScope(TryStatement->getLocStart(), TryStatement->getTryBlock());
}

void LocalizingVariablesHandler::processCatchStatement(const CXXCatchStmt* CatchStatement) {
  processNestedScope(CatchStatement->getLocStart(), CatchStatement->getHandlerBlock());
}

void LocalizingVariablesHandler::processGenericStatement(const Stmt* GenericStatement) {
  processSingleStatement(GenericStatement->getLocStart(), GenericStatement);
}

void LocalizingVariablesHandler::processNestedScope(SourceLocation InsertLocationBeforeScope, const Stmt* NestedScope) {
  const auto* CompoundNestedScope = llvm::dyn_cast_or_null<CompoundStmt>(NestedScope);
  if (CompoundNestedScope != nullptr) {
    processCompoundNestedScope(InsertLocationBeforeScope, CompoundNestedScope);
  } else {
    processSingleStatementNestedScope(InsertLocationBeforeScope, NestedScope);
  }
}

void LocalizingVariablesHandler::processCompoundNestedScope(SourceLocation InsertLocationBeforeScope, const CompoundStmt* CompoundNestedScope) {
  // TODO: create and push new scope context

  for (const auto* ScopeStatement : CompoundNestedScope->body()) {
    processStatement(ScopeStatement);
  }

  // TODO: pop scope context
}

void LocalizingVariablesHandler::processSingleStatementNestedScope(SourceLocation InsertLocationBeforeScope, const Stmt* SingleStatementNestedScope) {
  // TODO create and push new scope context

  processSingleStatement(InsertLocationBeforeScope, SingleStatementNestedScope);

  // TODO: pop scope context
}

void LocalizingVariablesHandler::processSingleStatement(SourceLocation InsertLocationBeforeStatement, const Stmt* GenericStatement) {
  // TODO: find all DeclRefStmt referring to variables in scope
  // TODO: add scope usage to appropriate variable context
}

void LocalizingVariablesHandler::onEndOfFunction() {
  // TODO: for all variables, generate diagnostics and FixIt hints
  // TODO: handle properly the case of removing whole DeclStmt
  // TODO: handle properly the case of converting single statement scopes to compound scopes
}

} // namespace tidy
} // namespace clang

