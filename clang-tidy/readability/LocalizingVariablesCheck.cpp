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

#include <iostream>

using namespace clang::ast_matchers;

namespace clang {
namespace tidy {

namespace {

AST_MATCHER_P(Expr, ignoringCasts, ast_matchers::internal::Matcher<Expr>,
              InnerMatcher) {
  return InnerMatcher.matches(*Node.IgnoreCasts(), Finder, Builder);
}

struct ScopeContext {
  const Stmt *Statement = nullptr;
  SourceLocation InsertLocationForDeclarationsBeforeScope;
};

using ScopeContextContainer = llvm::SmallVector<ScopeContext, 5>;
using ScopeIndexContainer = llvm::SmallVector<int, 5>;

struct DeclarationStatementContext {
  const DeclStmt *Statement = nullptr;
  int ScopeIndex = 0;
  int TotalDeclarationsCount = 0;
  int DeclarationsToBeRemovedCount = 0;
  SourceLocation NextStatementInsertLocation;
  bool IsFollowedByAnotherDeclarationStatement = false;
};

using DeclarationStatementContextContainer =
    llvm::SmallVector<DeclarationStatementContext, 10>;

enum class VariableInsertType { AsDeclaration, AsAssignment };

struct VariableUseContext {
  const Stmt *Statement = nullptr;
  ScopeIndexContainer ScopeStack;
  SourceLocation InsertLocation;
  VariableInsertType InsertType = VariableInsertType::AsDeclaration;
};

using ScopeUseContainer = llvm::SmallVector<VariableUseContext, 10>;

struct VariableDeclarationContext {
  const VarDecl *Declaration;
  int DeclarationStatementIndex;
  ScopeUseContainer Uses;
};

using VariableDeclarationContextContainer =
    llvm::SmallVector<VariableDeclarationContext, 10>;

class LocalizingVariablesHandler {
public:
  LocalizingVariablesHandler(LocalizingVariablesCheck &Check,
                             ASTContext &Context);

  void processStatement(const Stmt *Statement);

  void onEndOfFunction();

private:
  void processCompoundStatement(const CompoundStmt *CompoundStatement);
  void processIfStatement(const IfStmt *IfStatement);
  void processForStatement(const ForStmt *ForStatement);
  void processWhileStatement(const WhileStmt *WhileStatement);
  void processDoStatement(const DoStmt *DoStatement);
  void processForRangeStatement(const CXXForRangeStmt *ForRangeStatement);
  void processTryStatement(const CXXTryStmt *TryStatement);
  void processCatchStatement(const CXXCatchStmt *CatchStatement);
  void processGenericStatement(const Stmt *GenericStatement);

  void processNestedScope(SourceLocation InsertLocationBeforeScope,
                          const Stmt *NestedScope);
  void processCompoundNestedScope(SourceLocation InsertLocationBeforeScope,
                                  const CompoundStmt *CompoundNestedScope);
  void
  processSingleStatementNestedScope(SourceLocation InsertLocationBeforeScope,
                                    const Stmt *SingleStatementNestedScope);

  void pushScope(SourceLocation InsertLocationForDeclarationsBeforeScope,
                 const Stmt *ScopeStatement);
  void popScope();

  void processSingleStatement(SourceLocation InsertLocationBeforeStatement,
                              const Stmt *Statement,
                              bool AllowAssignmentInsertions = false);

  void processDeclarationStatement(const DeclStmt *DeclarationStatement);
  void processDeclarationUsesInStatement(
      SourceLocation InsertLocationBeforeStatement, const Stmt *Statement,
      bool AllowAssignmentInsertions);

  void localizeVariable(const VariableDeclarationContext &VariableDeclaration,
                        SourceLocation NewLocation,
                        VariableInsertType InsertType);

  void dump();

  LocalizingVariablesCheck &Check;
  ASTContext &Context;
  ScopeIndexContainer CurrentScopeStack;
  bool WasPreviousStatementADeclarationStatement = false;
  ScopeContextContainer Scopes;
  DeclarationStatementContextContainer DeclarationStatements;
  VariableDeclarationContextContainer VariableDeclarations;
};

} // anonymous namespace

////////////////////////////////

void LocalizingVariablesCheck::registerMatchers(MatchFinder *Finder) {
  Finder->addMatcher(
      functionDecl(unless(anyOf(isImplicit(), functionTemplateDecl(),
                                ast_matchers::isTemplateInstantiation())),
                   isDefinition())
          .bind("functionDecl"),
      this);
}

void LocalizingVariablesCheck::check(const MatchFinder::MatchResult &Result) {
  const auto *FunctionDeclaration =
      Result.Nodes.getNodeAs<FunctionDecl>("functionDecl");

  const auto *FunctionBody =
      llvm::dyn_cast_or_null<CompoundStmt>(FunctionDeclaration->getBody());
  if (FunctionBody == nullptr) {
    return;
  }

  std::cerr << "Function:" << std::endl;
  FunctionDeclaration->dump();

  LocalizingVariablesHandler Handler(*this, *Result.Context);
  Handler.processStatement(FunctionBody);

  Handler.onEndOfFunction();
}

////////////////////////////////

LocalizingVariablesHandler::LocalizingVariablesHandler(
    LocalizingVariablesCheck &Check, ASTContext &Context)
    : Check(Check), Context(Context) {}

void LocalizingVariablesHandler::processStatement(const Stmt *Statement) {
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

  // TODO: handle switch and case statements.

  default:
    processGenericStatement(Statement);
  }
}

void LocalizingVariablesHandler::processCompoundStatement(
    const CompoundStmt *CompoundStatement) {
  processCompoundNestedScope(CompoundStatement->getLocStart(),
                             CompoundStatement);
}

void LocalizingVariablesHandler::processIfStatement(const IfStmt *IfStatement) {
  processSingleStatement(IfStatement->getLocStart(), IfStatement->getCond());

  processNestedScope(IfStatement->getLocStart(), IfStatement->getThen());

  const Stmt *ElseStatement = IfStatement->getElse();
  while (ElseStatement != nullptr) {
    const auto *ChainedIfStatement = llvm::dyn_cast<IfStmt>(ElseStatement);

    if (ChainedIfStatement != nullptr) {
      // Chained "else if" scope.
      processSingleStatement(IfStatement->getLocStart(),
                             ChainedIfStatement->getCond());
      processNestedScope(IfStatement->getLocStart(),
                         ChainedIfStatement->getThen());
      ElseStatement = ChainedIfStatement->getElse();
    } else {
      // Regular else scope (final else statement).
      processNestedScope(IfStatement->getLocStart(), ElseStatement);
      ElseStatement = nullptr;
    }
  }
}

void LocalizingVariablesHandler::processForStatement(
    const ForStmt *ForStatement) {
  processSingleStatement(ForStatement->getLocStart(), ForStatement->getInit());
  processSingleStatement(ForStatement->getLocStart(), ForStatement->getCond());
  processSingleStatement(ForStatement->getLocStart(), ForStatement->getInc());
  processNestedScope(ForStatement->getLocStart(), ForStatement->getBody());
}

void LocalizingVariablesHandler::processWhileStatement(
    const WhileStmt *WhileStatement) {
  processSingleStatement(WhileStatement->getLocStart(),
                         WhileStatement->getCond());
  processNestedScope(WhileStatement->getLocStart(), WhileStatement->getBody());
}

void LocalizingVariablesHandler::processDoStatement(const DoStmt *DoStatement) {
  processSingleStatement(DoStatement->getLocStart(), DoStatement->getCond());
  processNestedScope(DoStatement->getLocStart(), DoStatement->getBody());
}

void LocalizingVariablesHandler::processForRangeStatement(
    const CXXForRangeStmt *ForRangeStatement) {
  processSingleStatement(ForRangeStatement->getLocStart(),
                         ForRangeStatement->getRangeStmt());
  processNestedScope(ForRangeStatement->getLocStart(),
                     ForRangeStatement->getBody());
}

void LocalizingVariablesHandler::processTryStatement(
    const CXXTryStmt *TryStatement) {
  processCompoundNestedScope(TryStatement->getLocStart(),
                             TryStatement->getTryBlock());
}

void LocalizingVariablesHandler::processCatchStatement(
    const CXXCatchStmt *CatchStatement) {
  processNestedScope(CatchStatement->getLocStart(),
                     CatchStatement->getHandlerBlock());
}

void LocalizingVariablesHandler::processGenericStatement(
    const Stmt *GenericStatement) {
  const bool AllowAssignmentInsertions = true;
  processSingleStatement(GenericStatement->getLocStart(), GenericStatement,
                         AllowAssignmentInsertions);
}

void LocalizingVariablesHandler::processNestedScope(
    SourceLocation InsertLocationBeforeScope, const Stmt *NestedScope) {
  const auto *CompoundNestedScope =
      llvm::dyn_cast_or_null<CompoundStmt>(NestedScope);
  if (CompoundNestedScope != nullptr) {
    processCompoundNestedScope(InsertLocationBeforeScope, CompoundNestedScope);
  } else {
    processSingleStatementNestedScope(InsertLocationBeforeScope, NestedScope);
  }
}

void LocalizingVariablesHandler::processCompoundNestedScope(
    SourceLocation InsertLocationBeforeScope,
    const CompoundStmt *CompoundNestedScope) {
  pushScope(InsertLocationBeforeScope, CompoundNestedScope);

  for (const auto *ScopeStatement : CompoundNestedScope->body()) {
    processStatement(ScopeStatement);
  }

  popScope();
}

void LocalizingVariablesHandler::processSingleStatementNestedScope(
    SourceLocation InsertLocationBeforeScope,
    const Stmt *SingleStatementNestedScope) {
  pushScope(InsertLocationBeforeScope, SingleStatementNestedScope);

  processSingleStatement(InsertLocationBeforeScope, SingleStatementNestedScope);

  popScope();
}

void LocalizingVariablesHandler::pushScope(
    SourceLocation InsertLocationForDeclarationsBeforeScope,
    const Stmt *ScopeStatement) {
  ScopeContext NewScopeContext;
  NewScopeContext.InsertLocationForDeclarationsBeforeScope =
      InsertLocationForDeclarationsBeforeScope;
  NewScopeContext.Statement = ScopeStatement;
  Scopes.push_back(std::move(NewScopeContext));

  int NewScopeIndex = Scopes.size() - 1;
  CurrentScopeStack.push_back(NewScopeIndex);
}

void LocalizingVariablesHandler::popScope() { CurrentScopeStack.pop_back(); }

void LocalizingVariablesHandler::processSingleStatement(
    SourceLocation InsertLocationBeforeStatement, const Stmt *Statement,
    bool AllowAssignmentInsertions) {
  const auto *DeclarationStatement = llvm::dyn_cast<DeclStmt>(Statement);

  if (WasPreviousStatementADeclarationStatement) {
    DeclarationStatements.back().NextStatementInsertLocation =
        InsertLocationBeforeStatement;
    DeclarationStatements.back().IsFollowedByAnotherDeclarationStatement =
        DeclarationStatement != nullptr;
  }

  if (DeclarationStatement != nullptr) {
    processDeclarationStatement(DeclarationStatement);
  }

  processDeclarationUsesInStatement(InsertLocationBeforeStatement, Statement,
                                    AllowAssignmentInsertions);

  WasPreviousStatementADeclarationStatement = DeclarationStatement != nullptr;
}

void LocalizingVariablesHandler::processDeclarationStatement(
    const DeclStmt *DeclarationStatement) {
  DeclarationStatementContext NewDeclarationStatementContext;
  NewDeclarationStatementContext.Statement = DeclarationStatement;
  int CurrentScopeIndex = Scopes.size() - 1;
  NewDeclarationStatementContext.ScopeIndex = CurrentScopeIndex;
  NewDeclarationStatementContext.TotalDeclarationsCount = 0;

  int NewDeclarationStatementContextIndex = DeclarationStatements.size();

  for (const Decl *Declaration : DeclarationStatement->decls()) {
    NewDeclarationStatementContext.TotalDeclarationsCount++;

    const auto *VariableDeclaration = llvm::dyn_cast<VarDecl>(Declaration);
    if (VariableDeclaration == nullptr) {
      continue;
    }

    if (!VariableDeclaration->getType().isPODType(Context)) {
      continue;
    }

    if (VariableDeclaration->hasInit()) {
      continue;
    }

    VariableDeclarationContext NewVariableDeclarationContext;
    NewVariableDeclarationContext.Declaration = VariableDeclaration;
    NewVariableDeclarationContext.DeclarationStatementIndex =
        NewDeclarationStatementContextIndex;
    VariableDeclarations.push_back(std::move(NewVariableDeclarationContext));
  }

  DeclarationStatements.push_back(std::move(NewDeclarationStatementContext));
}

void LocalizingVariablesHandler::processDeclarationUsesInStatement(
    SourceLocation InsertLocationBeforeStatement, const Stmt *Statement,
    bool AllowAssignmentInsertions) {

  const VarDecl *SimplyAssignedVariable = nullptr;
  if (AllowAssignmentInsertions) {
    auto SimpleAssignmentMatcher = binaryOperator(
        hasOperatorName("="), hasLHS(ignoringCasts(declRefExpr(
                                  to(varDecl().bind("assignedVarDecl"))))),
        unless(hasRHS(ignoringCasts(binaryOperator(hasOperatorName("="))))));
    auto AssignmentMatchResult =
        match(SimpleAssignmentMatcher, *Statement, Context);
    if (!AssignmentMatchResult.empty()) {
      SimplyAssignedVariable =
          AssignmentMatchResult[0].getNodeAs<VarDecl>("assignedVarDecl");
    }
  }

  auto VariableUsesMatcher =
      findAll(declRefExpr(to(varDecl().bind("varDecl"))).bind("declRefExpr"));
  auto MatchResults = match(VariableUsesMatcher, *Statement, Context);
  for (const auto &Result : MatchResults) {
    const auto *ReferenceExpression =
        Result.getNodeAs<DeclRefExpr>("declRefExpr");
    const auto *VariableDeclaration = Result.getNodeAs<VarDecl>("varDecl");

    auto VariableDeclarationIt = std::find_if(
        VariableDeclarations.begin(), VariableDeclarations.end(),
        [VariableDeclaration](const VariableDeclarationContext &Context) {
          return Context.Declaration == VariableDeclaration;
        });
    if (VariableDeclarationIt == VariableDeclarations.end()) {
      continue;
    }

    VariableUseContext NewVariableUseContext;
    NewVariableUseContext.Statement = ReferenceExpression;
    NewVariableUseContext.ScopeStack = CurrentScopeStack;
    NewVariableUseContext.InsertLocation = InsertLocationBeforeStatement;
    if (VariableDeclaration == SimplyAssignedVariable) {
      NewVariableUseContext.InsertType = VariableInsertType::AsAssignment;
    } else {
      NewVariableUseContext.InsertType = VariableInsertType::AsDeclaration;
    }

    VariableDeclarationIt->Uses.push_back(std::move(NewVariableUseContext));
  }
}

void LocalizingVariablesHandler::dump() {
  std::cerr << "ScopeStack:";
  for (int ScopeIndex : CurrentScopeStack) {
    std::cerr << " " << ScopeIndex;
  }
  std::cerr << std::endl;
  std::cerr << "WasPreviousStatementADeclarationStatement: "
            << WasPreviousStatementADeclarationStatement << std::endl;

  std::cerr << "Scopes:" << std::endl;
  int Index = 0;
  for (const ScopeContext &Scope : Scopes) {
    std::cerr << " Scope #" << (Index++) << std::endl;
    std::cerr << "  Statement: " << static_cast<const void *>(Scope.Statement)
              << std::endl;
    std::cerr << "  InsertLocationForDeclarationsBeforeScope: ";
    Scope.InsertLocationForDeclarationsBeforeScope.dump(
        Context.getSourceManager());
    std::cerr << std::endl;
  }

  std::cerr << "DeclarationStatements:" << std::endl;
  Index = 0;
  for (const DeclarationStatementContext &DeclarationStatement :
       DeclarationStatements) {
    std::cerr << " DeclarationStatement #" << (Index++) << std::endl;
    std::cerr << "  Statement: "
              << static_cast<const void *>(DeclarationStatement.Statement)
              << std::endl;
    std::cerr << "  ScopeIndex: " << DeclarationStatement.ScopeIndex
              << std::endl;
    std::cerr << "  TotalDeclarationsCount: "
              << DeclarationStatement.TotalDeclarationsCount << std::endl;
    std::cerr << "  DeclarationsToBeRemovedCount: "
              << DeclarationStatement.DeclarationsToBeRemovedCount << std::endl;
    std::cerr << "  NextStatementInsertLocation: ";
    DeclarationStatement.NextStatementInsertLocation.dump(
        Context.getSourceManager());
    std::cerr << std::endl;
    std::cerr << "  IsFollowedByAnotherDeclarationStatement: "
              << DeclarationStatement.IsFollowedByAnotherDeclarationStatement
              << std::endl;
  }

  std::cerr << "VariableDeclarations:" << std::endl;
  Index = 0;
  for (const VariableDeclarationContext &VariableDeclaration :
       VariableDeclarations) {
    std::cerr << " VariableDeclaration #" << (Index++) << std::endl;
    std::cerr << "  Declaration: "
              << static_cast<const void *>(VariableDeclaration.Declaration)
              << std::endl;
    std::cerr << "  DeclarationStatementIndex: "
              << VariableDeclaration.DeclarationStatementIndex << std::endl;
    std::cerr << "  Uses:" << std::endl;

    int SubIndex = 0;
    for (const VariableUseContext &VariableUse : VariableDeclaration.Uses) {
      std::cerr << "   VariableUse #" << (SubIndex++) << std::endl;
      std::cerr << "    Statement: "
                << static_cast<const void *>(VariableUse.Statement)
                << std::endl;
      std::cerr << "    ScopeStack:";
      for (int ScopeIndex : VariableUse.ScopeStack)
        std::cerr << " " << ScopeIndex;
      std::cerr << std::endl;
      std::cerr << "    InsertLocation: ";
      VariableUse.InsertLocation.dump(Context.getSourceManager());
      std::cerr << std::endl;
      std::cerr << "    InsertType: " << ((VariableUse.InsertType ==
                                           VariableInsertType::AsDeclaration)
                                              ? "AsDeclaration"
                                              : "AsAssignment")
                << std::endl;
    }
  }
}

void LocalizingVariablesHandler::onEndOfFunction() {
  dump();

  for (const VariableDeclarationContext &VariableDeclaration :
       VariableDeclarations) {
    DeclarationStatementContext &DeclarationStatement =
        DeclarationStatements[VariableDeclaration.DeclarationStatementIndex];

    llvm::Optional<ScopeIndexContainer> MaximumCommonScopeStack;

    for (const VariableUseContext &Use : VariableDeclaration.Uses) {
      if (!MaximumCommonScopeStack) {
        MaximumCommonScopeStack = Use.ScopeStack;
      } else {
        llvm::Optional<size_t> CutoffIndex;
        for (size_t Index = 0; Index < MaximumCommonScopeStack->size() &&
                               Index < Use.ScopeStack.size();
             ++Index) {
          if ((*MaximumCommonScopeStack)[Index] != Use.ScopeStack[Index]) {
            CutoffIndex = Index;
            break;
          }
        }

        if (CutoffIndex) {
          auto EraseIt = MaximumCommonScopeStack->begin();
          std::advance(EraseIt, *CutoffIndex);
          MaximumCommonScopeStack->erase(EraseIt,
                                         MaximumCommonScopeStack->end());
        }
      }
    }

    // There are no recorded uses of this variable - nothing to do.
    if (!MaximumCommonScopeStack) {
      continue;
    }

    assert(MaximumCommonScopeStack->size() >= 1 &&
           "Variable uses must share at least one common scope");

    DeclarationStatement.DeclarationsToBeRemovedCount++;

    // First use of variable is exactly in the maximum common scope stack we
    // found. We can safely move the declaration to a location before this use.
    const auto &FirstUseScopeStack =
        VariableDeclaration.Uses.front().ScopeStack;
    if (FirstUseScopeStack == *MaximumCommonScopeStack) {
      const VariableUseContext &FirstUse = VariableDeclaration.Uses.front();
      localizeVariable(VariableDeclaration, FirstUse.InsertLocation,
                       FirstUse.InsertType);
      continue;
    }

    // First use is in scope below the maximum common scope stack we found. This
    // means that the variable must remain declared in scope above, but we can
    // move it to a location just before the beginning of this scope.
    const ScopeContext &MaximumCommonScope =
        Scopes[MaximumCommonScopeStack->back()];
    localizeVariable(
        VariableDeclaration,
        MaximumCommonScope.InsertLocationForDeclarationsBeforeScope,
        VariableInsertType::AsDeclaration);
  }
}

// TODO: handle properly the case of converting single statement scopes to
// compound scopes

void LocalizingVariablesHandler::localizeVariable(
    const VariableDeclarationContext &VariableDeclaration,
    SourceLocation NewLocation, VariableInsertType InsertType) {

  if (InsertType == VariableInsertType::AsDeclaration) {
    // First, make sure that we are not doing something stupid - moving the
    // variable declaration to a location just after the declaration block,
    // e.g.:
    //  int a;
    //  int b;
    //  use(a);
    //  ^ move "a" declaration here
    for (size_t DeclarationStatementIndex =
             VariableDeclaration.DeclarationStatementIndex;
         DeclarationStatementIndex < DeclarationStatements.size();
         ++DeclarationStatementIndex) {
      // Are we inserting just after this declaration?
      if (DeclarationStatements[DeclarationStatementIndex]
              .NextStatementInsertLocation == NewLocation) {
        return;
      }
      // We can also have consecutive declaration statements.
      if (!DeclarationStatements[DeclarationStatementIndex]
               .IsFollowedByAnotherDeclarationStatement) {
        break;
      }
    }
  }

  DeclarationStatementContext &DeclarationStatement =
      DeclarationStatements[VariableDeclaration.DeclarationStatementIndex];

  auto Diagnostic = Check.diag(VariableDeclaration.Declaration->getLocStart(),
                               "declaration of variable '%0' can be localized "
                               "by moving it closer to its uses")
                    << VariableDeclaration.Declaration->getName();

  if (DeclarationStatement.DeclarationsToBeRemovedCount ==
      DeclarationStatement.TotalDeclarationsCount) {
    Diagnostic.AddFixItHint(
        FixItHint::CreateRemoval(CharSourceRange::getTokenRange(
            DeclarationStatement.Statement->getLocStart(),
            DeclarationStatement.Statement->getLocEnd())));
  } else {
    Diagnostic.AddFixItHint(
        FixItHint::CreateRemoval(CharSourceRange::getTokenRange(
            VariableDeclaration.Declaration->getLocStart(),
            VariableDeclaration.Declaration->getLocEnd())));
  }

  std::string DeclarationCode =
      VariableDeclaration.Declaration->getType().getAsString();
  DeclarationCode += " ";
  if (InsertType == VariableInsertType::AsDeclaration) {
    DeclarationCode += VariableDeclaration.Declaration->getNameAsString();
    DeclarationCode += "; ";
  }
  // There is no else as in case of assignment, variable name is already in the
  // following expression.

  Diagnostic.AddFixItHint(
      FixItHint::CreateInsertion(NewLocation, DeclarationCode));
}

} // namespace tidy
} // namespace clang
