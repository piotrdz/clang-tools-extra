//===--- LocalizingVariablesCheck.h - clang-tidy-----------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_READABILITY_LOCALIZING_VARIABLES_H
#define LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_READABILITY_LOCALIZING_VARIABLES_H

#include "../ClangTidy.h"

namespace clang {
namespace tidy {

/// \brief Checks for variables which are declared and first used far apart.
///
/// For the user-facing documentation see:
/// http://clang.llvm.org/extra/clang-tidy/checks/readability-localizing-variables.html
class LocalizingVariablesCheck : public ClangTidyCheck {
public:
  LocalizingVariablesCheck(StringRef Name, ClangTidyContext *Context)
      : ClangTidyCheck(Name, Context) {}
  void registerMatchers(ast_matchers::MatchFinder *Finder) override;
  void check(const ast_matchers::MatchFinder::MatchResult &Result) override;
};

} // namespace tidy
} // namespace clang

#endif // LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_READABILITY_LOCALIZING_VARIABLES_H
