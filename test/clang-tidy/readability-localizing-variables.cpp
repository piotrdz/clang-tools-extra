// RUN: %check_clang_tidy %s readability-localizing-variables %t

int returnValue();
void useByReference(int& a);
int useByReferenceAndReturn(int& a);
void call();

struct Range
{
  int* begin();
  int* end();
};
Range returnRange();
Range returnRange(int a);

/////// Variable declared and later used in assignment

void variableDeclarationAndAssignmentTogether() {
  int a = 1;
}

void variableDeclarationAndAssignmentInNextLine() {
  // CHECK-MESSAGES: :[[@LINE+3]]:3: warning: declaration of variable 'a' can be localized by moving it closer to its uses [readability-localizing-variables]
  // CHECK-FIXES:      {{^}}  {{$}}
  // CHECK-FIXES-NEXT: {{^}}  int a = 1;{{$}}
  int a;
  a = 1;
}

void variableDeclarationAndAssignmentSeparatedByOtherStatement() {
  // CHECK-MESSAGES: :[[@LINE+4]]:3: warning: declaration of variable 'a'
  // CHECK-FIXES:      {{^}}  {{$}}
  // CHECK-FIXES-NEXT: {{^}}  call();{{$}}
  // CHECK-FIXES-NEXT: {{^}}  int a = 1;{{$}}
  int a;
  call();
  a = 1;
}


/////// Variable declared and later used in expression

void variableDeclarationAndUseInExpressionWithoutAnythingBetween() {
  int a;
  useByReference(a);
}

void variableDeclarationAndUseInExpressionSeparatedByOtherDeclarationLine() {
  int a;
  int b;
  useByReference(a);
}

void variableDeclarationAndUseInExpressionSeparatedByOtherStatement() {
  // CHECK-MESSAGES: :[[@LINE+4]]:3: warning: declaration of variable 'a'
  // CHECK-FIXES:      {{^}}  {{$}}
  // CHECK-FIXES-NEXT: {{^}}  call();{{$}}
  // CHECK-FIXES-NEXT: {{^}}  int a; useByReference(a);{{$}}
  int a;
  call();
  useByReference(a);
}


/////// Special case - variable assignment and use in expression in same line

void variableDeclarationAndAssignmentAndUseInExpressionInSameLine() {
  // CHECK-MESSAGES: :[[@LINE+5]]:3: warning: declaration of variable 'a'
  // CHECK-MESSAGES: :[[@LINE+4]]:3: warning: declaration of variable 'b'
  // CHECK-FIXES:      {{^}}  {{$}}
  // CHECK-FIXES-NEXT: {{^}}  call();{{$}}
  // CHECK-FIXES-NEXT: {{^}}  int b; int a = useByReferenceAndReturn(b);{{$}}
  int a, b;
  call();
  a = useByReferenceAndReturn(b);
}


/////// Chained assignments treated as expressions

void variableDeclarationAndChainedAssignmentWithoutAnythingBetween() {
  int a, b;
  a = b = returnValue();
}

void variableDeclarationAndChainedAssignmentSeparatedByAnotherDeclarationBlock() {
  int a, b;
  int c;

  a = b = returnValue();
}

void variableDeclarationAndChainedAssignmentSeparatedByOtherExpression() {
  // CHECK-MESSAGES: :[[@LINE+5]]:3: warning: declaration of variable 'a'
  // CHECK-MESSAGES: :[[@LINE+4]]:3: warning: declaration of variable 'b'
  // CHECK-FIXES:      {{^}}  {{$}}
  // CHECK-FIXES-NEXT: {{^}}  call();{{$}}
  // CHECK-FIXES-NEXT: {{^}}  int a; int b; a = b = returnValue();{{$}}
  int a, b;
  call();
  a = b = returnValue();
}


/////// Nested scopes

void variableUsedOnlyInOneNestedScope() {
  // CHECK-MESSAGES: :[[@LINE+4]]:3: warning: declaration of variable 'a'
  // CHECK-FIXES:      {{^}}  {{$}}
  // CHECK-FIXES-NEXT: {{^}}  {{{$}}
  // CHECK-FIXES-NEXT: {{^}}    int a = returnValue();{{$}}
  int a;
  {
    a = returnValue();
    a = a + 2;
  }
}

void variableUsedInNestedScopesCannotBeMovedBecauseItIsAlreadyWhereNeeded() {
  int a;
  {
    a = returnValue();
  }
  {
    a = a + 2;
  }
}

void variableUsedInNestedScopesCannotBeMovedBecauseItWouldBeInSameDeclarationBlock() {
  int a;
  int b;
  {
    a = returnValue();
  }
  {
    a = a + 2;
  }
}

void variableUsedInNestedScopesCanBeMovedToANestedScope() {
  // CHECK-MESSAGES: :[[@LINE+4]]:3: warning: declaration of variable 'a'
  // CHECK-FIXES:      {{^}}  {{$}}
  // CHECK-FIXES-NEXT: {{^}}  {{{$}}
  // CHECK-FIXES-NEXT: {{^}}    int a; {{{$}}
  int a;
  {
    {
      a = returnValue();
    }
    {
      a = a + 2;
    }
  }
}

/////// Nested scopes in statements

void variableUsedInIfCondition() {
  // CHECK-MESSAGES: :[[@LINE+4]]:3: warning: declaration of variable 'a'
  // CHECK-FIXES:      {{^}}  {{$}}
  // CHECK-FIXES-NEXT: {{^}}  {{{$}}
  // CHECK-FIXES-NEXT: {{^}}    int a; if (a) {{{$}}
  int a;
  {
    if (a) {
    }
  }
}

void variableUsedInIfBody() {
  // CHECK-MESSAGES: :[[@LINE+5]]:3: warning: declaration of variable 'a'
  // CHECK-FIXES:      {{^}}  {{$}}
  // CHECK-FIXES-NEXT: {{^}}  {{{$}}
  // CHECK-FIXES-NEXT: {{^}}    if (true) {{{$}}
  // CHECK-FIXES-NEXT: {{^}}      int a = returnValue();{{$}}
  int a;
  {
    if (true) {
      a = returnValue();
    }
  }
}

void variableUsedInElseIfCondition() {
  // CHECK-MESSAGES: :[[@LINE+4]]:3: warning: declaration of variable 'a'
  // CHECK-FIXES:      {{^}}  {{$}}
  // CHECK-FIXES-NEXT: {{^}}  {{{$}}
  // CHECK-FIXES-NEXT: {{^}}    int a; if (true) {{{$}}
  int a;
  {
    if (true) {
    } else if (a) {
    }
  }
}

void variableUsedInElseIfBody() {
  // CHECK-MESSAGES: :[[@LINE+6]]:3: warning: declaration of variable 'a'
  // CHECK-FIXES:      {{^}}  {{$}}
  // CHECK-FIXES-NEXT: {{^}}  {{{$}}
  // CHECK-FIXES-NEXT: {{^}}    if (true) {{{$}}
  // CHECK-FIXES-NEXT: {{^}}    } else if (true) {{{$}}
  // CHECK-FIXES-NEXT: {{^}}      int a = returnValue();{{$}}
  int a;
  {
    if (true) {
    } else if (true) {
      a = returnValue();
    }
  }
}

void variableUsedInElseBody() {
  // CHECK-MESSAGES: :[[@LINE+6]]:3: warning: declaration of variable 'a'
  // CHECK-FIXES:      {{^}}  {{$}}
  // CHECK-FIXES-NEXT: {{^}}  {{{$}}
  // CHECK-FIXES-NEXT: {{^}}    if (true) {{{$}}
  // CHECK-FIXES-NEXT: {{^}}    } else {{{$}}
  // CHECK-FIXES-NEXT: {{^}}      int a = returnValue();{{$}}
  int a;
  {
    if (true) {
    } else {
      a = returnValue();
    }
  }
}

void variableUsedInIfBodyAndElseBody() {
  // CHECK-MESSAGES: :[[@LINE+4]]:3: warning: declaration of variable 'a'
  // CHECK-FIXES:      {{^}}  {{$}}
  // CHECK-FIXES-NEXT: {{^}}  {{{$}}
  // CHECK-FIXES-NEXT: {{^}}    int a; if (true) {{{$}}
  int a;
  {
    if (true) {
      a = returnValue();
    } else {
      a = returnValue();
    }
  }
}

void variableUsedInForLoopCondition() {
  // CHECK-MESSAGES: :[[@LINE+4]]:3: warning: declaration of variable 'a'
  // CHECK-FIXES:      {{^}}  {{$}}
  // CHECK-FIXES-NEXT: {{^}}  {{{$}}
  // CHECK-FIXES-NEXT: {{^}}    int a; for (a = 0; a < 10; ++a) {{{$}}
  int a;
  {
    for (a = 0; a < 10; ++a) {
    }
  }
}

void variableUsedInForLoopBody() {
  // CHECK-MESSAGES: :[[@LINE+5]]:3: warning: declaration of variable 'a'
  // CHECK-FIXES:      {{^}}  {{$}}
  // CHECK-FIXES:      {{^}}  {{{$}}
  // CHECK-FIXES-NEXT: {{^}}    for (int i = 0; i < 10; ++i) {{{$}}
  // CHECK-FIXES-NEXT: {{^}}      int a = returnValue();{{$}}
  int a;
  {
    for (int i = 0; i < 10; ++i) {
      a = returnValue();
    }
  }
}

void variableUsedInRangeForLoopCondition() {
  // CHECK-MESSAGES: :[[@LINE+4]]:3: warning: declaration of variable 'a'
  // CHECK-FIXES:      {{^}}  {{$}}
  // CHECK-FIXES-NEXT: {{^}}  {{{$}}
  // CHECK-FIXES-NEXT: {{^}}    int a; for (int v : returnRange(a)) {{{$}}
  int a;
  {
    for (int v : returnRange(a)) {
    }
  }
}

void variableUsedInRangeForLoopBody() {
  // CHECK-MESSAGES: :[[@LINE+5]]:3: warning: declaration of variable 'a'
  // CHECK-FIXES:      {{^}}  {{$}}
  // CHECK-FIXES-NEXT: {{^}}  {{{$}}
  // CHECK-FIXES-NEXT: {{^}}    for (int v : returnRange()) {{{$}}
  // CHECK-FIXES-NEXT: {{^}}      int a = returnValue();{{$}}
  int a;
  {
    for (int v : returnRange()) {
      a = returnValue();
    }
  }
}

void variableUsedInWhileLoopCondition() {
  // CHECK-MESSAGES: :[[@LINE+4]]:3: warning: declaration of variable 'a'
  // CHECK-FIXES:      {{^}}  {{$}}
  // CHECK-FIXES-NEXT: {{^}}  {{{$}}
  // CHECK-FIXES-NEXT: {{^}}    int a; while (useByReferenceAndReturn(a)) {{{$}}
  int a;
  {
    while (useByReferenceAndReturn(a)) {
    }
  }
}

void variableUsedInWhileLoopBody() {
  // CHECK-MESSAGES: :[[@LINE+5]]:3: warning: declaration of variable 'a'
  // CHECK-FIXES:      {{^}}  {{$}}
  // CHECK-FIXES-NEXT: {{^}}  {{{$}}
  // CHECK-FIXES-NEXT: {{^}}    while (true) {{{$}}
  // CHECK-FIXES-NEXT: {{^}}      int a = returnValue();{{$}}
  int a;
  {
    while (true) {
      a = returnValue();
    }
  }
}

void variableUsedInDoWhileLoopCondition() {
  // CHECK-MESSAGES: :[[@LINE+5]]:3: warning: declaration of variable 'a'
  // CHECK-FIXES:      {{^}}  {{$}}
  // CHECK-FIXES-NEXT: {{^}}  {{{$}}
  // CHECK-FIXES-NEXT: {{^}}    int a; do {{{$}}
  // CHECK-FIXES-NEXT: {{^}}    } while (useByReferenceAndReturn(a));{{$}}
  int a;
  {
    do {
    } while (useByReferenceAndReturn(a));
  }
}

void variableUsedInDoWhileLoopBody() {
  // CHECK-MESSAGES: :[[@LINE+5]]:3: warning: declaration of variable 'a'
  // CHECK-FIXES:      {{^}}  {{$}}
  // CHECK-FIXES:      {{^}}  {{{$}}
  // CHECK-FIXES-NEXT: {{^}}    do {{{$}}
  // CHECK-FIXES-NEXT: {{^}}      int a = returnValue();{{$}}
  int a;
  {
    do {
      a = returnValue();
    } while (true);
  }
}

void variableUsedInTryBlock() {
  // CHECK-MESSAGES: :[[@LINE+5]]:3: warning: declaration of variable 'a'
  // CHECK-FIXES:      {{^}}  {{$}}
  // CHECK-FIXES:      {{^}}  {{{$}}
  // CHECK-FIXES-NEXT: {{^}}    try {{{$}}
  // CHECK-FIXES-NEXT: {{^}}      int a = returnValue();{{$}}
  int a;
  {
    try {
      a = returnValue();
    } catch (int& b) {
    }
  }
}

void variableUsedInCatchBlock() {
  // CHECK-MESSAGES: :[[@LINE+7]]:3: warning: declaration of variable 'a'
  // CHECK-FIXES:      {{^}}  {{$}}
  // CHECK-FIXES:      {{^}}  {{{$}}
  // CHECK-FIXES-NEXT: {{^}}    try {{{$}}
  // CHECK-FIXES-NEXT: {{^}}      call();{{$}}
  // CHECK-FIXES-NEXT: {{^}}    } catch (int& b) {{{$}}
  // CHECK-FIXES-NEXT: {{^}}      int a = returnValue();{{$}}
  int a;
  {
    try {
      call();
    } catch (int& b) {
      a = returnValue();
    }
  }
}

void variableUsedInTryAndCatchBlocks() {
  // CHECK-MESSAGES: :[[@LINE+4]]:3: warning: declaration of variable 'a'
  // CHECK-FIXES:      {{^}}  {{$}}
  // CHECK-FIXES-NEXT: {{^}}  {{{$}}
  // CHECK-FIXES-NEXT: {{^}}    int a; try {{{$}}
  int a;
  {
    try {
      a = returnValue();
    } catch (int& b) {
      a = returnValue();
    }
  }
}

/////// Nested single-statement scopes

void variableUsedInSingleStatementIf() {
  // CHECK-MESSAGES: :[[@LINE+5]]:3: warning: declaration of variable 'a'
  // CHECK-FIXES:      {{^}}  {{$}}
  // CHECK-FIXES-NEXT: {{^}}  {{{$}}
  // CHECK-FIXES-NEXT: {{^}}    if (true){{$}}
  // CHECK-FIXES-NEXT: {{^}}      { int a = returnValue(); }{{$}}
  int a;
  {
    if (true)
      a = returnValue();
  }
}

// TODO: check switch, case statements

// TODO: check for correct removal of selected declarations in multiple declarations:
// int a, b, c;  // e.g. remove only "b"

// TODO: check for correct handling of pointers and arrays in multiple declarations:
// int a, *b, c[10];
