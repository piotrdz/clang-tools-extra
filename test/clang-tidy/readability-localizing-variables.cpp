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


/////// Ignore chained assignments

void variablesDeclarationAndChainedAssignmentWithoutAnythingBetween() {
  int a, b;
  a = b = returnValue();
}

void variablesDeclarationAndChainedAssignmentSeparatedByAnotherDeclarationBlock() {
  int a, b;
  int c;
  a = b = returnValue();
}

void variablesDeclarationAndChainedAssignmentSeparatedByOtherExpression() {
  // CHECK-MESSAGES: :[[@LINE+5]]:3: warning: declaration of variable 'a'
  // CHECK-MESSAGES: :[[@LINE+4]]:3: warning: declaration of variable 'b'
  // CHECK-FIXES:      {{^}}  {{$}}
  // CHECK-FIXES-NEXT: {{^}}  call();{{$}}
  // CHECK-FIXES-NEXT: {{^}}  int a; int b; a = b = returnValue();{{$}}
  int a, b;
  call();
  a = b = returnValue();
}

void variablesDeclaredSeparatelyAndChainedAssignmentSeparatedByOtherExpression() {
  // CHECK-MESSAGES: :[[@LINE+5]]:3: warning: declaration of variable 'a'
  // CHECK-FIXES:      {{^}}  {{$}}
  // CHECK-FIXES-NEXT: {{^}}  call();{{$}}
  // CHECK-FIXES-NEXT: {{^}}  int b;{{$}}
  // CHECK-FIXES-NEXT: {{^}}  int a; a = b = returnValue();{{$}}
  int a;
  call();
  int b;
  a = b = returnValue();
}

void variablesDeclaredSeparatelyAndChainedAssignmentInReverseOrderSeparatedByOtherExpression() {
  // CHECK-MESSAGES: :[[@LINE+5]]:3: warning: declaration of variable 'a'
  // CHECK-FIXES:      {{^}}  {{$}}
  // CHECK-FIXES-NEXT: {{^}}  call();{{$}}
  // CHECK-FIXES-NEXT: {{^}}  int b;{{$}}
  // CHECK-FIXES-NEXT: {{^}}  int a; b = a = returnValue();{{$}}
  int a;
  call();
  int b;
  b = a = returnValue();
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
  // CHECK-FIXES-NEXT: {{^}}    int a; for (useByReference(a); a < 10; ++a) {{{$}}
  int a;
  {
    for (useByReference(a); a < 10; ++a) {
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

////// Switch statement

void variableUsedInSwitchCondition() {
  // CHECK-MESSAGES: :[[@LINE+4]]:3: warning: declaration of variable 'a'
  // CHECK-FIXES:      {{^}}  {{$}}
  // CHECK-FIXES-NEXT: {{^}}  {{{$}}
  // CHECK-FIXES-NEXT: {{^}}    int a; switch (useByReferenceAndReturn(a)) {{{$}}
  int a;
  {
    switch (useByReferenceAndReturn(a)) {
    }
  }
}

void variableUsedInBracedCaseBody() {
  // CHECK-MESSAGES: :[[@LINE+6]]:3: warning: declaration of variable 'a'
  // CHECK-FIXES:      {{^}}  {{$}}
  // CHECK-FIXES-NEXT: {{^}}  {{{$}}
  // CHECK-FIXES-NEXT: {{^}}    switch (1) {{{$}}
  // CHECK-FIXES-NEXT: {{^}}      case 1: {{{$}}
  // CHECK-FIXES-NEXT: {{^}}        int a = returnValue();{{$}}
  int a;
  {
    switch (1) {
      case 1: {
        a = returnValue();
        break;
      }
      case 2: {
        call();
        break;
      }
    }
  }
}

void variableUsedInUnbracedCaseBody() {
  // CHECK-MESSAGES: :[[@LINE+7]]:3: warning: declaration of variable 'a'
  // CHECK-FIXES:      {{^}}  {{$}}
  // CHECK-FIXES-NEXT: {{^}}  {{{$}}
  // CHECK-FIXES-NEXT: {{^}}    switch (1) {{{$}}
  // CHECK-FIXES-NEXT: {{^}}      case 1:{{$}}
  // CHECK-FIXES-NEXT: {{^}}        { int a = returnValue();{{$}}
  // CHECK-FIXES-NEXT: {{^}}        break; }{{$}}
  int a;
  {
    switch (1) {
      case 1:
        a = returnValue();
        break;
      case 2:
        call();
        break;
    }
  }
}

void variableUsedInUnbracedFallthroughCaseBody() {
  // CHECK-MESSAGES: :[[@LINE+8]]:3: warning: declaration of variable 'a'
  // CHECK-FIXES:      {{^}}  {{$}}
  // CHECK-FIXES-NEXT: {{^}}  {{{$}}
  // CHECK-FIXES-NEXT: {{^}}    switch (1) {{{$}}
  // CHECK-FIXES-NEXT: {{^}}      case 1:{{$}}
  // CHECK-FIXES-NEXT: {{^}}      case 2:{{$}}
  // CHECK-FIXES-NEXT: {{^}}        { int a = returnValue();{{$}}
  // CHECK-FIXES-NEXT: {{^}}        break; }{{$}}
  int a;
  {
    switch (1) {
      case 1:
      case 2:
        a = returnValue();
        break;
      case 3:
        call();
        break;
    }
  }
}

void variableUsedInMoreThanOneBracedCaseBody() {
  // CHECK-MESSAGES: :[[@LINE+4]]:3: warning: declaration of variable 'a'
  // CHECK-FIXES:      {{^}}  {{$}}
  // CHECK-FIXES-NEXT: {{^}}  {{{$}}
  // CHECK-FIXES-NEXT: {{^}}    int a; switch (1) {{{$}}
  int a;
  {
    switch (1) {
      case 1: {
        a = returnValue();
        break;
      }
      case 2: {
        a = returnValue();
        break;
      }
    }
  }
}

void variableUsedInMoreThanOneUnbracedCaseBody() {
  // CHECK-MESSAGES: :[[@LINE+4]]:3: warning: declaration of variable 'a'
  // CHECK-FIXES:      {{^}}  {{$}}
  // CHECK-FIXES-NEXT: {{^}}  {{{$}}
  // CHECK-FIXES-NEXT: {{^}}    int a; switch (1) {{{$}}
  int a;
  {
    switch (1) {
      case 1:
        a = returnValue();
        break;
      case 2:
        a = returnValue();
        break;
    }
  }
}

///// Variable used in assignment in statement conditions

void variableUsedInIfConditionWithAssignmentPossible() {
  // CHECK-MESSAGES: :[[@LINE+4]]:3: warning: declaration of variable 'a'
  // CHECK-FIXES:      {{^}}  {{$}}
  // CHECK-FIXES-NEXT: {{^}}  {{{$}}
  // CHECK-FIXES-NEXT: {{^}}    if (int a = returnValue()) {{{$}}
  int a;
  {
    if (a = returnValue()) {
    }
  }
}

void variableUsedInElseIfConditionWithAssignmentPossible() {
  // CHECK-MESSAGES: :[[@LINE+5]]:3: warning: declaration of variable 'a'
  // CHECK-FIXES:      {{^}}  {{$}}
  // CHECK-FIXES-NEXT: {{^}}  {{{$}}
  // CHECK-FIXES-NEXT: {{^}}    if (true) {{{$}}
  // CHECK-FIXES-NEXT: {{^}}    } else if (int a = returnValue()) {{{$}}
  int a;
  {
    if (true) {
    } else if (a = returnValue()) {
    }
  }
}

void variableUsedInForLoopConditionWithAssignmentPossible() {
  // CHECK-MESSAGES: :[[@LINE+4]]:3: warning: declaration of variable 'a'
  // CHECK-FIXES:      {{^}}  {{$}}
  // CHECK-FIXES-NEXT: {{^}}  {{{$}}
  // CHECK-FIXES-NEXT: {{^}}    for (int a = 0; a < 10; ++a) {{{$}}
  int a;
  {
    for (a = 0; a < 10; ++a) {
    }
  }
}

void variableUsedInForLoopConditionAndBodyWithAssignmentPossible() {
  // CHECK-MESSAGES: :[[@LINE+4]]:3: warning: declaration of variable 'a'
  // CHECK-FIXES:      {{^}}  {{$}}
  // CHECK-FIXES-NEXT: {{^}}  {{{$}}
  // CHECK-FIXES-NEXT: {{^}}    for (int a = 0; a < 10; ++a) {{{$}}
  int a;
  {
    for (a = 0; a < 10; ++a) {
      a = returnValue();
    }
  }
}

void variableUsedInWhileLoopConditionWithAssignmentPossible() {
  // CHECK-MESSAGES: :[[@LINE+4]]:3: warning: declaration of variable 'a'
  // CHECK-FIXES:      {{^}}  {{$}}
  // CHECK-FIXES-NEXT: {{^}}  {{{$}}
  // CHECK-FIXES-NEXT: {{^}}    while (int a = returnValue()) {{{$}}
  int a;
  {
    while (a = returnValue()) {
    }
  }
}

void variableUsedInSwitchConditionWithAssignmentPossible() {
  // CHECK-MESSAGES: :[[@LINE+4]]:3: warning: declaration of variable 'a'
  // CHECK-FIXES:      {{^}}  {{$}}
  // CHECK-FIXES-NEXT: {{^}}  {{{$}}
  // CHECK-FIXES-NEXT: {{^}}    switch (int a = returnValue()) {{{$}}
  int a;
  {
    switch (a = returnValue()) {
    }
  }
}

// TODO: check for correct removal of selected declarations in multiple declarations:
// int a, b, c;  // e.g. remove only "b"

// TODO: check for correct handling of pointers and arrays in multiple declarations:
// int a, *b, c[10];
