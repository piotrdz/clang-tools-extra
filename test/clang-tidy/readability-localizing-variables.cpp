// RUN: %check_clang_tidy %s readability-localizing-variables %t

int returnValue();
void useByReference(int& a);
int useByReferenceAndReturn(int& a);
void call();


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

// TODO: check nested scopes, control statements, etc.

// TODO: check for correct removal of selected declarations in multiple declarations:
// int a, b, c;  // e.g. remove only "b"

// TODO: check for correct handling of pointers and arrays in multiple declarations:
// int a, *b, c[10];
