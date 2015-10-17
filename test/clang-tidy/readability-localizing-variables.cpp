// RUN: %python %S/check_clang_tidy.py %s readability-localizing-variables %t

int returnValue();
void useByReference(int& a);
void call();


/////// Variable declared and later used in assignment

void variableDeclarationAndAssignmentTogether() {
  int a = 1;
}

void variableDeclarationAndAssignmentInNextLine() {
  // CHECK-MESSAGES: :[[@LINE+3]]:3: warning: declaration of variable 'a' can be localized by moving it closer to its uses [readability-localizing-variables]
  // CHECK-FIX: {{^}}{{$}}
  // CHECK-FIX: {{^}}  int a = 1;{{$}}
  int a;
  a = 1;
}

void variableDeclarationAndAssignmentSeparatedByEmptyLine() {
  // CHECK-MESSAGES: :[[@LINE+3]]:3: warning: declaration of variable 'a'
  // CHECK-FIX: {{^}}{{$}}
  // CHECK-FIX: {{^}}  int a = 1;{{$}}
  int a;

  a = 1;
}

void variableDeclarationAndAssignmentSeparatedByOtherStatement() {
  // CHECK-MESSAGES: :[[@LINE+3]]:3: warning: declaration of variable 'a'
  // CHECK-FIX: {{^}}{{$}}
  // CHECK-FIX: {{^}}  int a = 1;{{$}}
  int a;
  call();
  a = 1;
}


/////// Variable declared and later used in expression

void variableDeclarationAndUseInExpressionWithoutAnythingBetween() {
  int a;
  useByReference(a);
}

void variableDeclarationAndUseInExpressionSeparatedByEmptyLine() {
  // CHECK-MESSAGES: :[[@LINE+3]]:3: warning: declaration of variable 'a'
  // CHECK-FIX: {{^}}{{$}}
  // CHECK-FIX: {{^}}  int a;{{$}}
  int a;

  useByReference(a);
}

void variableDeclarationAndUseInExpressionSeparatedByOtherStatement() {
  // CHECK-MESSAGES: :[[@LINE+3]]:3: warning: declaration of variable 'a'
  // CHECK-FIX: {{^}}{{$}}
  // CHECK-FIX: {{^}}  int a;{{$}}
  int a;
  call();
  useByReference(a);
}


/////// Chained assignments treated as expressions

void variableDeclarationAndChainedAssignmentWithoutAnythingBetween() {
  int a, b;
  a = b = returnValue();
}

void variableDeclarationAndChainedAssignmentSeparatedByEmptyLine() {
  // CHECK-MESSAGES: :[[@LINE+3]]:3: warning: declaration of variables 'a', 'b'
  // CHECK-FIX: {{^}}{{$}}
  // CHECK-FIX: {{^}}  int a, b;{{$}}
  int a, b;

  a = b = returnValue();
}

// TODO: check for correct removal of selected declarations in multiple declarations:
// int a, b, c;  // e.g. remove only "b"

// TODO: check for correct handling of pointers and arrays in multiple declarations:
// int a, *b, c[10];
