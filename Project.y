%{

#include <stdio.h>
#include "Proiect.h"
extern FILE* yyin;
extern int yylineno;

%}

%union
{
    char* strval;
    int intval;
    float floatval;
    char charval;
    int boolval;

    struct ExprInfo* expr_ptr;
    struct symbolList* tableEntryList_ptr;
    struct symbolTableEntry* tableEntry_ptr;

    struct functionSymbolTableEntry* functionTableEntry_ptr;
    struct functionSymbolList* functionEntryList_ptr;

    struct exprList* exprList_ptr;
}

%token ENTRYPOINT
%token TYPEOF EVAL
%token <strval> TYPE 
%token BEGIN_FUNCTION END_FUNCTION
%token CONST
%token IF ENDIF ELSE WHILE ENDWHILE FOR ENDFOR
%token RETURN
%token LBRACKET RBRACKET LSQRBRACKET RSQRBRACKET LBRACE RBRACE
%token EQ GT LT GTE LTE
%token AND OR
%token ASSIGN COMMA SEMICOLON PLUS MINUS MUL DIV
%token NEWTYPE ENDTYPEDEFINITION 

%token <strval> IDENTIFIER
%token <intval> NUMBER
%token <strval> STR_VALUE
%token <floatval> FLOAT_VALUE
%token <charval> CHAR_VALUE
%token <intval> TRUE
%token <intval> FALSE

%type <tableEntryList_ptr> identifier_list
%type <strval> variable_declaration
%type <tableEntry_ptr> vector
%type <tableEntryList_ptr> vector_list

%type <functionTableEntry_ptr> function 
%type <functionEntryList_ptr> functions
%type <tableEntry_ptr> parameter
%type <tableEntryList_ptr> parameter_list
%type <strval> function_call
%type <strval> vector_element

%type <exprList_ptr> call_parameter_list

%type <expr_ptr> expr
%type <expr_ptr> bool_expr

%start program

%left MINUS PLUS 
%left MUL DIV
%left EQ GT LT GTE LTE
%left AND OR

%%

program: global_variables functions newtype_list entryPoint {printCompilationStatus();}
    ;

global_variables: variable_declarations
    ;

functions: function {addFunctionEntry(programFunctions, $1);}
    | functions function {addFunctionEntry(programFunctions, $2);}
    ;

typeof: TYPEOF LBRACKET expr RBRACKET {TypeOf($3);};
    ;

eval: EVAL LBRACKET expr RBRACKET {Eval($3);};
    ;

entryPoint: BEGIN_FUNCTION TYPE ENTRYPOINT LBRACKET RBRACKET function_body END_FUNCTION
    | BEGIN_FUNCTION TYPE ENTRYPOINT LBRACKET parameter_list RBRACKET function_body END_FUNCTION
    ;

function: BEGIN_FUNCTION TYPE IDENTIFIER LBRACKET RBRACKET fake_function_body END_FUNCTION {$$ = createFunctionSymbolEntry($3, decideType($2), NULL);}
    |   BEGIN_FUNCTION TYPE IDENTIFIER LBRACKET parameter_list RBRACKET fake_function_body END_FUNCTION {$$ = createFunctionSymbolEntry($3, decideType($2), $5);}
    ;

function_body:
    |   statements
    |   variable_declarations statements
    ;

fake_function_body:
    | statement_block
    | variable_declaration statement_block
    ;

parameter_list: parameter {$$ = createSymbolList($1);}
    | parameter_list COMMA parameter {addEntry($1, $3);}
    ;

parameter: TYPE IDENTIFIER {$$ = createSymbolEntry($2); addType($$, $1);}
    | TYPE vector {addType($2, $1); $$ = $2;}
    ;

function_call: IDENTIFIER LBRACKET call_parameter_list RBRACKET {strcpy($$,$1);checkCall($1, $3);}
    ;

call_parameter_list: expr {$$ = createExprList($1);}
    | call_parameter_list COMMA expr {addExpr($1, $3);} 
    ;

newtype_list: newtype
    | newtype_list newtype
    ;

newtype: NEWTYPE IDENTIFIER field_definition_list ENDTYPEDEFINITION
    | NEWTYPE IDENTIFIER field_definition_list method_definiton_list ENDTYPEDEFINITION
    | NEWTYPE IDENTIFIER method_definiton_list ENDTYPEDEFINITION
    ;

field_definition_list: field_definition
    | field_definition_list field_definition
    ;

field_definition: TYPE identifier_list
    | TYPE vector_list
    ;

method_definiton_list: method_definiton
    | method_definiton_list method_definiton
    ;

method_definiton: BEGIN_FUNCTION TYPE IDENTIFIER LBRACKET RBRACKET fake_function_body END_FUNCTION 
    | BEGIN_FUNCTION TYPE IDENTIFIER LBRACKET parameter_list RBRACKET fake_function_body END_FUNCTION
    ;

statement_block: fake_statement
    | statement_block fake_statement
    ;

fake_statement: fake_assignment
    | if_statement
    | while_statement
    | for_statement
    | return_statement
    ;

fake_assignment: IDENTIFIER ASSIGN expr
    | IDENTIFIER ASSIGN bool_expr 
    | vector_element ASSIGN expr
    ;

statements: statement
    | statements statement
    ;

statement: assignment 
      | if_statement 
      | while_statement
      | for_statement
      | return_statement
      | typeof
      | function_call
      | eval
      ;

for_statement: FOR LBRACKET assignment SEMICOLON bool_expr SEMICOLON assignment RBRACKET statement_block ENDFOR

while_statement: WHILE LBRACKET bool_expr RBRACKET statement_block ENDWHILE

if_statement: IF LBRACKET bool_expr RBRACKET statement_block ENDIF
    |   IF LBRACKET bool_expr RBRACKET statement_block ELSE statement_block ENDIF
    ;

return_statement: RETURN expr
    ;

expr: LBRACKET expr RBRACKET {$$ = $2;}
    | expr MUL expr {$$ = createCompExpr($1, "*", $3);}
    | expr DIV expr {$$ = createCompExpr($1, "/", $3);}
    | expr MINUS expr {$$ = createCompExpr($1, "-", $3);}
    | expr PLUS expr {$$ = createCompExpr($1, "+", $3);}
    | MINUS expr {$$ = createCompExpr(NULL, "-", $2);}
    | NUMBER {$$ = createIntExpr($1);}
    | STR_VALUE {$$ = createStrExpr($1);}
    | FLOAT_VALUE {$$ = createFloatExpr($1);}
    | CHAR_VALUE {$$ = createCharExpr($1);}
    | IDENTIFIER {$$ = createExpr($1);}
    | function_call {$$ = createExpr($1);}
    | vector_element {$$ = createExpr($1);}
    ;

bool_expr: LBRACKET bool_expr RBRACKET {$$ = $2;}
    | bool_expr EQ bool_expr {$$ = createCompExpr($1, "==", $3);}
    | bool_expr GT bool_expr {$$ = createCompExpr($1, ">", $3);}
    | bool_expr LT bool_expr {$$ = createCompExpr($1, "<", $3);}
    | bool_expr GTE bool_expr {$$ = createCompExpr($1, ">=", $3);}
    | bool_expr LTE bool_expr {$$ = createCompExpr($1, "<=", $3);}
    | bool_expr AND bool_expr {$$ = createCompExpr($1, "and", $3);}
    | bool_expr OR bool_expr {$$ = createCompExpr($1, "or", $3);}
    | expr EQ expr {$$ = createCompExpr($1, "==", $3);}
    | expr GT expr {$$ = createCompExpr($1, ">", $3);}
    | expr LT expr {$$ = createCompExpr($1, "<", $3);}
    | expr GTE expr {$$ = createCompExpr($1, ">=", $3);}
    | expr LTE expr {$$ = createCompExpr($1, "<=", $3);}
    | TRUE {$$ = createBoolExpr($1);}
    | FALSE {$$ = createBoolExpr($1);}
    ;

assignment: IDENTIFIER ASSIGN expr {assignValue($1, $3);}
    | IDENTIFIER ASSIGN bool_expr {assignValue($1, $3);}
    | vector_element ASSIGN expr {assignValue($1, $3);}
    ;

variable_declarations: variable_declaration
    | variable_declarations variable_declaration
    ;

variable_declaration: TYPE identifier_list {addSymbol(decideType($1), $2, programSymbols);}
    | TYPE vector_list {addSymbol(decideType($1), $2, programSymbols);}
    | TYPE IDENTIFIER ASSIGN expr{addSymbol(decideType($1), createSymbolList(createSymbolEntry($2)), programSymbols); assignValue($2, $4);}
    ;

vector_element: IDENTIFIER LSQRBRACKET NUMBER RSQRBRACKET {strcpy($$, $1);strcat($$, "[");strcat($$, toString($3));strcat($$, "]");}
    ;

vector_list: vector {$$ = createSymbolList($1);}
    | vector_list COMMA vector {addEntry($1, $3);}
    ;

vector: IDENTIFIER LSQRBRACKET NUMBER RSQRBRACKET {$$ = createArraySymbolEntry($1, $3);}
    ;

identifier_list: IDENTIFIER {$$ = createSymbolList(createSymbolEntry($1));}
    | identifier_list COMMA IDENTIFIER {addEntry($1, createSymbolEntry($3));}
    | CONST IDENTIFIER {$$ = createSymbolList(createSymbolEntry($2));}
    ;

%%

int main(int argc, char **argv)
{
    FILE* f;
    if(argc > 1)
        f = fopen(argv[1], "r");
    yyin = f;

    init();

    yyparse();

    printSymbolList(programSymbols);
    printFunctionList(programFunctions);
}

int yyerror(char * s)
{
    printf("Syntactic error: %s at line: %d\n",s,yylineno);
}
