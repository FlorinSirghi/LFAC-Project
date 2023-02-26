%{

#include <stdio.h>
#include "y.tab.h"

%}

%option noyywrap

%%

("int"|"bool"|"float"|"string"|"char") {yylval.strval = strdup(yytext); return TYPE;} 

"entryPoint" {return ENTRYPOINT;}
"TypeOf" {return TYPEOF;}
"Eval" {return EVAL;}

"function" {return BEGIN_FUNCTION;}
"end" {return END_FUNCTION;}

"const" {return CONST;}

"if" {return IF;}
"endif" {return ENDIF;}
"else" {return ELSE;}
"while" {return WHILE;}
"endwhile" {return ENDWHILE;}
"for" {return FOR;}
"endfor" {return ENDFOR;}

"true" {yylval.boolval = 1 ; return TRUE;}
"false" {yylval.boolval = 0; return FALSE;}

"newType" {return NEWTYPE;}
"endTypeDefinition" {return ENDTYPEDEFINITION;}

"return" {return RETURN;}

"(" {return LBRACKET;}
")" {return RBRACKET;}
"[" {return LSQRBRACKET;}
"]" {return RSQRBRACKET;}
"{" {return LBRACE;}
"}" {return RBRACE;}

"=" {return ASSIGN;}
"," {return COMMA;}
";" {return SEMICOLON;}
"+" {return PLUS;}
"-" {return MINUS;}
"*" {return MUL;}
"/" {return DIV;}

"==" {return EQ;}
">" {return GT;}
"<" {return LT;}
">=" {return GTE;}
"<=" {return LTE;}

"or" {return AND;}
"and" {return OR;}

('.') {yylval.charval = yytext[1]; return CHAR_VALUE;}
([1-9]([0-9]*)|0) {yylval.intval = atoi(yytext); return NUMBER;}
([0-9]*\.[0-9]+) {yylval.floatval = atof(yytext); return FLOAT_VALUE;}
\"[^"\n]*["\n] {yylval.strval = strdup(yytext); return STR_VALUE;}
([a-z]|[A-Z]|[0-9])+ {yylval.strval= strdup(yytext); return IDENTIFIER;}

[ \t]   ;

\n  {yylineno++;}

.   {return yytext[0];}

%%
