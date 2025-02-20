%{
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "cgen.h"

extern int yylex(void);
extern int lineNum;
%}


%union {
	char* str;
}

%token <str> TK_IDENTIFIER
%token <str> TK_CONST_INT
%token <str> TK_CONST_FLOAT
%token <str> TK_CONST_STRING

%token TK_KEYWORD_INTEGER
%token TK_KEYWORD_SCALAR
%token TK_KEYWORD_STR
%token TK_KEYWORD_BOOLEAN
%token TK_KEYWORD_TRUE
%token TK_KEYWORD_FALSE
%token TK_KEYWORD_CONST
%token TK_KEYWORD_IF
%token TK_KEYWORD_ELSE
%token TK_KEYWORD_ENDIF
%token TK_KEYWORD_FOR
%token TK_KEYWORD_IN
%token TK_KEYWORD_ENDFOR
%token TK_KEYWORD_WHILE
%token TK_KEYWORD_ENDWHILE
%token TK_KEYWORD_BREAK
%token TK_KEYWORD_CONTINUE
%token TK_KEYWORD_DEF
%token TK_KEYWORD_ENDDEF
%token TK_KEYWORD_MAIN
%token TK_KEYWORD_RETURN
%token TK_KEYWORD_COMP
%token TK_KEYWORD_ENDCOMP
%token TK_KEYWORD_OF
%token TK_COLON
%token TK_COMMA
%token TK_SEMICOLON
%token TK_HASH

%start input

%type <str> listOfIds dataType compType variableDecl constantDecl expr functionCall functionArguments assignStmt listOfStmts ifStmt forStmt whileStmt stmt mainFunction function functionParameters functionBody functionBodyStmt compDecl compBody compBodyStmt compListOfIds compVariableDecl code codePart

%nonassoc THEN
%nonassoc TK_KEYWORD_ELSE

%right TK_ASSIGN_OP TK_PLUS_ASSIGN_OP TK_MINUS_ASSIGN_OP TK_MULT_ASSIGN_OP TK_DIV_ASSIGN_OP TK_MOD_ASSIGN_OP TK_ARRAY_ASSIGN_OP
%left TK_KEYWORD_OR
%left TK_KEYWORD_AND
%right TK_KEYWORD_NOT
%left TK_EQUAL_OP TK_NOT_EQUAL_OP
%left TK_LESS_OP TK_LESS_OR_EQUAL_OP TK_GREATER_OP TK_GREATER_OR_EQUAL_OP
%left TK_PLUS_OP TK_MINUS_OP
%left TK_MULT_OP TK_DIV_OP TK_MOD_OP
%right UPLUS UMINUS
%right TK_EXP_OP
%left TK_PERIOD TK_LEFT_PARENTHESIS TK_RIGHT_PARENTHESIS TK_LEFT_SQUARE_BRACKET TK_RIGHT_SQUARE_BRACKET


%%

input:
	code
	{
		if (yyerror_count == 0) {
			puts(c_prologue);
			printf("%s\n", $1);
		}
	}
	;

listOfIds:
	TK_IDENTIFIER
	| listOfIds TK_COMMA TK_IDENTIFIER { $$ = template("%s, %s", $1, $3); }
	;

dataType:
	compType
	| TK_KEYWORD_INTEGER { $$ = template("int"); }
	| TK_KEYWORD_SCALAR { $$ = template("double"); }
	| TK_KEYWORD_STR { $$ = template("char*"); }
	| TK_KEYWORD_BOOLEAN { $$ = template("int"); }
	;

compType:
	TK_IDENTIFIER
	;

variableDecl:
	listOfIds TK_COLON dataType TK_SEMICOLON { $$ = template("%s %s;\n", $3, $1); }
	| TK_IDENTIFIER TK_LEFT_SQUARE_BRACKET expr TK_RIGHT_SQUARE_BRACKET TK_COLON dataType TK_SEMICOLON { $$ = template("%s %s[%s];\n", $6, $1, $3); }
	;

constantDecl:
	TK_KEYWORD_CONST TK_IDENTIFIER TK_ASSIGN_OP expr TK_COLON dataType TK_SEMICOLON { $$ = template("const %s %s = %s;\n", $6, $2, $4); }
	;

expr:
	TK_IDENTIFIER
	| TK_CONST_INT
	| TK_CONST_FLOAT
	| TK_CONST_STRING
	| TK_KEYWORD_TRUE { $$ = template("1"); }
	| TK_KEYWORD_FALSE { $$ = template("0"); }
	| expr TK_KEYWORD_OR expr { $$ = template("%s || %s", $1, $3); }
	| expr TK_KEYWORD_AND expr { $$ = template("%s && %s", $1, $3); }
	| TK_KEYWORD_NOT expr { $$ = template("!(%s)", $2); }
	| expr TK_EQUAL_OP expr { $$ = template("%s == %s", $1, $3); }
	| expr TK_NOT_EQUAL_OP expr { $$ = template("%s != %s", $1, $3); }
	| expr TK_LESS_OP expr { $$ = template("%s < %s", $1, $3); }
	| expr TK_LESS_OR_EQUAL_OP expr { $$ = template("%s <= %s", $1, $3); }
	| expr TK_GREATER_OP expr { $$ = template("%s > %s", $1, $3); }
	| expr TK_GREATER_OR_EQUAL_OP expr { $$ = template("%s >= %s", $1, $3); }
	| expr TK_PLUS_OP expr { $$ = template("%s + %s", $1, $3); }
	| expr TK_MINUS_OP expr { $$ = template("%s - %s", $1, $3); }
	| expr TK_MULT_OP expr { $$ = template("%s * %s", $1, $3); }
	| expr TK_DIV_OP expr { $$ = template("%s / %s", $1, $3); }
	| expr TK_MOD_OP expr { $$ = template("%s %% %s", $1, $3); }
	| TK_PLUS_OP expr %prec UPLUS { $$ = template("+%s", $2); }
	| TK_MINUS_OP expr %prec UMINUS { $$ = template("-%s", $2); }
	| expr TK_EXP_OP expr { $$ = template("pow(%s, %s)", $1, $3); }
	| TK_LEFT_PARENTHESIS expr TK_RIGHT_PARENTHESIS { $$ = template("(%s)", $2); }
	| expr TK_LEFT_SQUARE_BRACKET expr TK_RIGHT_SQUARE_BRACKET { $$ = template("%s[%s]", $1, $3); }
	| expr TK_PERIOD expr { $$ = template("%s.%s", $1, $3); }
	| TK_HASH TK_IDENTIFIER { $$ = template("%s", $2); }
	| functionCall
	;

functionCall:
	TK_IDENTIFIER TK_LEFT_PARENTHESIS functionArguments TK_RIGHT_PARENTHESIS { $$ = template("%s(%s)", $1, $3); }
	;

functionArguments:
	%empty { $$ = template(""); }
	| expr { $$ = template("%s", $1); }
	| functionArguments TK_COMMA expr { $$ = template("%s, %s", $1, $3); }
	;

assignStmt:
	expr TK_ASSIGN_OP expr TK_SEMICOLON { $$ = template("%s = %s;\n", $1, $3); }
	| expr TK_PLUS_ASSIGN_OP expr TK_SEMICOLON { $$ = template("%s += %s;\n", $1, $3); }
	| expr TK_MINUS_ASSIGN_OP expr TK_SEMICOLON { $$ = template("%s -= %s;\n", $1, $3); }
	| expr TK_MULT_ASSIGN_OP expr TK_SEMICOLON { $$ = template("%s *= %s;\n", $1, $3); }
	| expr TK_DIV_ASSIGN_OP expr TK_SEMICOLON { $$ = template("%s /= %s;\n", $1, $3); }
	| expr TK_MOD_ASSIGN_OP expr TK_SEMICOLON { $$ = template("%s %%= %s;\n", $1, $3); }
	| TK_IDENTIFIER TK_ARRAY_ASSIGN_OP TK_LEFT_SQUARE_BRACKET expr TK_KEYWORD_FOR TK_IDENTIFIER TK_COLON expr TK_RIGHT_SQUARE_BRACKET TK_COLON dataType TK_SEMICOLON { $$ = template("%s* %s = (%s*)malloc((%s)*sizeof(%s));\nfor (int %s = 0; %s < (%s); ++%s) {\n%s[%s] = %s;\n}\n", $11, $1, $11, $8, $11, $6, $6, $8, $6, $1, $6, $4); }
	| TK_IDENTIFIER TK_ARRAY_ASSIGN_OP TK_LEFT_SQUARE_BRACKET expr TK_KEYWORD_FOR TK_IDENTIFIER TK_COLON dataType TK_KEYWORD_IN TK_IDENTIFIER TK_KEYWORD_OF expr TK_RIGHT_SQUARE_BRACKET TK_COLON dataType TK_SEMICOLON { $$ = template("%s* %s = (%s*)malloc((%s)*sizeof(%s));\nfor (int a_i = 0; a_i < (%s); ++a_i) {\n%s = %s[a_i];\n%s[a_i] = %s;\n}\n", $15, $1, $15, $12, $15, $12, $6, $10, $1, $4); }
	;

listOfStmts:
	stmt
	| listOfStmts stmt { $$ = template("%s%s", $1, $2); }
	;

ifStmt:
	TK_KEYWORD_IF TK_LEFT_PARENTHESIS expr TK_RIGHT_PARENTHESIS TK_COLON listOfStmts TK_KEYWORD_ELSE TK_COLON listOfStmts TK_KEYWORD_ENDIF TK_SEMICOLON { $$ = template("if (%s) {\n%s} else {\n%s}\n", $3, $6, $9); }
	| TK_KEYWORD_IF TK_LEFT_PARENTHESIS expr TK_RIGHT_PARENTHESIS TK_COLON listOfStmts %prec THEN TK_KEYWORD_ENDIF TK_SEMICOLON { $$ = template("if (%s) {\n%s}\n", $3, $6); }
	;

forStmt:
	TK_KEYWORD_FOR TK_IDENTIFIER TK_KEYWORD_IN TK_LEFT_SQUARE_BRACKET expr TK_COLON expr TK_COLON expr TK_RIGHT_SQUARE_BRACKET TK_COLON listOfStmts TK_KEYWORD_ENDFOR TK_SEMICOLON { $$ = template("for (int %s = %s; %s < %s; %s += %s) {\n%s}\n", $2, $5, $2, $7, $2, $9, $12); }
	| TK_KEYWORD_FOR TK_IDENTIFIER TK_KEYWORD_IN TK_LEFT_SQUARE_BRACKET expr TK_COLON expr TK_RIGHT_SQUARE_BRACKET TK_COLON listOfStmts TK_KEYWORD_ENDFOR TK_SEMICOLON { $$ = template("for (int %s = %s; %s < %s; %s++) {\n%s}\n", $2, $5, $2, $7, $2, $10); }
	;

whileStmt:
	TK_KEYWORD_WHILE TK_LEFT_PARENTHESIS expr TK_RIGHT_PARENTHESIS TK_COLON listOfStmts TK_KEYWORD_ENDWHILE TK_SEMICOLON { $$ = template("while (%s) {\n%s}\n", $3, $6); }
	;

stmt:
	assignStmt
	| TK_SEMICOLON { $$ = template(";\n"); }
	| ifStmt
	| forStmt
	| whileStmt
	| TK_KEYWORD_BREAK TK_SEMICOLON { $$ = template("break;\n"); }
	| TK_KEYWORD_CONTINUE TK_SEMICOLON { $$ = template("continue;\n"); }
	| TK_KEYWORD_RETURN TK_SEMICOLON { $$ = template("return;\n"); }
	| TK_KEYWORD_RETURN expr TK_SEMICOLON { $$ = template("return %s;\n", $2); }
	| functionCall TK_SEMICOLON { $$ = template("%s;\n", $1); }
	| expr TK_PERIOD functionCall TK_SEMICOLON { $$ = template("%s.%s;\n", $1, $3); }
	;

mainFunction:
	TK_KEYWORD_DEF TK_KEYWORD_MAIN TK_LEFT_PARENTHESIS TK_RIGHT_PARENTHESIS TK_COLON functionBody TK_KEYWORD_ENDDEF TK_SEMICOLON { $$ = template("int main() {\n%s}\n", $6); }
	;

function:
	TK_KEYWORD_DEF TK_IDENTIFIER TK_LEFT_PARENTHESIS functionParameters TK_RIGHT_PARENTHESIS TK_MINUS_OP TK_GREATER_OP dataType TK_COLON functionBody TK_KEYWORD_ENDDEF TK_SEMICOLON { $$ = template("%s %s(%s) {\n%s}\n", $8, $2, $4, $10); }
	| TK_KEYWORD_DEF TK_IDENTIFIER TK_LEFT_PARENTHESIS functionParameters TK_RIGHT_PARENTHESIS TK_COLON functionBody TK_KEYWORD_ENDDEF TK_SEMICOLON { $$ = template("void %s(%s) {\n%s}\n", $2, $4, $7); }
	;

functionParameters:
	%empty { $$ = template(""); }
	| TK_IDENTIFIER TK_COLON dataType { $$ = template("%s %s", $3, $1); }
	| TK_IDENTIFIER TK_LEFT_SQUARE_BRACKET TK_RIGHT_SQUARE_BRACKET TK_COLON dataType { $$ = template("%s *%s", $5, $1); }
	| TK_IDENTIFIER TK_COLON dataType TK_COMMA functionParameters { $$ = template("%s %s, %s", $3, $1, $5); }
	| TK_IDENTIFIER TK_LEFT_SQUARE_BRACKET TK_RIGHT_SQUARE_BRACKET TK_COLON dataType TK_COMMA functionParameters { $$ = template("%s *%s, %s", $5, $1, $7); }
	;

functionBody:
	functionBodyStmt
	| functionBody functionBodyStmt { $$ = template("%s%s", $1, $2); }
	;

functionBodyStmt:
	variableDecl
	| constantDecl
	| listOfStmts
	;

compDecl:
	TK_KEYWORD_COMP compType TK_COLON compBody TK_KEYWORD_ENDCOMP TK_SEMICOLON { $$ = template("#undef SELF\n#define SELF struct %s *self\ntypedef struct %s {\n%s} %s;\n", $2, $2, $4, $2); }
	;

compBody:
	compBodyStmt
	| compBody compBodyStmt { $$ = template("%s%s", $1, $2); }
	;

compBodyStmt:
	compVariableDecl
	| function
	;

compListOfIds:
	TK_HASH TK_IDENTIFIER { $$ = template("%s", $2); }
	| compListOfIds TK_COMMA TK_HASH TK_IDENTIFIER { $$ = template("%s, %s", $1, $4); }
	;

compVariableDecl:
	compListOfIds TK_COLON dataType TK_SEMICOLON { $$ = template("%s %s;\n", $3, $1); }
	| TK_HASH TK_IDENTIFIER TK_LEFT_SQUARE_BRACKET expr TK_RIGHT_SQUARE_BRACKET TK_COLON dataType TK_SEMICOLON { $$ = template("%s %s[%s];\n", $7, $2, $4); }
	;

code:
	codePart
	| code codePart { $$ = template("%s%s", $1, $2); }
	;

codePart:
	variableDecl
	| constantDecl
	| function
	| mainFunction
	| compDecl
	;


%%

int main()
{
	if (yyparse() == 0)
		printf("Accepted!\n");
	else
		printf("Rejected!\n");
}
