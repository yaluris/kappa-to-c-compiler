%{
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "cgen.h"
#include "myanalyzer.tab.h"

int line_num = 1;
%}


IDENTIFIER		[a-zA-Z][0-9a-zA-Z_]*
DIGIT   		[0-9]
CONST_INT       [1-9]{DIGIT}*|0
CONST_FLOAT     {CONST_INT}("."{DIGIT}+)([eE][+-]?{CONST_INT})?
NORMAL_CHAR     [\.\, 0-9a-zA-Z_\-\+\*\/\:\$\%\!\#\@\&\~\^\(\)]
ESCAPE_CHAR     "\\n"|"\\t"|"\\r"|"\\\\"|"\\\""
CHARACTER       {NORMAL_CHAR}|{ESCAPE_CHAR}
CONST_STRING    \"{CHARACTER}*\"


%%

integer		{ return TK_KEYWORD_INTEGER; }
scalar      { return TK_KEYWORD_SCALAR; }
str         { return TK_KEYWORD_STR; }
boolean     { return TK_KEYWORD_BOOLEAN; }
True        { return TK_KEYWORD_TRUE; }
False       { return TK_KEYWORD_FALSE; }
const       { return TK_KEYWORD_CONST; }           
if          { return TK_KEYWORD_IF; } 
else        { return TK_KEYWORD_ELSE; } 
endif       { return TK_KEYWORD_ENDIF; } 
for         { return TK_KEYWORD_FOR; } 
in          { return TK_KEYWORD_IN; } 
endfor      { return TK_KEYWORD_ENDFOR; } 
while       { return TK_KEYWORD_WHILE; } 
endwhile    { return TK_KEYWORD_ENDWHILE; } 
break       { return TK_KEYWORD_BREAK; }  
continue    { return TK_KEYWORD_CONTINUE; } 
not         { return TK_KEYWORD_NOT; } 
and         { return TK_KEYWORD_AND; } 
or          { return TK_KEYWORD_OR; } 
def         { return TK_KEYWORD_DEF; } 
enddef      { return TK_KEYWORD_ENDDEF; } 
main        { return TK_KEYWORD_MAIN; } 
return      { return TK_KEYWORD_RETURN; } 
comp        { return TK_KEYWORD_COMP; } 
endcomp     { return TK_KEYWORD_ENDCOMP; } 
of          { return TK_KEYWORD_OF; } 

{IDENTIFIER}		{ yylval.str = strdup(yytext); return TK_IDENTIFIER; }
{CONST_INT}		    { yylval.str = strdup(yytext); return TK_CONST_INT; }
{CONST_FLOAT}		{ yylval.str = strdup(yytext); return TK_CONST_FLOAT; }
{CONST_STRING}		{ yylval.str = strdup(yytext); return TK_CONST_STRING; }

"+"		{ return TK_PLUS_OP; }
"-"		{ return TK_MINUS_OP; }
"*"		{ return TK_MULT_OP; }
"/"		{ return TK_DIV_OP; }
"%"		{ return TK_MOD_OP; }
"**"	{ return TK_EXP_OP; }

"=="		{ return TK_EQUAL_OP; }
"!="		{ return TK_NOT_EQUAL_OP; }
"<"			{ return TK_LESS_OP; }
"<="		{ return TK_LESS_OR_EQUAL_OP; }
">"			{ return TK_GREATER_OP; }
">="		{ return TK_GREATER_OR_EQUAL_OP; }

"="		{ return TK_ASSIGN_OP; }
"+="	{ return TK_PLUS_ASSIGN_OP; }
"-="	{ return TK_MINUS_ASSIGN_OP; }
"*="	{ return TK_MULT_ASSIGN_OP; }
"/="	{ return TK_DIV_ASSIGN_OP; }
"%="	{ return TK_MOD_ASSIGN_OP; }
":="	{ return TK_ARRAY_ASSIGN_OP; }

";"		{ return TK_SEMICOLON; }
"("		{ return TK_LEFT_PARENTHESIS; }
")"		{ return TK_RIGHT_PARENTHESIS; }
","		{ return TK_COMMA; }
"["		{ return TK_LEFT_SQUARE_BRACKET; }
"]"		{ return TK_RIGHT_SQUARE_BRACKET; }
":"		{ return TK_COLON; }
"."		{ return TK_PERIOD; }
"#"		{ return TK_HASH; }

"--"[^\n]*		{ } /* eat line comments */
[ \r\t]+		{ } /* eat whitespace */
\n 				{ ++line_num; }

<<EOF>>		{ return EOF; }
.           { printf("Line %d Lexical Error: Unrecognized literal %s\n", line_num, yytext); }


%%
