%{
  #include "y.tab.h"
  #include<string.h>
  #include<stdlib.h>
  #include<stdio.h>
  #include <ctype.h>

  void customerror(char *);

  void toLowercase(char *str) 
  {
    while (*str) 
    {
      if (isalpha((unsigned char)*str)) 
      {
        *str = tolower((unsigned char)*str);
      }
      str++;
    }
  } 

%}

%x comment_state
%x text_state1
%x text_state2

variable_name [A-Za-z]+[A-Za-z0-9_]*
comment "//"

%%

{comment} {BEGIN comment_state;}
<comment_state>\n {BEGIN 0;}
<comment_state>. ;

["] {BEGIN text_state1;}
<text_state1>["] {BEGIN 0;}
<text_state1>.*/["] {yylval.sval=malloc(sizeof(char)*1024); strcpy(yylval.sval,yytext); return TEXT;}

['] {BEGIN text_state2;}
<text_state2>['] {BEGIN 0;}
<text_state2>.*/['] {yylval.sval=malloc(sizeof(char)*1024); strcpy(yylval.sval,yytext); return TEXT;}

[p|P][r|R][o|O][g|G][r|R][a|A][m|M] {return PROGRAM;}


[v|V][a|A][r|R] {return VAR;}
[i|I][n|N][t|T][e|E][g|G][e|E][r|R] {return INTEGER;}
[r|R][e|E][a|A][l|L] {return REAL;}
[b|B][o|O][o|O][l|L][e|E][a|A][n|N] {return BOOLEAN;}
[c|C][h|H][a|A][r|R] {return CHAR;}

[bB][eE][gG][iI][nN] {return BEGIN_STMT;}
[eE][nN][dD] {return END_STMT;}

[wW][rR][iI][tT][eE]|[wW][rR][iI][tT][eE][lL][nN] {return WRITE;}

[rR][eE][aA][dD] {return READ;}


[;:,.] {return *yytext;}
[+/%*=] {return *yytext;}
[dD][iI][vV] {return DIV;}
"-"|">"|"<"|"["|"]"|"("|")" {return *yytext;}
[aA][nN][dD] {return AND;}
[oO][rR] {return OR;}
[nN][oO][tT] {return NOT;}

[fF][oO][rR] {return FOR;}
[dD][oO] {return DO;}
[iI][fF] {return IF;}
[tT][hH][eE][nN] {return THEN;}
[eE][lL][sS][eE] {return ELSE;}
[wW][hH][iI][lL][eE] {return WHILE;}
[tT][oO] {return TO;}
[dD][oO][wW][nN][tT][oO] {return DOWNTO;}
[aA][rR][rR][aA][yY]/[ ]*"[" {return ARRAY;}
[oO][fF] {return OF;}

[Tt][rR][uU][eE] {return TRUE_CONST;}
[Ff][Aa][Ll][Ss][Ee] {return FALSE_CONST;}

{variable_name}/[ ]*"[" {toLowercase(yytext); yylval.sval=malloc(sizeof(char)*1024); strcpy(yylval.sval,yytext); return ARR_ID;}
{variable_name} {toLowercase(yytext); yylval.sval=malloc(sizeof(char)*1024); strcpy(yylval.sval,yytext); return VAR_ID;}



[0-9]+ {yylval.ival=atoi(yytext); return INTEGER_CONST;}
[0-9]+([.][0-9]+) {yylval.fval=atof(yytext); return FLOAT_CONST;}

\n ;
[ ] ;
. { char msg[50]; sprintf(msg,"error reading tokens <%s>",yytext); customerror(msg);}


%%
