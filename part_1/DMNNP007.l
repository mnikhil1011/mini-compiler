%{
  #include<string.h>
  #include<stdlib.h>
  #include<stdio.h>
  #include <ctype.h>
  int line_number=1;
  int table_index=0;

  struct table_entry{
    int line;
    char lexeme[32];
    char token_type[32];
  }table[1024];

  void add_to_table(int line,char * lexeme,char * type)
  {
    table[table_index].line=line;
    strcpy(table[table_index].lexeme,lexeme);
    strcpy(table[table_index].token_type,type);
    table_index++;
  }

%}

%x comment_state

variable_name [A-Za-z]+[A-Za-z0-9_]*
comment "//"
number ("-")?[0-9]+([.][0-9]+)?
string ["]([^\"\n]*)["]|[']([^\'\n]*)[']

%%

{comment} {BEGIN comment_state;}
<comment_state>\n {line_number++; BEGIN 0;}
<comment_state>. ;

[pP][rR][oO][gG][rR][aA][mM] |
[vV][aA][rR] |
[tT][oO] |
[dD][oO][wW][nN][tT][oO] |
([iI][nN][tT][eE][gG][eE][rR])|([iI][nN][tT]) |
[rR][eE][aA][lL] |
[bB][oO][oO][lL][eE][aA][nN] |
[cC][hH][aA][rR] |
[bB][eE][gG][iI][nN] |
[eE][nN][dD] |
[wW][rR][iI][tT][eE]|[wW][rR][iI][tT][eE][lL][nN] |
[rR][eE][aA][dD] |
[aA][nN][dD] |
[oO][rR] |
[nN][oO][tT] |
[fF][oO][rR] |
[dD][oO] |
[iI][fF] |
[tT][hH][eE][nN] |
[eE][lL][sS][eE] |
[wW][hH][iI][lL][eE] |
[dD][iI][vV] |
[oO][fF] |
[aA][rR][rR][aA][yY] {add_to_table(line_number,yytext,"Keyword");}

[;:,."']|"\("|"\)"|"["|"]" {add_to_table(line_number,yytext,"Punctuator");}

">" |
"<" |
"=" |
":=" |
"<>" |
"<=" |
">=" |
"-" |
[+/%*] {add_to_table(line_number,yytext,"Operator");}

{number} |
{string} {add_to_table(line_number,yytext,"literal");}

{variable_name} {add_to_table(line_number,yytext,"Identifier");}

\n {line_number++;}
[ ] ;
. ;

%%

int main(int argc, char *argv[])
{
  FILE * file=fopen(argv[1],"r");
  if (file == NULL) 
  {
    printf("Failed to open the file.\n");
    return 1;
  }
  yyin=file;
  yylex();
  printf("%-32s %-32s %s\n", "Line Number", "Lexeme", "Token");
  for(int i=0;i<table_index;i++)
    printf("%-32d  %-32s  %s\n",table[i].line,table[i].lexeme,table[i].token_type);
}
