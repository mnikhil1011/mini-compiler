%{

  #include<stdio.h>
  #include<stdlib.h>
  #include<string.h>
  #include<stdbool.h>
  int yylex(void);
  extern FILE *yyin;


  // part 4 from here
  int temp_var_num=0,temp_lab_num=0,tos=-1;
  struct TAC{
    char *s1;
    char *op;
    char *s2;
    char *s3;
  }tac[8224];
  int line_num=0;

  void add_tac(char *s1,char *s2,char *op,char *s3)
  {
    tac[line_num].s1=s1;
    tac[line_num].s2=s2;
    tac[line_num].s3=s3;
    tac[line_num].op=op;
    line_num++;
    return;
  }


%}

%union {
  char* sval;
  int ival;
  float fval;
  struct{
    union{
      int ival;//1
      float fval;//2
      char cval;
    }val;
    int type;
  }float_int;

  struct{
    int first;
    int second;
  }pair_int;

}

%token VAR INTEGER BOOLEAN CHAR REAL PROGRAM BEGIN_STMT END_STMT WRITE READ IF ELSE AND OR NOT DOWNTO TO ARRAY OF DIV TRUE_CONST FALSE_CONST
%token<ival> INTEGER_CONST
%token<fval> FLOAT_CONST
%token<sval> TEXT VAR_ID ARR_ID
%token<ival> DO THEN WHILE FOR


%type<sval> expression_statement arithmetic_statement logical_expression variable logical_expression_helper assignment_statement
%type<float_int>number_const
%type<ival>loop_statement conditional_statement for_count

%left OR
%left AND
%right NOT
%left '%'
%left '+' '-'
%left '*' '/'
%left DIV


%%

S : program '.' {return 1;};

program: program_header var_structure program_block;


program_header : PROGRAM VAR_ID ';' ;


var_structure : VAR  var_line ;

var_line : var_ids ':' var_type ';'  var_line 
  | VAR_ID  ':' var_type ';'  var_line 
  | VAR_ID ':' ARRAY '[' INTEGER_CONST '.' '.' INTEGER_CONST ']' OF var_type ';' var_line 
  | 
  ;

var_ids : VAR_ID ','  VAR_ID  next_var  ;

next_var:  ',' VAR_ID next_var 
  | 
  ;

var_type : INTEGER 
  | REAL 
  | CHAR 
  | BOOLEAN 
  ;



program_block :BEGIN_STMT block_body END_STMT ;

block_body: program_statement ';' block_body |  ;

program_statement : WRITE '(' print_statement ')' | READ '(' scan_statement ')' | assignment_statement | conditional_statement | loop_statement;

print_statement : print_variable
  | TEXT  
  |
  ;

print_variable : variable ',' print_variable | variable ;

scan_statement : variable 
  ;

assignment_statement: variable ':' '=' expression_statement {
      printf("%s = %s\n",$1,$4);
      tac[line_num].s1=$1;
      tac[line_num].s2=$4;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,":=");
      line_num++;
      $$=$1;
    }
  | variable ':' '=' TEXT {
      printf("%s = %s\n",$1,$4);
      tac[line_num].s1=$1;
      tac[line_num].s2=$4;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,":=");
      line_num++;
      $$=$1;
    }
  ;

expression_statement:  arithmetic_statement  {$$=$1;}
  | logical_expression {$$=$1;}
  ;

variable :  VAR_ID {$$=$1;}
  | ARR_ID '[' arithmetic_statement ']' {char*temp=malloc(sizeof(char)*1024);sprintf(temp,"%s[%s]",$1,$3);$$=temp;}
  |'(' variable ')' {$$=$2;}
  ;

 
arithmetic_statement: arithmetic_statement '+' arithmetic_statement {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      printf("%s = %s + %s\n",$$,$1,$3);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].s3=$3;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"+");
      line_num++;
    }
  | arithmetic_statement '*' arithmetic_statement {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      printf("%s = %s * %s\n",$$,$1,$3);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].s3=$3;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"*");
      line_num++;
    }
  | arithmetic_statement '-' arithmetic_statement {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      printf("%s = %s - %s\n",$$,$1,$3);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].s3=$3;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"-");
      line_num++;
    }
  | arithmetic_statement '/' arithmetic_statement {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      printf("%s = %s / %s\n",$$,$1,$3);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].s3=$3;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"/");
      line_num++;
    }
  | arithmetic_statement DIV arithmetic_statement {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      printf("%s = %s DIV %s\n",$$,$1,$3);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].s3=$3;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"DIV");
      line_num++;
    }
  | arithmetic_statement '%' arithmetic_statement {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      printf("%s = %s %% %s\n",$$,$1,$3);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].s3=$3;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"%%");
      line_num++;
    }
  | '(' arithmetic_statement ')' {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      printf("%s = %s\n",$$,$2);
      tac[line_num].s1=$$;
      tac[line_num].s2=$2;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,":=");
      line_num++;
    }
  | '+' '(' arithmetic_statement ')' {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      printf("%s = %s\n",$$,$3);
      tac[line_num].s1=$$;
      tac[line_num].s2=$3;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,":=");
      line_num++;
    }
  | '-'  '(' arithmetic_statement ')' {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      printf("%s = -%s\n",$$,$3);
      tac[line_num].s1=$$;
      tac[line_num].s2=$3;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,":-");
      line_num++;
    }
  | variable {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      printf("%s = %s\n",$$,$1);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,":=");
      line_num++;
    }
  | '-' variable {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      printf("%s = -%s\n",$$,$2);
      tac[line_num].s1=$$;
      tac[line_num].s2=$2;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,":-");
      line_num++;
    }
  | number_const {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      if($<float_int.type>1==1)
      {
        printf("%s = %d\n",$$,$<float_int.val.ival>1);
        tac[line_num].s1=$$;
        tac[line_num].s2=malloc(sizeof(char)*16);;
        tac[line_num].op=malloc(sizeof(char)*16);
        sprintf(tac[line_num].op,":=");
        sprintf(tac[line_num].s2,"%d",$<float_int.val.ival>1);
        line_num++;
      }
      else
      {
        printf("%s = %f\n",$$,$<float_int.val.fval>1);
        tac[line_num].s1=$$;
        tac[line_num].s2=malloc(sizeof(char)*16);;
        tac[line_num].op=malloc(sizeof(char)*16);
        sprintf(tac[line_num].op,":=");
        sprintf(tac[line_num].s2,"%f",$<float_int.val.fval>1);
        line_num++;
      }
    }
  | '-' number_const {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      if($<float_int.type>2==1)
      {
        printf("%s = -%d\n",$$,$<float_int.val.ival>2);
        tac[line_num].s1=$$;
        tac[line_num].s2=malloc(sizeof(char)*16);;
        tac[line_num].op=malloc(sizeof(char)*16);
        sprintf(tac[line_num].op,":-");
        sprintf(tac[line_num].s2,"%d",$<float_int.val.ival>2);
        line_num++;
      }
      else
      {
        printf("%s = -%f\n",$$,$<float_int.val.fval>2);
        tac[line_num].s1=$$;
        tac[line_num].s2=malloc(sizeof(char)*16);
        tac[line_num].op=malloc(sizeof(char)*16);
        sprintf(tac[line_num].op,":=");
        sprintf(tac[line_num].s2,"%f",$<float_int.val.fval>2);
        line_num++;
      }
    }
  ; 

number_const: INTEGER_CONST { $<float_int.type>$=1;$<float_int.val.ival>$=$1;}
  | FLOAT_CONST {$<float_int.type>$=2;$<float_int.val.fval>$=$1;}
  ;



conditional_statement :IF logical_expression_helper THEN {

      printf("if %s goto l%d\n",$2,temp_lab_num);
      tac[line_num].s1=$2;
      tac[line_num].s2=malloc(sizeof(char)*16);
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"IF");
      sprintf(tac[line_num].s2,"l%d",temp_lab_num);
      line_num++;
      $3=temp_lab_num+1;
      printf("goto l%d\n",$3);
      tac[line_num].s2=malloc(sizeof(char)*16);
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"GOTO");
      sprintf(tac[line_num].s2,"l%d",$3);
      line_num++;

      printf("l%d:\n",temp_lab_num);
      tac[line_num].s2=malloc(sizeof(char)*16);
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"LAB");
      sprintf(tac[line_num].s2,"l%d",temp_lab_num);
      line_num++;
      temp_lab_num+=2;


} program_block{

      
      printf("goto l%d\n",temp_lab_num);
      tac[line_num].s2=malloc(sizeof(char)*16);
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"GOTO");
      sprintf(tac[line_num].s2,"l%d",temp_lab_num);
      line_num++;


      printf("l%d:\n",$3);
      tac[line_num].s2=malloc(sizeof(char)*16);
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"LAB");
      sprintf(tac[line_num].s2,"l%d",$3);
      line_num++;

      $3=temp_lab_num;
      temp_lab_num++;

} maybe_else_block {
      printf("l%d:\n",$3);
      tac[line_num].s2=malloc(sizeof(char)*16);
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"LAB");
      sprintf(tac[line_num].s2,"l%d",$3);
      line_num++;
}
  ;
maybe_else_block : ELSE program_block 
  | 
  ;

logical_expression_helper: variable {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      printf("%s = %s\n",$$,$1);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,":=");
      line_num++;
    } 
  |logical_expression {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      printf("%s = %s\n",$$,$1);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,":=");
      line_num++;
    }
  ;

logical_expression : NOT logical_expression {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      printf("%s = NOT %s\n",$$,$2);
      tac[line_num].s1=$$;
      tac[line_num].s2=$2;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"NOT");
      line_num++;
    } 
  | '(' logical_expression ')' {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      printf("%s = %s\n",$$,$2);
      tac[line_num].s1=$$;
      tac[line_num].s2=$2;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,":=");
      line_num++;
    } 
  | NOT variable {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      printf("%s = NOT %s\n",$$,$2);
      tac[line_num].s1=$$;
      tac[line_num].s2=$2;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"NOT");
      line_num++;
    } 
  | arithmetic_statement '>' arithmetic_statement {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      printf("%s = %s > %s\n",$$,$1,$3);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].s3=$3;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,">");
      line_num++;
    }
  | arithmetic_statement '>' '=' arithmetic_statement {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      printf("%s = %s >= %s\n",$$,$1,$4);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].s3=$4;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,">=");
      line_num++;
    }
  | arithmetic_statement '<' '=' arithmetic_statement {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      printf("%s = %s <= %s\n",$$,$1,$4);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].s3=$4;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"<=");
      line_num++;
    }
  | arithmetic_statement '<' arithmetic_statement {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      printf("%s = %s < %s\n",$$,$1,$3);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].s3=$3;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"<");
      line_num++;
    }
  | arithmetic_statement '=' arithmetic_statement {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      printf("%s = %s == %s\n",$$,$1,$3);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].s3=$3;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"==");
      line_num++;
    }
  | arithmetic_statement '<' '>' arithmetic_statement {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      printf("%s = %s != %s\n",$$,$1,$4);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].s3=$4;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"!=");
      line_num++;
    }
  | logical_expression AND logical_expression {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      printf("%s = %s AND %s\n",$$,$1,$3);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].s3=$3;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"AND");
      line_num++;
    }
  | logical_expression OR logical_expression {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      printf("%s = %s OR %s\n",$$,$1,$3);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].s3=$3;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"OR");
      line_num++;
    }
  | variable AND variable {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      printf("%s = %s AND %s\n",$$,$1,$3);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].s3=$3;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"AND");
      line_num++;
    }
  | logical_expression AND variable {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      printf("%s = %s AND %s\n",$$,$1,$3);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].s3=$3;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"AND");
      line_num++;
    }
  | variable AND logical_expression {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      printf("%s = %s AND %s\n",$$,$1,$3);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].s3=$3;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"AND");
      line_num++;
    }
  | variable OR logical_expression {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      printf("%s = %s OR %s\n",$$,$1,$3);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].s3=$3;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"OR");
      line_num++;
    }
  | variable OR variable {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      printf("%s = %s OR %s\n",$$,$1,$3);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].s3=$3;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"OR");
      line_num++;
    }
  | logical_expression OR variable {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      printf("%s = %s OR %s\n",$$,$1,$3);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].s3=$3;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"OR");
      line_num++;
    }
  | TRUE_CONST {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      printf("%s = TRUE \n",$$);
      tac[line_num].s1=$$;
      tac[line_num].s2=malloc(sizeof(char)*16);;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,":=");
      sprintf(tac[line_num].s2,"TRUE");
      line_num++;
    }
  | FALSE_CONST {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      printf("%s = FALSE \n",$$);
      tac[line_num].s1=$$;
      tac[line_num].s2=malloc(sizeof(char)*16);;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,":=");
      sprintf(tac[line_num].s2,"FALSE");
      line_num++;
    } 
  ;

loop_statement : WHILE {

      $1=temp_lab_num;
      temp_lab_num++;
      printf("l%d:\n",$1);
      tac[line_num].s2=malloc(sizeof(char)*16);
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"LAB");
      sprintf(tac[line_num].s2,"l%d",$1);
      line_num++;    

  } logical_expression_helper DO {  

      printf("if %s goto l%d\n",$3,temp_lab_num);
      tac[line_num].s1=$3;
      tac[line_num].s2=malloc(sizeof(char)*16);
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"IF");
      sprintf(tac[line_num].s2,"l%d",temp_lab_num);
      line_num++;
      $4=temp_lab_num+1;
      printf("goto l%d\n",$4);
      tac[line_num].s2=malloc(sizeof(char)*16);
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"GOTO");
      sprintf(tac[line_num].s2,"l%d",$4);
      line_num++;

      printf("l%d:\n",temp_lab_num);
      tac[line_num].s2=malloc(sizeof(char)*16);
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"LAB");
      sprintf(tac[line_num].s2,"l%d",temp_lab_num);
      line_num++;
      temp_lab_num+=2;

    } program_block {
        printf("goto l%d\n",$1);
        tac[line_num].s2=malloc(sizeof(char)*16);
        tac[line_num].op=malloc(sizeof(char)*16);
        sprintf(tac[line_num].op,"GOTO");
        sprintf(tac[line_num].s2,"l%d",$1);
        line_num++;

        printf("l%d:\n",$4);
        tac[line_num].s2=malloc(sizeof(char)*16);
        tac[line_num].op=malloc(sizeof(char)*16);
        sprintf(tac[line_num].op,"LAB");
        sprintf(tac[line_num].s2,"l%d",$4);
        line_num++;
      }
| FOR assignment_statement {
      $1=temp_lab_num;
      temp_lab_num++;
      printf("l%d:\n",$1);
      tac[line_num].s2=malloc(sizeof(char)*16);
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"LAB");
      sprintf(tac[line_num].s2,"l%d",$1);
      line_num++;  
  } for_count arithmetic_statement DO {

      

      printf("if %s<=%s goto l%d\n",$2,$5,temp_lab_num);
      tac[line_num].s1=$2;
      tac[line_num].s2=malloc(sizeof(char)*16);
      tac[line_num].s3=$5;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"IF_2");
      sprintf(tac[line_num].s2,"l%d",temp_lab_num);
      line_num++;

      $6=temp_lab_num+1;
      printf("goto l%d\n",$6);
      tac[line_num].s2=malloc(sizeof(char)*16);
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"GOTO");
      sprintf(tac[line_num].s2,"l%d",$6);
      line_num++;

      printf("l%d:\n",temp_lab_num);
      tac[line_num].s2=malloc(sizeof(char)*16);
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"LAB");
      sprintf(tac[line_num].s2,"l%d",temp_lab_num);
      line_num++;
      temp_lab_num+=2;


      
    }program_block{
        
        tac[line_num].op=malloc(sizeof(char)*16);
        if($4==1)
        { 
          printf("%s = %s + 1\n",$2,$2);
          sprintf(tac[line_num].op,"+");
        }
        else
        {
          printf("%s = %s - 1\n",$2,$2);
          sprintf(tac[line_num].op,"-");
        }
        tac[line_num].s1=$2;
        tac[line_num].s2=$2;
        tac[line_num].s3=malloc(sizeof(char)*16);
        sprintf(tac[line_num].s3,"1");
        line_num++;

        printf("goto l%d\n",$1);
        tac[line_num].s2=malloc(sizeof(char)*16);
        tac[line_num].op=malloc(sizeof(char)*16);
        sprintf(tac[line_num].op,"GOTO");
        sprintf(tac[line_num].s2,"l%d",$1);
        line_num++;

        printf("l%d:\n",$6);
        tac[line_num].s2=malloc(sizeof(char)*16);
        tac[line_num].op=malloc(sizeof(char)*16);
        sprintf(tac[line_num].op,"LAB");
        sprintf(tac[line_num].s2,"l%d",$6);
        line_num++;
      } 
  ;
for_count: TO {$$=1;}| DOWNTO {$$=-1;};


%%

void main(int argc, char *argv[])
{
  FILE * file=fopen(argv[1],"r");
  if (file == NULL) 
  {
    printf("Failed to open the file.\n");
    return 1;
  }
  yyin=file;
  yyparse();
  /* printf("tac table\n\n\n");
  for(int i=0;i<line_num;i++)
  {
    printf("%s = %s %s %s\n",tac[i].s1,tac[i].s2,tac[i].op,tac[i].s3);
  } */
  //printf("valid input\n");
  exit(0);
}

void yyerror()
{
  printf("compilation error");
  exit(0);
}

void customerror(char*s)
{
  printf("%s",s);
  exit(0);
}