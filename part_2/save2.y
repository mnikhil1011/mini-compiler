%{

  #include<stdio.h>
  #include<stdlib.h>
  #include<string.h>
  #include<stdbool.h>
  int yylex(void);
  extern FILE *yyin;

  struct Symbol_table{
    char* id;
    int type;
    bool arr_flag;
    int start;
    int end;
    union{
      int ival;
      float fval;
      char cval;      
      bool bval;
      //for arr
      int * ival_ptr;
      float * fval_ptr;
      char * cval_ptr;
      bool * bval_ptr;
    }value;
    
  }symbol_table[1024];

  int end_pos=0;

  void assign_arr_space()
  {
    switch(symbol_table[end_pos].type)
    {
      case 1:
        symbol_table[end_pos].value.ival_ptr=(int*)malloc((symbol_table[end_pos].end-symbol_table[end_pos].start+1)*sizeof(int));
        break;
      case 2:
        symbol_table[end_pos].value.fval_ptr=(float*)malloc((symbol_table[end_pos].end-symbol_table[end_pos].start+1)*sizeof(float));
        break;
      case 3:
        symbol_table[end_pos].value.cval_ptr=(char*)malloc((symbol_table[end_pos].end-symbol_table[end_pos].start+1)*sizeof(char));
        break;
      case 4:
        symbol_table[end_pos].value.bval_ptr=(bool*)malloc((symbol_table[end_pos].end-symbol_table[end_pos].start+1)*sizeof(bool));
        break;

    }
  }

  void check_var_id(char* s)
  {
    for(int i=0;i<end_pos;i++)
    {
      if(strcmp(s,symbol_table[i].id)==0)
      {
        printf("multiple declarations of  a variable %s",s);
        exit(0);
      }
    }
  }

  int find_pos_id(char* s)
  {
    for(int i=0;i<end_pos;i++)
    {
      if(strcmp(s,symbol_table[i].id)==0)
      {
        return i;
      }
    }
    printf("undeclared variable %s",s);
    exit(0);

  }
  void check_bounds(int var ,int index)
  {
    if(index>=symbol_table[var].start && index<=symbol_table[var].end)
      return;
    printf("index out of bounds var_id:%s index:%d bounds:%d to %d",symbol_table[var].id,index,symbol_table[var].start,symbol_table[var].end);
    exit(0);
  }
%}

%union {
  char* sval;
  int ival;
  float fval;

  struct{
    int first;
    int second;
  }pair_int;

  struct{
    union{
      int ival;//1
      float fval;//2
    }val;
    int type;
  }float_int;
  
}

%token VAR INTEGER BOOLEAN CHAR REAL PROGRAM BEGIN_STMT END_STMT WRITE READ IF ELSE THEN AND OR NOT WHILE DO DOWNTO TO FOR ARRAY OF  DIV
%token<ival> INTEGER_CONST
%token<fval> FLOAT_CONST
%token<sval> TEXT VAR_ID ARR_ID

%type<ival> var_type 
%type<pair_int>var_ids next_var variable
%type<float_int> arithmetic_statement number_const expression_statement 

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

var_line : var_ids ':' var_type ';'  var_line {
      for(int i=$<pair_int.first>1;i<$<pair_int.second>1;i++)
      {
        symbol_table[i].type=$3;
        symbol_table[i].arr_flag=0;
      }
    }
  | VAR_ID  ':' var_type ';'  var_line {
      check_var_id($1);
      symbol_table[end_pos].id=$1;
      symbol_table[end_pos].type=$3;
      symbol_table[end_pos].arr_flag=0;
      end_pos++;
    }
  | VAR_ID ':' ARRAY '[' INTEGER_CONST '.' '.' INTEGER_CONST ']' OF var_type ';' var_line {
      check_var_id($1);
      symbol_table[end_pos].id=$1;
      symbol_table[end_pos].arr_flag=1;
      symbol_table[end_pos].type=$11;
      symbol_table[end_pos].start=$5;
      symbol_table[end_pos].end=$8;
      assign_arr_space();
      end_pos++;
    }
  | 
  ;

var_ids : VAR_ID ','  VAR_ID  next_var {
    
    check_var_id($1);
    symbol_table[end_pos].id=$1;
    end_pos++;
    check_var_id($3);
    symbol_table[end_pos].id=$3;
    end_pos++;
    $<pair_int.first>$=$<pair_int.first>4;
    $<pair_int.second>$=end_pos;
  } ;

next_var:  ',' VAR_ID next_var {
      check_var_id($2);
      symbol_table[end_pos].id=$2;
      end_pos++;
      $<pair_int.first>$=$<pair_int.first>3;
    }
  | {$<pair_int.first>$=end_pos;};

var_type : INTEGER {$$=1;}
  | REAL {$$=2;}
  | CHAR {$$=3;}
  | BOOLEAN {$$=4;}
  ;



program_block :BEGIN_STMT block_body END_STMT ;

block_body: program_statement ';' block_body |  ;

program_statement : WRITE '(' print_statement ')' | READ '(' scan_statement ')' | assignment_statement | conditional_statement | loop_statement;

print_statement : print_variable
  | TEXT print_statement 
  |
  ;

print_variable : variable ',' print_variable | variable ;

scan_statement : variable ;

assignment_statement: variable ':' '=' expression_statement {
      int pos=$<pair_int.first>1;
      if(symbol_table[pos].arr_flag)
      {
        
      }
      else
      {
        if(symbol_table[pos].type==1 && $<float_int.type>4==1)
        {
          symbol_table[pos].value.ival=$<float_int.val.ival>4;
        }
        else if(symbol_table[pos].type==3 && $<float_int.type>4==3)
        {
          symbol_table[pos].value.fval=$<float_int.val.fval>4;
        }
        else
        {
          printf("type mismatch");
          exit(0);
        }
      }
    }
  | variable ':' '=' TEXT {if(strlen($4)>1) yyerror();}
  ;

expression_statement:  arithmetic_statement {$$=$1;}
 // | logical_expression {$<float_int.type>$=4;$<float_int.val.ival>$=$1;}
  ;

variable :  VAR_ID {$<pair_int.first>$=find_pos_id($1);}
  | ARR_ID '[' arithmetic_statement ']' {
      int temp=find_pos_id($1);
      $<pair_int.first>$=temp;
      if($<float_int.type>3==1)
      {
        int ind=$<float_int.val.ival>3;
        check_bounds(temp,ind);
        $<pair_int.second>$=ind; 
      }
      else
      {
        printf("compilation error , only int values allowed as array index");
        exit(0);
      }
    }
  |'(' variable ')' {$$=$2;}
  ;

 
arithmetic_statement: arithmetic_statement '+' arithmetic_statement {
      if($<float_int.type>1==1 && $<float_int.type>3==1)
      {
        $<float_int.type>$=1;
        $<float_int.val.ival>$=$<float_int.val.ival>1+$<float_int.val.ival>3;
      }
      else if($<float_int.type>1==2 && $<float_int.type>3==2)
      {
        $<float_int.type>$=2;
        $<float_int.val.fval>$=$<float_int.val.fval>1+$<float_int.val.fval>3;
      }
      else
      {
        printf("type mismatch");
        exit(0);
      }
    } 
  | arithmetic_statement '*' arithmetic_statement {
      if($<float_int.type>1==1 && $<float_int.type>3==1)
      {
        $<float_int.type>$=1;
        $<float_int.val.ival>$=$<float_int.val.ival>1*$<float_int.val.ival>3;
      }
      else if($<float_int.type>1==2 && $<float_int.type>3==2)
      {
        $<float_int.type>$=2;
        $<float_int.val.fval>$=$<float_int.val.fval>1*$<float_int.val.fval>3;
      }
      else
      {
        printf("type mismatch");
        exit(0);
      }
    } 
  | arithmetic_statement '-' arithmetic_statement {
      if($<float_int.type>1==1 && $<float_int.type>3==1)
      {
        $<float_int.type>$=1;
        $<float_int.val.ival>$=$<float_int.val.ival>1-$<float_int.val.ival>3;
      }
      else if($<float_int.type>1==2 && $<float_int.type>3==2)
      {
        $<float_int.type>$=2;
        $<float_int.val.fval>$=$<float_int.val.fval>1-$<float_int.val.fval>3;
      }
      else
      {
        printf("type mismatch");
        exit(0);
      }
    } 
  | arithmetic_statement '/' arithmetic_statement {
      $<float_int.type>$=2;
      float num1,num2;
      if($<float_int.type>1==1 )
      {
        num1=(float)$<float_int.val.ival>1;
      }
      else
      {
        num1=$<float_int.val.fval>1;
      }

      if($<float_int.type>3==1)
      {
        num2=(float)$<float_int.val.ival>3;
      }
      else
      {
        num2=$<float_int.val.fval>3;
      }
      $<float_int.val.fval>$=num1/num2;
    } 
  | arithmetic_statement DIV arithmetic_statement {
      if($<float_int.type>1==1 && $<float_int.type>3==1)
      {
        $<float_int.type>$=1;
        $<float_int.val.ival>$=$<float_int.val.ival>1/$<float_int.val.ival>3;
      }
      else
      {
        printf("DIV can only be performed using int");
        exit(0);
      }
    }
  | arithmetic_statement '%' arithmetic_statement {
      if($<float_int.type>1==1 && $<float_int.type>3==1)
      {
        $<float_int.type>$=1;
        $<float_int.val.ival>$=$<float_int.val.ival>1%$<float_int.val.ival>3;
      }
      else
      {
        printf("mod can only be performed using int");
        exit(0);
      }
    }
  | '(' arithmetic_statement ')' {$$=$2;}
  | '+' '(' arithmetic_statement ')' {$$=$3;}
  | '-'  '(' arithmetic_statement ')' {
      if($<float_int.type>3==1 )
      {
        $<float_int.type>$=1;
        $<float_int.val.ival>$=-$<float_int.val.ival>3;
      }
      else 
      {
        $<float_int.type>$=2;
        $<float_int.val.fval>$=-$<float_int.val.fval>3;
      }
    }
  | variable {
      int pos=$<pair_int.first>1;
      int type=symbol_table[pos].type;
      $<float_int.type>$=type;
      switch(type)
      {
        case 1:
          if(symbol_table[pos].arr_flag)
          {
            $<float_int.val.ival>$=symbol_table[pos].value.ival_ptr[$<pair_int.second>1-symbol_table[pos].start];
          }
          else
          {
            $<float_int.val.ival>$=symbol_table[pos].value.ival;
          }
          break;

        case 2:
          if(symbol_table[pos].arr_flag)
          {
            $<float_int.val.fval>$=symbol_table[pos].value.fval_ptr[$<pair_int.second>1-symbol_table[pos].start];
          }
          else
          {
            $<float_int.val.fval>$=symbol_table[pos].value.fval;
          }
          break;

        default:
          printf("error: type can be only float or int value");
          exit(0);
      }
    }
  | '-' variable {
      int pos=$<pair_int.first>2;
      int type=symbol_table[pos].type;
      $<float_int.type>$=type;
      switch(type)
      {
        case 1:
          if(symbol_table[pos].arr_flag)
          {
            $<float_int.val.ival>$=-symbol_table[pos].value.ival_ptr[$<pair_int.second>2-symbol_table[pos].start];
          }
          else
          {
            $<float_int.val.ival>$=-symbol_table[pos].value.ival;
          }
          break;

        case 2:
          if(symbol_table[pos].arr_flag)
          {
            $<float_int.val.fval>$=-symbol_table[pos].value.fval_ptr[$<pair_int.second>2-symbol_table[pos].start];
          }
          else
          {
            $<float_int.val.fval>$=-symbol_table[pos].value.fval;
          }
          break;

        default:
          printf("error: type can be only float or int value");
          exit(0);
      }
    }
  | number_const {$$=$1;}
  | '-' number_const {
    $$=$2;
    if($<float_int.type>$==1)
      $<float_int.val.ival>$=-$<float_int.val.ival>$;
    else
      $<float_int.val.fval>$=-$<float_int.val.fval>$;
  }
  ; 

number_const: INTEGER_CONST {$<float_int.type>$=1;$<float_int.val.ival>$=$1;}
  | FLOAT_CONST {$<float_int.type>$=2;$<float_int.val.fval>$=$1;}
  ;



conditional_statement :IF logical_expression_helper THEN program_block ELSE program_block 
  | IF logical_expression_helper THEN program_block
  ;

logical_expression_helper: variable | logical_expression;

logical_expression : NOT logical_expression
  | '(' logical_expression ')'
  | NOT variable
  |arithmetic_statement '>' arithmetic_statement
  | arithmetic_statement '>' '=' arithmetic_statement
  | arithmetic_statement '<' '=' arithmetic_statement
  | arithmetic_statement '<' arithmetic_statement
  | arithmetic_statement '=' arithmetic_statement
  | arithmetic_statement '<' '>' arithmetic_statement
  | logical_expression AND logical_expression
  | logical_expression OR logical_expression
  | variable AND variable
  | logical_expression AND variable
  | variable AND logical_expression
  | variable OR logical_expression
  | variable OR variable
  | logical_expression OR variable
  ;

loop_statement : WHILE logical_expression_helper DO program_block
  | FOR assignment_statement TO arithmetic_statement DO program_block
  | FOR assignment_statement DOWNTO arithmetic_statement DO program_block
  ;

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
  for(int i=0;i<end_pos;i++)
  {
    printf("%s",symbol_table[i].id);
    printf("%d %d %d %d \n",symbol_table[i].type,symbol_table[i].value.ival,symbol_table[i].start,symbol_table[i].end);
  }
  printf("valid input\n");
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