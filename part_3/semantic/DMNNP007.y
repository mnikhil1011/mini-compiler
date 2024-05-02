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

  char*initial[1024];
  int top_initial=0;
  void add_initial(char*s)
  {
    initial[top_initial]=s;
    top_initial++;
  }
  
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
        printf("multiple declarations of  a variable %s\n",s);
        return;
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
    //printf("undeclared variable %s\n",s);
    return -1;

  }
  void find_initial(char*s)
  {
    int i=find_pos_id(s);
    if( i>=0 && symbol_table[i].arr_flag==1)
      return;
    for(int i=0;i<top_initial;i++)
    {
      if(strcmp(s,initial[i])==0)
      {
        return;
      }
    }
    printf("variable %s used before a value is set\n",s);
  }
  void check_bounds(int var ,int index)
  {
    if(!symbol_table[var].arr_flag)
    {
      printf("var_id:%s is not an array\n",symbol_table[var].id);
      return;
    }
    if(index>=symbol_table[var].start && index<=symbol_table[var].end)
      return;
    printf("index out of bounds var_id:%s index:%d bounds:%d to %d\n",symbol_table[var].id,index,symbol_table[var].start,symbol_table[var].end);
  }
  char* get_type(int a)
  {
    char*ret=malloc(50*sizeof(char));
    switch(a)
    {
      case 1:
        strcpy(ret,"integer");
        break;
      case 2:
        strcpy(ret,"real");
        break;
      case 3:
        strcpy(ret,"char");
        break;
      case 4:
        strcpy(ret,"bool");
        break;
      default:
        strcpy(ret,"underclared_var(no type)");
    }
    return ret;
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
      char cval;
    }val;
    int type;
  }float_int;

  
  
}

%token VAR INTEGER BOOLEAN CHAR REAL PROGRAM BEGIN_STMT END_STMT WRITE READ IF ELSE THEN AND OR NOT WHILE DO DOWNTO TO FOR ARRAY OF DIV TRUE_CONST FALSE_CONST
%token<ival> INTEGER_CONST
%token<fval> FLOAT_CONST
%token<sval> TEXT VAR_ID ARR_ID

%type<ival> var_type logical_expression logical_expression_helper
%type<pair_int>var_ids next_var variable
%type<float_int> arithmetic_statement number_const expression_statement assignment_statement

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
  | TEXT  //{printf("%s",$1);}
  |
  ;

print_variable : variable ','{int pos=$<pair_int.first>1;find_initial(symbol_table[pos].id);} print_variable | variable {int pos=$<pair_int.first>1;find_initial(symbol_table[pos].id);};

scan_statement : variable {int pos=$<pair_int.first>1;add_initial(symbol_table[pos].id);};

assignment_statement: variable ':' '=' expression_statement {
      $$=$4;
      int pos=$<pair_int.first>1;
      if(symbol_table[pos].arr_flag)
      {
        if(symbol_table[pos].type==1 && $<float_int.type>4==1)
        {
          symbol_table[pos].value.ival_ptr[$<pair_int.second>1-symbol_table[pos].start]=$<float_int.val.ival>4;
        }
        else if(symbol_table[pos].type==2 && $<float_int.type>4==2)
        {
          symbol_table[pos].value.fval_ptr[$<pair_int.second>1-symbol_table[pos].start]=$<float_int.val.fval>4;
        }
        else if(symbol_table[pos].type==3 && $<float_int.type>4==3)
        {
          symbol_table[pos].value.cval_ptr[$<pair_int.second>1-symbol_table[pos].start]=$<float_int.val.cval>4;
        }
        else if(symbol_table[pos].type==4 && $<float_int.type>4==4)
        {
          symbol_table[pos].value.bval_ptr[$<pair_int.second>1-symbol_table[pos].start]=$<float_int.val.ival>4;
        }
        else
        {
          printf("type mismatch %s := %s for var %s\n",get_type(symbol_table[pos].type),get_type($<float_int.type>4),symbol_table[pos].id);
        }
      }
      else
      {
        
        if(symbol_table[pos].type==1 && $<float_int.type>4==1)
        {
          symbol_table[pos].value.ival=$<float_int.val.ival>4;
          add_initial(symbol_table[pos].id);
        }
        else if(symbol_table[pos].type==2 && $<float_int.type>4==2)
        {
          symbol_table[pos].value.fval=$<float_int.val.fval>4;
          add_initial(symbol_table[pos].id);
        }
        else if(symbol_table[pos].type==3 && $<float_int.type>4==3)
        {
          symbol_table[pos].value.cval=$<float_int.val.cval>4;
          add_initial(symbol_table[pos].id);
        }
        else if(symbol_table[pos].type==4 && $<float_int.type>4==4)
        {
          symbol_table[pos].value.bval=$<float_int.val.ival>4;
          add_initial(symbol_table[pos].id);
        }
        else
        {
          printf("type mismatch %s := %s for var %s\n",get_type(symbol_table[pos].type),get_type($<float_int.type>4),symbol_table[pos].id);
        }
        
      }
    }
  | variable ':' '=' TEXT {
      int pos=$<pair_int.first>1;
      if(symbol_table[pos].arr_flag)
      {
         if(strlen($4)>1) 
          yyerror();
        if(symbol_table[pos].type==3)
        {
          symbol_table[pos].value.cval_ptr[$<pair_int.second>1-symbol_table[pos].start]=$4[0];
        }
        else
        {
          printf("type mismatch %s := char for var %s\n",get_type(symbol_table[pos].type),symbol_table[pos].id);
        }
      }
      else
      {
        if(strlen($4)>1) 
          yyerror();
        if(symbol_table[pos].type==3)
        {
          symbol_table[pos].value.cval=$4[0];
          add_initial(symbol_table[pos].id);
        }
        else
        {
          printf("type mismatch %s := char for var %s\n",get_type(symbol_table[pos].type),symbol_table[pos].id);
        }
      }
    }
  ;

expression_statement:  arithmetic_statement {$$=$1;}
 | logical_expression {$<float_int.type>$=4;$<float_int.val.ival>$=$1;}
  ;

variable :  VAR_ID {
    $<pair_int.first>$=find_pos_id($1);
    int pos=$<pair_int.first>$;
    if(symbol_table[pos].arr_flag)
    {
      printf("%s is an arrray , not a non-array-var\n",$1);
      $<pair_int.second>$=symbol_table[pos].start; 
    }
  }
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
        printf("compilation error , only int values allowed as array index for var %s\n",$1);
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
        printf("type mismatch %s + %s \n",get_type($<float_int.type>1),get_type($<float_int.type>3));
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
        printf("type mismatch %s * %s \n",get_type($<float_int.type>1),get_type($<float_int.type>3));
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
        printf("type mismatch %s - %s \n",get_type($<float_int.type>1),get_type($<float_int.type>3));
      }
    } 
  | arithmetic_statement '/' arithmetic_statement {
      $<float_int.type>$=2;
      float num1,num2;
      if($<float_int.type>1==1 )
      {
        num1=(float)$<float_int.val.ival>1;
      }
      else if($<float_int.type>1==2 )
      {
        num1=$<float_int.val.fval>1;
      }
      else
      {
        printf("type mismatch %s / %s \n",get_type($<float_int.type>1),get_type($<float_int.type>3));
      }

      if($<float_int.type>3==1)
      {
        num2=(float)$<float_int.val.ival>3;
      }
      else if($<float_int.type>3==2)
      {
        num2=$<float_int.val.fval>3;
      }
      else
      {
        printf("type mismatch %s / %s \n",get_type($<float_int.type>1),get_type($<float_int.type>3));
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
        printf("DIV can only be performed using int %s DIV %s \n",get_type($<float_int.type>1),get_type($<float_int.type>3));
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
        printf("mod can only be performed using int %s DIV %s \n",get_type($<float_int.type>1),get_type($<float_int.type>3));
      }
    }
  | '(' arithmetic_statement ')' {$$=$2;}
  | '+' '(' arithmetic_statement ')' {$$=$3;if($<float_int.type>3==4 || $<float_int.type>3==3) {printf("type mismatch\n");exit(0);}}
  | '-'  '(' arithmetic_statement ')' {
      if($<float_int.type>3==1 )
      {
        $<float_int.type>$=1;
        $<float_int.val.ival>$=-$<float_int.val.ival>3;
      }
      else if($<float_int.type>3==2)
      {
        $<float_int.type>$=2;
        $<float_int.val.fval>$=-$<float_int.val.fval>3;
      }
      else
      {
        printf("type mismatch only int/ float allowed\n");
      }
    }
  | variable {
      int pos=$<pair_int.first>1;
      int type=symbol_table[pos].type;
      find_initial(symbol_table[pos].id);
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
        
        case 3:
          if(symbol_table[pos].arr_flag)
          {
            $<float_int.val.cval>$=symbol_table[pos].value.cval_ptr[$<pair_int.second>1-symbol_table[pos].start];
          }
          else
          {
            $<float_int.val.cval>$=symbol_table[pos].value.cval;
          }
          break;
        
        case 4:
          if(symbol_table[pos].arr_flag)
          {
            $<float_int.val.ival>$=symbol_table[pos].value.bval_ptr[$<pair_int.second>1-symbol_table[pos].start];
          }
          else
          {
            $<float_int.val.ival>$=symbol_table[pos].value.bval;
          }
          break;

        default:
          printf("error: type can be only float or int value %s\n",symbol_table[pos].id);
      }
    }
  | '-' variable {
      int pos=$<pair_int.first>2;
      find_initial(symbol_table[pos].id);
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
          printf("error: type can be only float or int value %s\n",symbol_table[pos].id);
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

logical_expression_helper: variable {
      int pos=$<pair_int.first>1;
      find_initial(symbol_table[pos].id);
      int type=symbol_table[pos].type;
      int temp1;
      if(type==4)
      {
        if(symbol_table[pos].arr_flag)
        {
          temp1=symbol_table[pos].value.bval_ptr[$<pair_int.second>1-symbol_table[pos].start];
        }
        else
        {
          temp1=symbol_table[pos].value.bval;
        }
        $$=(temp1);
      }
      else
      {
        printf("error: type can be only bool %s\n",symbol_table[pos].id);
      }
    }
  |logical_expression{$$=$1;}
  ;

logical_expression : NOT logical_expression {$$=(!$2);}
  | '(' logical_expression ')'{$$=$2;}
  | NOT variable {
      int pos=$<pair_int.first>2;
      int type=symbol_table[pos].type;
      find_initial(symbol_table[pos].id);
      int temp1;
      if(type==4)
      {
        if(symbol_table[pos].arr_flag)
        {
          temp1=symbol_table[pos].value.bval_ptr[$<pair_int.second>2-symbol_table[pos].start];
        }
        else
        {
          temp1=symbol_table[pos].value.bval;
        }
        $$=(!temp1);
      }
      else
      {
        printf("error: type can be only bool NOT %s\n",symbol_table[pos].id);
      }
    }
  |arithmetic_statement '>' arithmetic_statement {
      if($<float_int.type>1==1 && $<float_int.type>3==1)
      {
        $$=($<float_int.val.ival>1 > $<float_int.val.ival>3);
      }
      else if($<float_int.type>1==2 && $<float_int.type>3==2)
      {
        $$=($<float_int.val.fval>1 > $<float_int.val.fval>3);
      }
      else if($<float_int.type>1==3 && $<float_int.type>3==3)
      {
        $$=($<float_int.val.cval>1 > $<float_int.val.cval>3);
      }
      else
      {
        printf("type mismatch %s > %s \n",get_type($<float_int.type>1),get_type($<float_int.type>3));
      }
    }

  | arithmetic_statement '>' '=' arithmetic_statement {
      if($<float_int.type>1==1 && $<float_int.type>4==1)
      {
        $$=($<float_int.val.ival>1 >=$<float_int.val.ival>4);
      }
      else if($<float_int.type>1==2 && $<float_int.type>4==2)
      {
        $$=($<float_int.val.fval>1 >=$<float_int.val.fval>4);
      }
      else if($<float_int.type>1==3 && $<float_int.type>4==3)
      {
        $$=($<float_int.val.cval>1 >=$<float_int.val.cval>4);
      }
      else
      {
        printf("type mismatch %s >= %s \n",get_type($<float_int.type>1),get_type($<float_int.type>4));
      }
    }
  | arithmetic_statement '<' '=' arithmetic_statement {
      if($<float_int.type>1==1 && $<float_int.type>4==1)
      {
        $$=($<float_int.val.ival>1<=$<float_int.val.ival>4);
      }
      else if($<float_int.type>1==2 && $<float_int.type>4==2)
      {
        $$=($<float_int.val.fval>1<=$<float_int.val.fval>4);
      }
      else if($<float_int.type>1==3 && $<float_int.type>4==3)
      {
        $$=($<float_int.val.cval>1<=$<float_int.val.cval>4);
      }
      else
      {
        printf("type mismatch %s <= %s \n",get_type($<float_int.type>1),get_type($<float_int.type>4));
      }
    }
  | arithmetic_statement '<' arithmetic_statement {
      if($<float_int.type>1==1 && $<float_int.type>3==1)
      {
        $$=($<float_int.val.ival>1 < $<float_int.val.ival>3);
      }
      else if($<float_int.type>1==2 && $<float_int.type>3==2)
      {
        $$=($<float_int.val.fval>1 < $<float_int.val.fval>3);
      }
      else if($<float_int.type>1==3 && $<float_int.type>3==3)
      {
        $$=($<float_int.val.cval>1 < $<float_int.val.cval>3);
      }
      else
      {
        printf("type mismatch %s < %s \n",get_type($<float_int.type>1),get_type($<float_int.type>3));
      }
    }
  | arithmetic_statement '=' arithmetic_statement {
      if($<float_int.type>1==1 && $<float_int.type>3==1)
      {
        $$=($<float_int.val.ival>1==$<float_int.val.ival>3);
      }
      else if($<float_int.type>1==2 && $<float_int.type>3==2)
      {
        $$=($<float_int.val.fval>1==$<float_int.val.fval>3);
      }
      else if($<float_int.type>1==3 && $<float_int.type>3==3)
      {
        $$=($<float_int.val.cval>1==$<float_int.val.cval>3);
      }
      else
      {
        printf("type mismatch %s == %s \n",get_type($<float_int.type>1),get_type($<float_int.type>3));
      }
    }
  | arithmetic_statement '<' '>' arithmetic_statement {
      if($<float_int.type>1==1 && $<float_int.type>4==1)
      {
        $$=($<float_int.val.ival>1!=$<float_int.val.ival>4);
      }
      else if($<float_int.type>1==2 && $<float_int.type>4==2)
      {
        $$=($<float_int.val.fval>1!=$<float_int.val.fval>4);
      }
      else if($<float_int.type>1==3 && $<float_int.type>4==3)
      {
        $$=($<float_int.val.cval>1!=$<float_int.val.cval>4);
      }
      else
      {
        printf("type mismatch %s <> %s \n",get_type($<float_int.type>1),get_type($<float_int.type>4));
      }
    }
  | logical_expression AND logical_expression {$$=$1&$3;}
  | logical_expression OR logical_expression {$$=$1|$3;}
  | variable AND variable {
    int pos=$<pair_int.first>1;
    int type=symbol_table[pos].type;
    find_initial(symbol_table[pos].id);
    int temp1;
    if(type==4)
    {
      if(symbol_table[pos].arr_flag)
      {
        temp1=symbol_table[pos].value.bval_ptr[$<pair_int.second>1-symbol_table[pos].start];
      }
      else
      {
        temp1=symbol_table[pos].value.bval;
      }
    }
    else
    {
      printf("error: type can be only bool 'AND' %s\n",symbol_table[pos].id);
    }

    pos=$<pair_int.first>3;
    find_initial(symbol_table[pos].id);
    type=symbol_table[pos].type;
    int temp2;
    if(type==4)
    {
      if(symbol_table[pos].arr_flag)
      {
        temp2=symbol_table[pos].value.bval_ptr[$<pair_int.second>3-symbol_table[pos].start];
      }
      else
      {
        temp2=symbol_table[pos].value.bval;
      }
    }
    else
    {
      printf("error: type can be only bool 'AND' %s\n",symbol_table[pos].id);
    }
    $$=temp1 & temp2;
    }
  | logical_expression AND variable {
    int pos=$<pair_int.first>3;
    find_initial(symbol_table[pos].id);
    int type=symbol_table[pos].type;
    int temp1;
    if(type==4)
    {
      if(symbol_table[pos].arr_flag)
      {
        temp1=symbol_table[pos].value.bval_ptr[$<pair_int.second>3-symbol_table[pos].start];
      }
      else
      {
        temp1=symbol_table[pos].value.bval;
      }
    }
    else
    {
      printf("error: type can be only bool 'AND'' %s\n",symbol_table[pos].id);
    }
    $$=$1&temp1;
    }
  | variable AND logical_expression{
    int pos=$<pair_int.first>1;
    find_initial(symbol_table[pos].id);
    int type=symbol_table[pos].type;
    int temp1;
    if(type==4)
    {
      if(symbol_table[pos].arr_flag)
      {
        temp1=symbol_table[pos].value.bval_ptr[$<pair_int.second>1-symbol_table[pos].start];
      }
      else
      {
        temp1=symbol_table[pos].value.bval;
      }
    }
    else
    {
      printf("error: type can be only bool 'AND' %s\n",symbol_table[pos].id);
    }
    $$=temp1 & $3;
    }
  | variable OR logical_expression {
    int pos=$<pair_int.first>1;
    find_initial(symbol_table[pos].id);
    int type=symbol_table[pos].type;
    int temp1;
    if(type==4)
    {
      if(symbol_table[pos].arr_flag)
      {
        temp1=symbol_table[pos].value.bval_ptr[$<pair_int.second>1-symbol_table[pos].start];
      }
      else
      {
        temp1=symbol_table[pos].value.bval;
      }
    }
    else
    {
      printf("error: type can be only bool 'OR' %s\n",symbol_table[pos].id);
    }
    $$=temp1|$3;
    }
  | variable OR variable {
    int pos=$<pair_int.first>1;
    find_initial(symbol_table[pos].id);
    int type=symbol_table[pos].type;
    int temp1;
    if(type==4)
    {
      if(symbol_table[pos].arr_flag)
      {
        temp1=symbol_table[pos].value.bval_ptr[$<pair_int.second>1-symbol_table[pos].start];
      }
      else
      {
        temp1=symbol_table[pos].value.bval;
      }
    }
    else
    {
      printf("error: type can be only bool 'OR' %s\n",symbol_table[pos].id);
    }

    pos=$<pair_int.first>3;
    find_initial(symbol_table[pos].id);
    type=symbol_table[pos].type;
    int temp2;
    if(type==4)
    {
      if(symbol_table[pos].arr_flag)
      {
        temp2=symbol_table[pos].value.bval_ptr[$<pair_int.second>3-symbol_table[pos].start];
      }
      else
      {
        temp2=symbol_table[pos].value.bval;
      }
    }
    else
    {
      printf("error: type can be only bool 'OR' %s\n",symbol_table[pos].id);
    }
    $$=temp1|temp2;
    }
  | logical_expression OR variable {
    int pos=$<pair_int.first>3;
    find_initial(symbol_table[pos].id);
    int type=symbol_table[pos].type;
    int temp1;
    if(type==4)
    {
      if(symbol_table[pos].arr_flag)
      {
        temp1=symbol_table[pos].value.bval_ptr[$<pair_int.second>3-symbol_table[pos].start];
      }
      else
      {
        temp1=symbol_table[pos].value.bval;
      }
    }
    else
    {
      printf("error: type can be only bool 'OR' %s\n",symbol_table[pos].id);
    }
    $$=$1|temp1;
    }
  | TRUE_CONST {$$=1;}
  | FALSE_CONST {$$=0;}
  ;

// changed this
loop_statement : WHILE logical_expression_helper DO program_block 
  | FOR assignment_statement TO arithmetic_statement DO program_block {
      if($<float_int.type>4!=1 || $<float_int.type>2!=1 )
      {
        printf("only int allowed in loop; type mismatch\n");
      }
    }
  | FOR assignment_statement DOWNTO arithmetic_statement DO program_block {
      if($<float_int.type>4!=1 || $<float_int.type>2!=1 )
      {
        printf("only int allowed in loop; type mismatch\n");
      }
    }
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
  /* for(int i=0;i<end_pos;i++)
  {
    printf("%s",symbol_table[i].id);
    printf("%d  val:%c:  %d %d \n",symbol_table[i].type,symbol_table[i].value.cval,symbol_table[i].start,symbol_table[i].end);
  }
  printf("valid input\n"); */
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