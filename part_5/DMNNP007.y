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
      int bval;
      //for arr
      int * ival_ptr;
      float * fval_ptr;
      char * cval_ptr;
      int * bval_ptr;
    }value;
    
  }symbol_table[1024];

  char* getType(int i)
  {
    char*retval=malloc(sizeof(char)*32);
    switch(i)
    {
      case 1:
        strcpy(retval,"INTEGER");
        break;
      case 2:
        strcpy(retval,"REAL");
        break;
      case 3:
        strcpy(retval,"CHAR");
        break;
      case 4:
        strcpy(retval,"BOOLEAN");
        break;
    }
    return retval;
  }

  int end_pos=0,n=0;

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
        symbol_table[end_pos].value.bval_ptr=(int*)malloc((symbol_table[end_pos].end-symbol_table[end_pos].start+1)*sizeof(int));
        break;

    }
  }

  int find_var(char* s)
  {
    for(int i=0;i<end_pos;i++)
    {
      if(strcmp(s,symbol_table[i].id)==0)
      {
        return i;
      }
    }
    return -1;
    
  }
  int make_var(char*s,int type)
  {
    symbol_table[end_pos].id=s;
    symbol_table[end_pos].type=type;
    symbol_table[end_pos].arr_flag=0;
    end_pos++;
    return end_pos-1;
  }
  
  // part 4 from here
  int temp_var_num=0,temp_lab_num=0;
  struct TAC{
    char *s1;
    char *op;
    char *s2;
    char *s3;
  }tac[8224];
  int line_num=0;

  struct{
    char* s;
    int line;
  }goto_line[1024];
  int top=0;

  int find_goto_line(char*s)
  {
    for(int i=0;i<top;i++)
    {
      if(strcmp(s,goto_line[i].s)==0)
      {
        //printf("  ret %d ret  ",i);
        return goto_line[i].line;
        
      }
      //printf("%d",i);
        
    } 
    // printf("100  ");
    // return 100;
    exit(0);
    return -1;
  }

  struct{
    union{
      int ival;//1
      float fval;//2
      char cval;
    }val;
    int type;
  }float_int;


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
%type<ival>loop_statement conditional_statement for_count var_type
%type<pair_int>var_ids next_var

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


var_structure : VAR  var_line {n=end_pos;};

var_line : var_ids ':' var_type ';'  var_line {
      for(int i=$<pair_int.first>1;i<$<pair_int.second>1;i++)
      {
        symbol_table[i].type=$3;
        symbol_table[i].arr_flag=0;
      }
    }
  | VAR_ID  ':' var_type ';'  var_line {
      symbol_table[end_pos].id=$1;
      symbol_table[end_pos].type=$3;
      symbol_table[end_pos].arr_flag=0;
      end_pos++;
    }
  | VAR_ID ':' ARRAY '[' INTEGER_CONST '.' '.' INTEGER_CONST ']' OF var_type ';' var_line {
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
    
    symbol_table[end_pos].id=$1;
    end_pos++;
    symbol_table[end_pos].id=$3;
    end_pos++;
    $<pair_int.first>$=$<pair_int.first>4;
    $<pair_int.second>$=end_pos;
  } ;

next_var:  ',' VAR_ID next_var {
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

print_statement : variable {
    tac[line_num].s1=$1;
    tac[line_num].op=malloc(sizeof(char)*16);
    sprintf(tac[line_num].op,"print_1");
    line_num++;
    }  print_variable
  | TEXT  {
  tac[line_num].s1=$1;
  tac[line_num].op=malloc(sizeof(char)*16);
  sprintf(tac[line_num].op,"print_2");
  line_num++;
  } 
  ;

print_variable :',' variable {

  tac[line_num].s1=$2;
  tac[line_num].op=malloc(sizeof(char)*16);
  sprintf(tac[line_num].op,"print_1");
  line_num++;

  }  print_variable | ;

scan_statement : variable {

  tac[line_num].s1=$1;
  tac[line_num].op=malloc(sizeof(char)*16);
  sprintf(tac[line_num].op,"scan");
  line_num++;

  } 
  ;

assignment_statement: variable ':' '=' expression_statement {
      //printf("%s = %s\n",$1,$4);
      tac[line_num].s1=$1;
      tac[line_num].s2=$4;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,":=");
      line_num++;
      $$=$1;
    }
  | variable ':' '=' TEXT {
      //printf("%s = %s\n",$1,$4);
      tac[line_num].s1=$1;
      tac[line_num].s2=$4;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,":=c");
      
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
      sprintf(temp,"_t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      //printf("%s = %s + %s\n",$$,$1,$3);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].s3=$3;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"+");
      line_num++;
    }
  | arithmetic_statement '*' arithmetic_statement {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"_t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      //printf("%s = %s * %s\n",$$,$1,$3);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].s3=$3;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"*");
      line_num++;
    }
  | arithmetic_statement '-' arithmetic_statement {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"_t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      // printf("%s = %s - %s\n",$$,$1,$3);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].s3=$3;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"-");
      line_num++;
    }
  | arithmetic_statement '/' arithmetic_statement {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"_t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      // printf("%s = %s / %s\n",$$,$1,$3);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].s3=$3;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"/");
      line_num++;
    }
  | arithmetic_statement DIV arithmetic_statement {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"_t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      // printf("%s = %s DIV %s\n",$$,$1,$3);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].s3=$3;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"DIV");
      line_num++;
    }
  | arithmetic_statement '%' arithmetic_statement {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"_t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      // printf("%s = %s %% %s\n",$$,$1,$3);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].s3=$3;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"%%");
      line_num++;
    }
  | '(' arithmetic_statement ')' {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"_t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      // printf("%s = %s\n",$$,$2);
      tac[line_num].s1=$$;
      tac[line_num].s2=$2;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,":=");
      line_num++;
    }
  | '+' '(' arithmetic_statement ')' {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"_t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      // printf("%s = %s\n",$$,$3);
      tac[line_num].s1=$$;
      tac[line_num].s2=$3;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,":=");
      line_num++;
    }
  | '-'  '(' arithmetic_statement ')' {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"_t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      // printf("%s = -%s\n",$$,$3);
      tac[line_num].s1=$$;
      tac[line_num].s2=$3;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,":-");


      line_num++;
    }
  | variable {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"_t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      // printf("%s = %s\n",$$,$1);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,":=");
      line_num++;
    }
  | '-' variable {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"_t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      // printf("%s = -%s\n",$$,$2);
      tac[line_num].s1=$$;
      tac[line_num].s2=$2;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,":-");
      
      line_num++;
    }
  | number_const {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"_t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      if($<float_int.type>1==1 || $<float_int.type>1==4)
      {
        // printf("%s = %d\n",$$,$<float_int.val.ival>1);
        tac[line_num].s1=$$;
        tac[line_num].s2=malloc(sizeof(char)*16);
        tac[line_num].op=malloc(sizeof(char)*16);
        tac[line_num].s3=malloc(sizeof(char)*16);
        sprintf(tac[line_num].s3,"%d",$<float_int.type>1);
        //if($<float_int.type>1==1)
          sprintf(tac[line_num].op,":=i");
        // else
        //   sprintf(tac[line_num].op,":=b");


        sprintf(tac[line_num].s2,"%d",$<float_int.val.ival>1);
        line_num++;
      }
      else if( $<float_int.type>1==2)
      {
        // printf("%s = %f\n",$$,$<float_int.val.fval>1);
        tac[line_num].s1=$$;
        tac[line_num].s2=malloc(sizeof(char)*16);;
        tac[line_num].op=malloc(sizeof(char)*16);
        tac[line_num].s3=malloc(sizeof(char)*16);
        sprintf(tac[line_num].s3,"%d",$<float_int.type>1);
        sprintf(tac[line_num].op,":=f");


        sprintf(tac[line_num].s2,"%f",$<float_int.val.fval>1);
        line_num++;
      }
      else
      {
        // printf("%s = %c\n",$$,$<float_int.val.cval>1);
        tac[line_num].s1=$$;
        tac[line_num].s2=malloc(sizeof(char)*16);;
        tac[line_num].op=malloc(sizeof(char)*16);
        tac[line_num].s3=malloc(sizeof(char)*16);
        sprintf(tac[line_num].s3,"%d",$<float_int.type>1);
        sprintf(tac[line_num].op,":=c");
        // symbol_table[end_pos].id=$$;
        // symbol_table[end_pos].type=$<float_int.type>1;
        // symbol_table[end_pos].arr_flag=0;
        // end_pos++;
        sprintf(tac[line_num].s2,"%c",$<float_int.val.cval>1);
        line_num++;
      }
    }
  | '-' number_const {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"_t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      if($<float_int.type>2==1 )
      {
        // printf("%s = -%d\n",$$,$<float_int.val.ival>2);
        tac[line_num].s1=$$;
        tac[line_num].s2=malloc(sizeof(char)*16);;
        tac[line_num].op=malloc(sizeof(char)*16);
        sprintf(tac[line_num].op,":-i");

        // symbol_table[end_pos].id=$$;
        // symbol_table[end_pos].type=$<float_int.type>2;
        // symbol_table[end_pos].arr_flag=0;
        // end_pos++;

        sprintf(tac[line_num].s2,"%d",$<float_int.val.ival>2);
        tac[line_num].s3=malloc(sizeof(char)*16);
        sprintf(tac[line_num].s3,"%d",$<float_int.type>2);
        line_num++;
      }
      else if($<float_int.type>2==2)
      {
        // printf("%s = -%f\n",$$,$<float_int.val.fval>2);
        tac[line_num].s1=$$;
        tac[line_num].s2=malloc(sizeof(char)*16);
        tac[line_num].op=malloc(sizeof(char)*16);
        tac[line_num].s3=malloc(sizeof(char)*16);
        sprintf(tac[line_num].s3,"%d",$<float_int.type>2);
        sprintf(tac[line_num].op,":-f");

        // symbol_table[end_pos].id=$$;
        // symbol_table[end_pos].type=$<float_int.type>2;
        // symbol_table[end_pos].arr_flag=0;
        // end_pos++;

        sprintf(tac[line_num].s2,"%f",$<float_int.val.fval>2);
        line_num++;
      }
    }
  ; 

number_const: INTEGER_CONST { $<float_int.type>$=1;$<float_int.val.ival>$=$1;}
  | FLOAT_CONST {$<float_int.type>$=2;$<float_int.val.fval>$=$1;}
  ;



conditional_statement :IF logical_expression_helper THEN {

      // printf("if %s goto l%d\n",$2,temp_lab_num);
      tac[line_num].s1=$2;
      tac[line_num].s2=malloc(sizeof(char)*16);
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"IF");
      sprintf(tac[line_num].s2,"l%d",temp_lab_num);
      line_num++;
      $3=temp_lab_num+1;
      // printf("goto l%d\n",$3);
      tac[line_num].s2=malloc(sizeof(char)*16);
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"GOTO");
      sprintf(tac[line_num].s2,"l%d",$3);
      line_num++;

      // printf("l%d:\n",temp_lab_num);
      
      tac[line_num].s2=malloc(sizeof(char)*16);
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"LAB");
      sprintf(tac[line_num].s2,"l%d",temp_lab_num);
      goto_line[top].s=tac[line_num].s2;
      goto_line[top].line=line_num;
      top++;
      line_num++;
      temp_lab_num+=2;


} program_block{

      
      // printf("goto l%d\n",temp_lab_num);
      tac[line_num].s2=malloc(sizeof(char)*16);
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"GOTO");
      sprintf(tac[line_num].s2,"l%d",temp_lab_num);
      line_num++;


      // printf("l%d:\n",$3);
      tac[line_num].s2=malloc(sizeof(char)*16);
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"LAB");
      sprintf(tac[line_num].s2,"l%d",$3);
      goto_line[top].s=tac[line_num].s2;
      goto_line[top].line=line_num;
      top++;
      line_num++;

      $3=temp_lab_num;
      temp_lab_num++;

} maybe_else_block {
      // printf("l%d:\n",$3);
      tac[line_num].s2=malloc(sizeof(char)*16);
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"LAB");
      sprintf(tac[line_num].s2,"l%d",$3);
      goto_line[top].s=tac[line_num].s2;
      goto_line[top].line=line_num;
      top++;
      line_num++;
}
  ;
maybe_else_block : ELSE program_block 
  | 
  ;

logical_expression_helper: variable {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"_t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      // printf("%s = %s\n",$$,$1);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,":=");
      line_num++;
    } 
  |logical_expression {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"_t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      // printf("%s = %s\n",$$,$1);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,":=");


      line_num++;
    }
  ;

logical_expression : NOT logical_expression {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"_t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      // printf("%s = NOT %s\n",$$,$2);
      tac[line_num].s1=$$;
      tac[line_num].s2=$2;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"NOT");
      line_num++;
    } 
  | '(' logical_expression ')' {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"_t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      // printf("%s = %s\n",$$,$2);
      tac[line_num].s1=$$;
      tac[line_num].s2=$2;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,":=");

      line_num++;
    } 
  | NOT variable {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"_t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      // printf("%s = NOT %s\n",$$,$2);
      tac[line_num].s1=$$;
      tac[line_num].s2=$2;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"NOT");
      line_num++;
    } 
  | arithmetic_statement '>' arithmetic_statement {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"_t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      // printf("%s = %s > %s\n",$$,$1,$3);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].s3=$3;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,">");
      line_num++;
    }
  | arithmetic_statement '>' '=' arithmetic_statement {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"_t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      // printf("%s = %s >= %s\n",$$,$1,$4);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].s3=$4;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,">=");
      line_num++;
    }
  | arithmetic_statement '<' '=' arithmetic_statement {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"_t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      // printf("%s = %s <= %s\n",$$,$1,$4);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].s3=$4;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"<=");
      line_num++;
    }
  | arithmetic_statement '<' arithmetic_statement {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"_t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      // printf("%s = %s < %s\n",$$,$1,$3);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].s3=$3;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"<");
      line_num++;
    }
  | arithmetic_statement '=' arithmetic_statement {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"_t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      // printf("%s = %s == %s\n",$$,$1,$3);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].s3=$3;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"==");
      line_num++;
    }
  | arithmetic_statement '<' '>' arithmetic_statement {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"_t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      // printf("%s = %s != %s\n",$$,$1,$4);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].s3=$4;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"!=");
      line_num++;
    }
  | logical_expression AND logical_expression {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"_t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      // printf("%s = %s AND %s\n",$$,$1,$3);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].s3=$3;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"AND");
      line_num++;
    }
  | logical_expression OR logical_expression {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"_t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      // printf("%s = %s OR %s\n",$$,$1,$3);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].s3=$3;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"OR");
      line_num++;
    }
  | variable AND variable {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"_t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      // printf("%s = %s AND %s\n",$$,$1,$3);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].s3=$3;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"AND");
      line_num++;
    }
  | logical_expression AND variable {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"_t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      // printf("%s = %s AND %s\n",$$,$1,$3);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].s3=$3;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"AND");
      line_num++;
    }
  | variable AND logical_expression {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"_t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      // printf("%s = %s AND %s\n",$$,$1,$3);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].s3=$3;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"AND");
      line_num++;
    }
  | variable OR logical_expression {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"_t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      // printf("%s = %s OR %s\n",$$,$1,$3);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].s3=$3;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"OR");
      line_num++;
    }
  | variable OR variable {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"_t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      // printf("%s = %s OR %s\n",$$,$1,$3);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].s3=$3;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"OR");
      line_num++;
    }
  | logical_expression OR variable {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"_t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      // printf("%s = %s OR %s\n",$$,$1,$3);
      tac[line_num].s1=$$;
      tac[line_num].s2=$1;
      tac[line_num].s3=$3;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"OR");
      line_num++;
    }
  | TRUE_CONST {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"_t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      // printf("%s = TRUE \n",$$);
      tac[line_num].s1=$$;
      tac[line_num].s2=malloc(sizeof(char)*16);;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,":=b");
      sprintf(tac[line_num].s2,"TRUE");
      line_num++;

      // symbol_table[end_pos].id=$$;
      // symbol_table[end_pos].type=4;
      // symbol_table[end_pos].arr_flag=0;
      // end_pos++;

    }
  | FALSE_CONST {
      char*temp=malloc(sizeof(char)*1024);
      sprintf(temp,"_t%d",temp_var_num);
      temp_var_num++;
      $$=temp;
      // printf("%s = FALSE \n",$$);
      tac[line_num].s1=$$;
      tac[line_num].s2=malloc(sizeof(char)*16);;
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,":=b");
      sprintf(tac[line_num].s2,"FALSE");
      line_num++;


      // symbol_table[end_pos].id=$$;
      // symbol_table[end_pos].type=4;
      // symbol_table[end_pos].arr_flag=0;
      // end_pos++;
    } 
  ;

loop_statement : WHILE {

      $1=temp_lab_num;
      temp_lab_num++;
      // printf("l%d:\n",$1);
      tac[line_num].s2=malloc(sizeof(char)*16);
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"LAB");
      sprintf(tac[line_num].s2,"l%d",$1);
      goto_line[top].s=tac[line_num].s2;
      goto_line[top].line=line_num;
      top++;
      line_num++;    

  } logical_expression_helper DO {  

      // printf("if %s goto l%d\n",$3,temp_lab_num);
      tac[line_num].s1=$3;
      tac[line_num].s2=malloc(sizeof(char)*16);
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"IF");
      sprintf(tac[line_num].s2,"l%d",temp_lab_num);
      line_num++;
      $4=temp_lab_num+1;
      // printf("goto l%d\n",$4);
      tac[line_num].s2=malloc(sizeof(char)*16);
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"GOTO");
      sprintf(tac[line_num].s2,"l%d",$4);
      line_num++;

      // printf("l%d:\n",temp_lab_num);
      tac[line_num].s2=malloc(sizeof(char)*16);
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"LAB");
      sprintf(tac[line_num].s2,"l%d",temp_lab_num);
      goto_line[top].s=tac[line_num].s2;
      goto_line[top].line=line_num;
      top++;
      line_num++;
      temp_lab_num+=2;

    } program_block {
        // printf("goto l%d\n",$1);
        tac[line_num].s2=malloc(sizeof(char)*16);
        tac[line_num].op=malloc(sizeof(char)*16);
        sprintf(tac[line_num].op,"GOTO");
        sprintf(tac[line_num].s2,"l%d",$1);
        line_num++;

        // printf("l%d:\n",$4);
        tac[line_num].s2=malloc(sizeof(char)*16);
        tac[line_num].op=malloc(sizeof(char)*16);
        sprintf(tac[line_num].op,"LAB");
        sprintf(tac[line_num].s2,"l%d",$4);
        goto_line[top].s=tac[line_num].s2;
        goto_line[top].line=line_num;
        top++;
        line_num++;
      }
| FOR assignment_statement {
      $1=temp_lab_num;
      temp_lab_num++;
      // printf("l%d:\n",$1);
      tac[line_num].s2=malloc(sizeof(char)*16);
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"LAB");
      sprintf(tac[line_num].s2,"l%d",$1);
      goto_line[top].s=tac[line_num].s2;
      goto_line[top].line=line_num;
      top++;
      line_num++;  
  } for_count arithmetic_statement DO {

      

      // printf("if %s!=%s goto l%d\n",$2,$5,temp_lab_num);
      tac[line_num].s1=$2;
      tac[line_num].s2=malloc(sizeof(char)*16);
      tac[line_num].s3=$5;
      tac[line_num].op=malloc(sizeof(char)*16);
      if($4==1)
      { 
        sprintf(tac[line_num].op,"IF_2+");
      }
      else
      {
        sprintf(tac[line_num].op,"IF_2-");
      }
      sprintf(tac[line_num].s2,"l%d",temp_lab_num);
      line_num++;

      $6=temp_lab_num+1;
      // printf("goto l%d\n",$6);
      tac[line_num].s2=malloc(sizeof(char)*16);
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"GOTO");
      sprintf(tac[line_num].s2,"l%d",$6);
      line_num++;

      // printf("l%d:\n",temp_lab_num);
      tac[line_num].s2=malloc(sizeof(char)*16);
      tac[line_num].op=malloc(sizeof(char)*16);
      sprintf(tac[line_num].op,"LAB");
      sprintf(tac[line_num].s2,"l%d",temp_lab_num);
      goto_line[top].s=tac[line_num].s2;
      goto_line[top].line=line_num;
      top++;
      line_num++;
      temp_lab_num+=2;


      
    }program_block{
        
        tac[line_num].op=malloc(sizeof(char)*16);
        if($4==1)
        { 
          // printf("%s = %s + 1\n",$2,$2);
          sprintf(tac[line_num].op,"++");
        }
        else
        {
          // printf("%s = %s - 1\n",$2,$2);
          sprintf(tac[line_num].op,"--");
        }
        tac[line_num].s1=$2;
        tac[line_num].s2=$2;
        tac[line_num].s3=malloc(sizeof(char)*16);
        sprintf(tac[line_num].s3,"1");
        line_num++;

        // printf("goto l%d\n",$1);
        tac[line_num].s2=malloc(sizeof(char)*16);
        tac[line_num].op=malloc(sizeof(char)*16);
        sprintf(tac[line_num].op,"GOTO");
        sprintf(tac[line_num].s2,"l%d",$1);
        line_num++;

        // printf("l%d:\n",$6);
        tac[line_num].s2=malloc(sizeof(char)*16);
        tac[line_num].op=malloc(sizeof(char)*16);
        sprintf(tac[line_num].op,"LAB");
        sprintf(tac[line_num].s2,"l%d",$6);
        goto_line[top].s=tac[line_num].s2;
        goto_line[top].line=line_num;
        top++;
        line_num++;

        tac[line_num].op=malloc(sizeof(char)*16);
        if($4==1)
        { 
          // printf("%s = %s - 1\n",$2,$2);
          sprintf(tac[line_num].op,"--");
          
        }
        else
        {
          // printf("%s = %s + 1\n",$2,$2);
          sprintf(tac[line_num].op,"++");
        }
        tac[line_num].s1=$2;
        tac[line_num].s2=$2;
        tac[line_num].s3=malloc(sizeof(char)*16);
        sprintf(tac[line_num].s3,"1");
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

  /* printf("\n\n goto table \n\n");
  for(int i=0;i<top;i++)
  {
    printf("%s ",goto_line[i].s);
    printf("%d  \n",goto_line[i].line);
  } */
  
  //printf("\n solving\n");
  // till n we have normal vars 
  //int temp_table_top=0;
  for(int i=0;i<line_num;i++)
  {
    //every line based on operator
    if(strcmp(tac[i].op,":=")==0)
    {
      char *start_index = strchr(tac[i].s2, '[');
      if(start_index==NULL)
      {
        //int
        int s2=find_var(tac[i].s2);
        start_index = strchr(tac[i].s1, '[');
        if(start_index==NULL)
        {
          int s1=find_var(tac[i].s1);
          if(s1==-1)
            s1=make_var(tac[i].s1,symbol_table[s2].type);
          symbol_table[s1].value=symbol_table[s2].value;
        }
        else
        {
          char*new_var=malloc(128*sizeof(char));
          strcpy(new_var,tac[i].s1);
          char *end_index = strchr(tac[i].s1, ']');
          *end_index='\0';
          *start_index='\0';
          char* after_str=start_index+1;
          int s1=find_var(tac[i].s1);
          int index=find_var(after_str);
          if(symbol_table[s1].type==1)
            symbol_table[s1].value.ival_ptr[symbol_table[index].value.ival-symbol_table[s1].start]=symbol_table[s2].value.ival;
          else if(symbol_table[s1].type==2)
            symbol_table[s1].value.fval_ptr[symbol_table[index].value.ival-symbol_table[s1].start]=symbol_table[s2].value.fval;
          else if(symbol_table[s1].type==3)
            symbol_table[s1].value.cval_ptr[symbol_table[index].value.ival-symbol_table[s1].start]=symbol_table[s2].value.cval;
          else if(symbol_table[s1].type==4)
            symbol_table[s1].value.bval_ptr[symbol_table[index].value.ival-symbol_table[s1].start]=symbol_table[s2].value.bval;
          strcpy(tac[i].s1,new_var);
        }
      }
      else
      {
        char *end_index = strchr(tac[i].s2, ']');
        char*new_var=malloc(128*sizeof(char));
        strcpy(new_var,tac[i].s2);
        *end_index='\0';
        *start_index='\0';
        char* after_str=start_index+1;
        int s2=find_var(tac[i].s2);
        int index=find_var(after_str);
        int s1=find_var(tac[i].s1);
        if(s1==-1)
          s1=make_var(tac[i].s1,symbol_table[s2].type);
        if(symbol_table[s2].type==1)
          symbol_table[s1].value.ival=symbol_table[s2].value.ival_ptr[symbol_table[index].value.ival-symbol_table[s2].start];
        else if(symbol_table[s2].type==2)
          symbol_table[s1].value.fval=symbol_table[s2].value.fval_ptr[symbol_table[index].value.ival-symbol_table[s2].start];
        else if(symbol_table[s2].type==3)
          symbol_table[s1].value.cval=symbol_table[s2].value.cval_ptr[symbol_table[index].value.ival-symbol_table[s2].start];
        else if(symbol_table[s2].type==4)
          symbol_table[s1].value.bval=symbol_table[s2].value.bval_ptr[symbol_table[index].value.ival-symbol_table[s2].start];
        strcpy(tac[i].s2,new_var);
      }
    }
    
    else if(strcmp(tac[i].op,":-")==0)
    {
      
      char *start_index = strchr(tac[i].s2, '[');
      if(start_index==NULL)
      {
        //int
        int s2=find_var(tac[i].s2);
        int s1=find_var(tac[i].s1);
        if(s1==-1)
          s1=make_var(tac[i].s1,symbol_table[s2].type);
        if(symbol_table[s2].type==1)
          symbol_table[s1].value.ival=-symbol_table[s2].value.ival;
        else if(symbol_table[s2].type==2)
          symbol_table[s1].value.fval=-symbol_table[s2].value.fval;
        
      }
      else
      {
        char *end_index = strchr(tac[i].s2, ']');
        char*new_var=malloc(128*sizeof(char));
        strcpy(new_var,tac[i].s2);
        *end_index='\0';
        *start_index='\0';
        char* after_str=start_index+1;
        int s2=find_var(tac[i].s2);
        int index=find_var(after_str);
        int s1=find_var(tac[i].s1);
        if(s1==-1)
          s1=make_var(tac[i].s1,symbol_table[s2].type);
        if(symbol_table[s2].type==1)
          symbol_table[s1].value.ival=-symbol_table[s2].value.ival_ptr[symbol_table[index].value.ival-symbol_table[s2].start];
        else if(symbol_table[s2].type==2)
          symbol_table[s1].value.fval=-symbol_table[s2].value.fval_ptr[symbol_table[index].value.ival-symbol_table[s2].start];
        strcpy(tac[i].s2,new_var);
      }

    }
    else if(strcmp(tac[i].op,"++")==0)
    {
      
      char *start_index = strchr(tac[i].s1, '[');
      if(start_index==NULL)
      {
        int s1=find_var(tac[i].s1);
        symbol_table[s1].value.ival=symbol_table[s1].value.ival+1;
        
      }
      else
      {
        char *end_index = strchr(tac[i].s1, ']');
        char*new_var=malloc(128*sizeof(char));
        strcpy(new_var,tac[i].s1);
        *end_index='\0';
        *start_index='\0';
        char* after_str=start_index+1;
        int s1=find_var(tac[i].s1);
        int index=find_var(after_str);
        symbol_table[s1].value.ival=symbol_table[s1].value.ival_ptr[symbol_table[index].value.ival-symbol_table[s1].start]+1;
        strcpy(tac[i].s2,new_var);
      }

    }
    else if(strcmp(tac[i].op,"--")==0)
    {
      
      char *start_index = strchr(tac[i].s1, '[');
      if(start_index==NULL)
      {
        int s1=find_var(tac[i].s1);
        symbol_table[s1].value.ival=symbol_table[s1].value.ival-1;
        
      }
      else
      {
        char *end_index = strchr(tac[i].s1, ']');
        char*new_var=malloc(128*sizeof(char));
        strcpy(new_var,tac[i].s1);
        *end_index='\0';
        *start_index='\0';
        char* after_str=start_index+1;
        int s1=find_var(tac[i].s1);
        int index=find_var(after_str);
        symbol_table[s1].value.ival=symbol_table[s1].value.ival_ptr[symbol_table[index].value.ival-symbol_table[s1].start]-1;
        strcpy(tac[i].s2,new_var);
      }

    }
    else if(strcmp(tac[i].op,":-i")==0)
    {
      int s1=find_var(tac[i].s1);
      if(s1==-1)
        s1=make_var(tac[i].s1,1);
      symbol_table[s1].value.ival=-atoi(tac[i].s2);
      
    }
    else if(strcmp(tac[i].op,":-f")==0)
    {
      
      int s1=find_var(tac[i].s1);
      if(s1==-1)
        s1=make_var(tac[i].s1,2);
      symbol_table[s1].value.fval=-atoi(tac[i].s2);
    }
    else if(strcmp(tac[i].op,":=i")==0)
    {
      int s1=find_var(tac[i].s1);
      if(s1==-1)
        s1=make_var(tac[i].s1,1);
      symbol_table[s1].value.ival=atoi(tac[i].s2);
    }
    else if(strcmp(tac[i].op,":=f")==0)
    {
      int s1=find_var(tac[i].s1);
      if(s1==-1)
        s1=make_var(tac[i].s1,2);
      symbol_table[s1].value.fval=atof(tac[i].s2);
    }
    else if(strcmp(tac[i].op,":=c")==0)
    {
      int s1=find_var(tac[i].s1);
      if(s1==-1)
        s1=make_var(tac[i].s1,3);
      symbol_table[s1].value.cval=tac[i].s2[0];
    }
    else if(strcmp(tac[i].op,":=b")==0)
    {
      int s1=find_var(tac[i].s1);
      if(s1==-1)
        s1=make_var(tac[i].s1,4);
      int temp=1;
      if(strcmp(tac[i].s2,"FALSE")==0)
        temp=0;
      symbol_table[s1].value.bval=temp;
    }
    else if(strcmp(tac[i].op,"*")==0)
    {
      int s2=find_var(tac[i].s2);
      int s3=find_var(tac[i].s3);
      int s1=find_var(tac[i].s1);
      if(s1==-1)
        s1=make_var(tac[i].s1,symbol_table[s2].type);

      if(symbol_table[s2].type==1)
        symbol_table[s1].value.ival= symbol_table[s2].value.ival* symbol_table[s3].value.ival;
      else
        symbol_table[s1].value.fval= symbol_table[s2].value.fval* symbol_table[s3].value.fval;      
    }
    else if(strcmp(tac[i].op,"-")==0)
    {
      int s2=find_var(tac[i].s2);
      int s3=find_var(tac[i].s3);
      int s1=find_var(tac[i].s1);
      if(s1==-1)
        s1=make_var(tac[i].s1,symbol_table[s2].type);

      if(symbol_table[s2].type==1)
        symbol_table[s1].value.ival= symbol_table[s2].value.ival- symbol_table[s3].value.ival;
      else
        symbol_table[s1].value.fval= symbol_table[s2].value.fval- symbol_table[s3].value.fval;      
    }
    else if(strcmp(tac[i].op,"+")==0)
    {
      int s2=find_var(tac[i].s2);
      int s3=find_var(tac[i].s3);
      int s1=find_var(tac[i].s1);
      if(s1==-1)
        s1=make_var(tac[i].s1,symbol_table[s2].type);

      if(symbol_table[s2].type==1)
        symbol_table[s1].value.ival= symbol_table[s2].value.ival+ symbol_table[s3].value.ival;
      else
        symbol_table[s1].value.fval= symbol_table[s2].value.fval+ symbol_table[s3].value.fval;      
    }
    else if(strcmp(tac[i].op,"/")==0)
    {
      int s2=find_var(tac[i].s2);
      int s3=find_var(tac[i].s3);
      int s1=find_var(tac[i].s1);
      if(s1==-1)
        s1=make_var(tac[i].s1,symbol_table[s2].type);
      float temp1=(symbol_table[s2].type==1?(float)symbol_table[s2].value.ival:symbol_table[s2].value.fval);
      float temp2=(symbol_table[s3].type==1?(float)symbol_table[s3].value.ival:symbol_table[s3].value.fval);
      symbol_table[s1].value.fval=temp1/temp2;  
    }
    else if(strcmp(tac[i].op,"%")==0)
    {
      int s2=find_var(tac[i].s2);
      int s3=find_var(tac[i].s3);
      int s1=find_var(tac[i].s1);
      if(s1==-1)
        s1=make_var(tac[i].s1,symbol_table[s2].type);
      symbol_table[s1].value.ival= symbol_table[s2].value.ival % symbol_table[s3].value.ival; 
    }
    else if(strcmp(tac[i].op,"DIV")==0)
    {
      int s2=find_var(tac[i].s2);
      int s3=find_var(tac[i].s3);
      int s1=find_var(tac[i].s1);
      if(s1==-1)
        s1=make_var(tac[i].s1,symbol_table[s2].type);
      symbol_table[s1].value.ival= symbol_table[s2].value.ival / symbol_table[s3].value.ival; 
    }
    else if(strcmp(tac[i].op,">")==0)
    {
      int s2=find_var(tac[i].s2);
      int s3=find_var(tac[i].s3);
      int s1=find_var(tac[i].s1);
      if(s1==-1)
        s1=make_var(tac[i].s1,4);
      if(symbol_table[s2].type==1)
      {
        symbol_table[s1].value.bval=symbol_table[s2].value.ival>symbol_table[s3].value.ival;
      }
      else
      {
        symbol_table[s1].value.bval=symbol_table[s2].value.fval>symbol_table[s3].value.fval;
      }
    }
    else if(strcmp(tac[i].op,"<")==0)
    {
      int s2=find_var(tac[i].s2);
      int s3=find_var(tac[i].s3);
      int s1=find_var(tac[i].s1);
      if(s1==-1)
        s1=make_var(tac[i].s1,4);
      if(symbol_table[s2].type==1)
      {
        symbol_table[s1].value.bval=symbol_table[s2].value.ival<symbol_table[s3].value.ival;
      }
      else
      {
        symbol_table[s1].value.bval=symbol_table[s2].value.fval<symbol_table[s3].value.fval;
      }
    }
    else if(strcmp(tac[i].op,">=")==0)
    {
      int s2=find_var(tac[i].s2);
      int s3=find_var(tac[i].s3);
      int s1=find_var(tac[i].s1);
      if(s1==-1)
        s1=make_var(tac[i].s1,4);
      if(symbol_table[s2].type==1)
      {
        symbol_table[s1].value.bval=symbol_table[s2].value.ival>=symbol_table[s3].value.ival;
      }
      else
      {
        symbol_table[s1].value.bval=symbol_table[s2].value.fval>=symbol_table[s3].value.fval;
      }
    }
    else if(strcmp(tac[i].op,"<=")==0)
    {
      int s2=find_var(tac[i].s2);
      int s3=find_var(tac[i].s3);
      int s1=find_var(tac[i].s1);
      if(s1==-1)
        s1=make_var(tac[i].s1,4);
      if(symbol_table[s2].type==1)
      {
        symbol_table[s1].value.bval=symbol_table[s2].value.ival<=symbol_table[s3].value.ival;
      }
      else
      {
        symbol_table[s1].value.bval=symbol_table[s2].value.fval<=symbol_table[s3].value.fval;
      }
    }
    else if(strcmp(tac[i].op,"==")==0)
    {
      int s2=find_var(tac[i].s2);
      int s3=find_var(tac[i].s3);
      int s1=find_var(tac[i].s1);
      if(s1==-1)
        s1=make_var(tac[i].s1,4);
      if(symbol_table[s2].type==1)
      {
        symbol_table[s1].value.bval=symbol_table[s2].value.ival==symbol_table[s3].value.ival;
      }
      else
      {
        symbol_table[s1].value.bval=symbol_table[s2].value.fval==symbol_table[s3].value.fval;
      }
    }
    else if(strcmp(tac[i].op,"!=")==0)
    {
      int s2=find_var(tac[i].s2);
      int s3=find_var(tac[i].s3);
      int s1=find_var(tac[i].s1);
      if(s1==-1)
        s1=make_var(tac[i].s1,4);
      if(symbol_table[s2].type==1)
      {
        symbol_table[s1].value.bval=symbol_table[s2].value.ival!=symbol_table[s3].value.ival;
      }
      else
      {
        symbol_table[s1].value.bval=symbol_table[s2].value.fval!=symbol_table[s3].value.fval;
      }
    }
    else if(strcmp(tac[i].op,"NOT")==0)
    {
      int s1=find_var(tac[i].s1);
      if(s1==-1)
        s1=make_var(tac[i].s1,4);

      char *start_index = strchr(tac[i].s2, '[');
      if(start_index==NULL)
      {
        int s2=find_var(tac[i].s2);
        symbol_table[s1].value.bval=!symbol_table[s2].value.bval;        
      }
      else
      {
        char *end_index = strchr(tac[i].s2, ']');
        char*new_var=malloc(128*sizeof(char));
        strcpy(new_var,tac[i].s2);
        *end_index='\0';
        *start_index='\0';
        char* after_str=start_index+1;
        int s2=find_var(tac[i].s2);
        int index=find_var(after_str);
        symbol_table[s1].value.bval=!symbol_table[s2].value.bval_ptr[symbol_table[index].value.ival-symbol_table[s2].start];
        strcpy(tac[i].s2,new_var);
      }
    }
    else if(strcmp(tac[i].op,"AND")==0)
    {
      int s1=make_var(tac[i].s1,4);
      int temp1,temp2;
      char *start_index = strchr(tac[i].s2, '[');
      if(start_index==NULL)
      {
        int s2=find_var(tac[i].s2);
        temp1=symbol_table[s2].value.bval;        
      }
      else
      {
        char *end_index = strchr(tac[i].s2, ']');
        char*new_var=malloc(128*sizeof(char));
        strcpy(new_var,tac[i].s2);
        *end_index='\0';
        *start_index='\0';
        char* after_str=start_index+1;
        int s2=find_var(tac[i].s2);
        int index=find_var(after_str);
        temp1=symbol_table[s2].value.bval_ptr[symbol_table[index].value.ival-symbol_table[s2].start];
        strcpy(tac[i].s2,new_var);
      }

      start_index = strchr(tac[i].s3, '[');
      if(start_index==NULL)
      {
        int s3=find_var(tac[i].s3);
        temp2=symbol_table[s3].value.bval;        
      }
      else
      {
        char *end_index = strchr(tac[i].s3, ']');
        char*new_var=malloc(128*sizeof(char));
        strcpy(new_var,tac[i].s3);
        *end_index='\0';
        *start_index='\0';
        char* after_str=start_index+1;
        int s3=find_var(tac[i].s3);
        int index=find_var(after_str);
        temp2=symbol_table[s3].value.bval_ptr[symbol_table[index].value.ival-symbol_table[s3].start];
        strcpy(tac[i].s3,new_var);
      }
      symbol_table[s1].value.bval=temp1&temp2;

    }
    else if(strcmp(tac[i].op,"OR")==0)
    {
      int s1=find_var(tac[i].s1);
      if(s1==-1)
        s1=make_var(tac[i].s1,4);
      int temp1,temp2;
      char *start_index = strchr(tac[i].s2, '[');
      if(start_index==NULL)
      {
        int s2=find_var(tac[i].s2);
        temp1=symbol_table[s2].value.bval;        
      }
      else
      {
        char *end_index = strchr(tac[i].s2, ']');
        char*new_var=malloc(128*sizeof(char));
        strcpy(new_var,tac[i].s2);
        *end_index='\0';
        *start_index='\0';
        char* after_str=start_index+1;
        int s2=find_var(tac[i].s2);
        int index=find_var(after_str);
        temp1=symbol_table[s2].value.bval_ptr[symbol_table[index].value.ival-symbol_table[s2].start];
        strcpy(tac[i].s2,new_var);
      }

      start_index = strchr(tac[i].s3, '[');
      if(start_index==NULL)
      {
        int s3=find_var(tac[i].s3);
        temp2=symbol_table[s3].value.bval;        
      }
      else
      {
        char *end_index = strchr(tac[i].s3, ']');
        char*new_var=malloc(128*sizeof(char));
        strcpy(new_var,tac[i].s3);
        *end_index='\0';
        *start_index='\0';
        char* after_str=start_index+1;
        int s3=find_var(tac[i].s3);
        int index=find_var(after_str);
        temp2=symbol_table[s3].value.bval_ptr[symbol_table[index].value.ival-symbol_table[s3].start];
        strcpy(tac[i].s3,new_var);
      }
      symbol_table[s1].value.bval=temp1|temp2;
    }
    else if(strcmp(tac[i].op,"GOTO")==0)
    {
      i=find_goto_line(tac[i].s2);
    }
    else if(strcmp(tac[i].op,"IF")==0)
    {
      int temp=0;
      int s1=find_var(tac[i].s1);
      if(symbol_table[s1].value.bval==1)
        i=find_goto_line(tac[i].s2);
    }
    else if(strcmp(tac[i].op,"IF_2-")==0)
    {
      int temp=0;
      int s1=find_var(tac[i].s1);
      int s3=find_var(tac[i].s3);
      if(symbol_table[s1].value.ival!=symbol_table[s3].value.ival-1 )
        i=find_goto_line(tac[i].s2);
      /* else
        symbol_table[s1].value.ival=symbol_table[s1].value.ival+1; */
    }
    else if(strcmp(tac[i].op,"IF_2+")==0)
    {
      int temp=0;
      int s1=find_var(tac[i].s1);
      int s3=find_var(tac[i].s3);
      if(symbol_table[s1].value.ival!=symbol_table[s3].value.ival+1 )
        i=find_goto_line(tac[i].s2);
      /* else
        symbol_table[s1].value.ival=symbol_table[s1].value.ival-1; */
    }
    else if(strcmp(tac[i].op,"print_2")==0)
    {
      printf("%s\n",tac[i].s1);
    }
    else if(strcmp(tac[i].op,"print_1")==0)
    {
      char *start_index = strchr(tac[i].s1, '[');
      if(start_index==NULL)
      {
        int s1=find_var(tac[i].s1);
        if(symbol_table[s1].type==1)
        {
          printf("%d\n",symbol_table[s1].value.ival);
        }
        else if(symbol_table[s1].type==2)
        {
          printf("%f\n",symbol_table[s1].value.fval);
        }
        else if(symbol_table[s1].type==3)
        {
          printf("%c\n",symbol_table[s1].value.cval);
        }
        else if(symbol_table[s1].type==4)
        {
          printf("%d\n",symbol_table[s1].value.bval);
        }
              
      }
      else
      {
        char *end_index = strchr(tac[i].s1, ']');
        char*new_var=malloc(128*sizeof(char));
        strcpy(new_var,tac[i].s1);
        *end_index='\0';
        *start_index='\0';
        char* after_str=start_index+1;
        int s1=find_var(tac[i].s1);
        int index=find_var(after_str);
        
        if(symbol_table[s1].type==1)
        {
          printf("%d\n",symbol_table[s1].value.ival_ptr[symbol_table[index].value.ival-symbol_table[s1].start]);
        }
        else if(symbol_table[s1].type==2)
        {
          printf("%f\n",symbol_table[s1].value.fval_ptr[symbol_table[index].value.ival-symbol_table[s1].start]);
        }
        else if(symbol_table[s1].type==3)
        {
          printf("%c\n",symbol_table[s1].value.cval_ptr[symbol_table[index].value.ival-symbol_table[s1].start]);
        }
        else if(symbol_table[s1].type==4)
        {
          printf("%d\n",symbol_table[s1].value.bval_ptr[symbol_table[index].value.ival-symbol_table[s1].start]);
        }

        strcpy(tac[i].s2,new_var);
      }
    }
    else if(strcmp(tac[i].op,"scan")==0)
    {
      char* start_index = strchr(tac[i].s1, '[');
      if(start_index==NULL)
      {
        int s1=find_var(tac[i].s1);
        if(symbol_table[s1].type==1)
        {
          scanf("%d",&symbol_table[s1].value.ival);
        }
        else if(symbol_table[s1].type==2)
        {
          scanf("%f",&symbol_table[s1].value.fval);
        }
        else if(symbol_table[s1].type==3)
        {
          scanf("%c",&symbol_table[s1].value.cval);
        }
        else if(symbol_table[s1].type==4)
        {
          scanf("%d",&symbol_table[s1].value.bval);
        }
      }
      else
      {
        char *end_index = strchr(tac[i].s1, ']');
        char*new_var=malloc(128*sizeof(char));
        strcpy(new_var,tac[i].s1);
        *end_index='\0';
        *start_index='\0';
        char* after_str=start_index+1;
        int s1=find_var(tac[i].s1);
        int index=find_var(after_str);

        if(symbol_table[s1].type==1)
        {
          scanf("%d",&symbol_table[s1].value.ival_ptr[symbol_table[index].value.ival-symbol_table[s1].start]);
        }
        else if(symbol_table[s1].type==2)
        {
          scanf("%f",&symbol_table[s1].value.fval_ptr[symbol_table[index].value.ival-symbol_table[s1].start]);
        }
        else if(symbol_table[s1].type==3)
        {
          scanf("%c",&symbol_table[s1].value.cval_ptr[symbol_table[index].value.ival-symbol_table[s1].start]);
        }
        else if(symbol_table[s1].type==4)
        {
          scanf("%d",&symbol_table[s1].value.bval_ptr[symbol_table[index].value.ival-symbol_table[s1].start]);
        }
        strcpy(tac[i].s1,new_var);
      }
    }


  }

  printf("\n symbol table\n\n");
  printf("%-32s %-32s %s\n", "Variable", "Type", "Value");

  for(int i=0;i<n;i++)
  {
    printf("%-32s %-32s",symbol_table[i].id,getType(symbol_table[i].type));
    switch(symbol_table[i].type)
    {
      case 1:
        if(symbol_table[i].arr_flag)
        {
          for(int j=symbol_table[i].start;j<=symbol_table[i].end;j++)
          {
            printf("  %d",symbol_table[i].value.ival_ptr[j-symbol_table[i].start]);
          }
          printf("\n");
        }
        else
        {
          printf("  %d\n",symbol_table[i].value.ival);
        }
        break;
      case 2:
        if(symbol_table[i].arr_flag)
        {
          for(int j=symbol_table[i].start;j<=symbol_table[i].end;j++)
          {
            printf("  %f",symbol_table[i].value.fval_ptr[j-symbol_table[i].start]);
          }
          printf("\n");
        }
        else
        {
          printf("  %f\n",symbol_table[i].value.fval);
        }
        break;
      case 3:
        if(symbol_table[i].arr_flag)
        {
          for(int j=symbol_table[i].start;j<=symbol_table[i].end;j++)
          {
            printf("  %c",symbol_table[i].value.cval_ptr[j-symbol_table[i].start]);
          }
          printf("\n");
        }
        else
        {
          printf("  %c\n",symbol_table[i].value.cval);
        }
        break;
      
      case 4:
      if(symbol_table[i].arr_flag)
      {
        for(int j=symbol_table[i].start;j<=symbol_table[i].end;j++)
        {
          printf("  %d",symbol_table[i].value.bval_ptr[j-symbol_table[i].start]);
        }
        printf("\n");
      }
      else
      {
        printf("  %d\n",symbol_table[i].value.bval);
      }
      break;
    }
  }


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