%{

  #include<stdio.h>
  #include<stdlib.h>
  #include<string.h>
  #include<stdbool.h>
  #include<limits.h>
  #include<ctype.h>
  #include<stdarg.h>
  int yylex(void);
  extern FILE *yyin;

  typedef struct{
    int num;
    char* op;
    struct Node** nodes;
  }Opr;

  typedef struct  {
    union
    {
      Opr opr;
      char * leaf;
    }val;
    int type;//1 is op
  }Node;

  Node* make_op_node(int num,char* op,...)
  {
    va_list ap;
    Node** nodes=malloc(sizeof(Node*)*num);

    va_start(ap, num);
    for (int i = 0; i < num; i++)
      nodes[i] = va_arg(ap, Node*);
    va_end(ap);
    Opr opr;
    opr.nodes=nodes;
    opr.op=op;
    opr.num=num;

    
    Node * retnode=malloc(sizeof(Node));
    retnode->type=1;
    retnode->val.opr=opr;
    return retnode;
  }

  Node* make_leaf_node(char*s)
  {
    
    Node * retnode=malloc(sizeof(Node));
    retnode->type=2;
    retnode->val.leaf=s;
    return retnode;
  }

  void print_node(Node* head)
  {
    if(! head)
    {
      printf("NULL");
    }
    else if(head->type==1)
    {
      printf("%s\n",head->val.opr.op);
      for(int i=0;i<head->val.opr.num;i++)
      {
        print_node(head->val.opr.nodes[i]);
      }
    }
    else
    {
      printf("%s\n",head->val.leaf);
    }
    
  }

  void print_node_to_file(FILE*f,Node* head)
  {
    fprintf(f,"(");
    if(! head)
    {
      fprintf(f,"NULL");
    }
    else if(head->type==1)
    {
      fprintf(f,"%s",head->val.opr.op);
      for(int i=0;i<head->val.opr.num;i++)
      {
        print_node_to_file(f,head->val.opr.nodes[i]);
      }
    }
    else
    {
      fprintf(f,"%s",head->val.leaf);
    }
    fprintf(f,")");
    
  }
  Node* head;
  void replace_space(char*str)
  {
    int length = strlen(str);
    for (int i = 0; i < length; i++) {
      if (str[i] == ' ') {
        str[i] = '_';
      }
    }
  }
%}

%union {
  char* sval;
  int ival;
  float fval;
  struct Node* node;
}

%token VAR INTEGER BOOLEAN CHAR REAL PROGRAM BEGIN_STMT END_STMT WRITE READ IF ELSE THEN AND OR NOT WHILE DO DOWNTO TO FOR ARRAY OF DIV TRUE_CONST FALSE_CONST
%token<ival> INTEGER_CONST
%token<fval> FLOAT_CONST
%token<sval> TEXT VAR_ID ARR_ID

%type<node>S program number_const arithmetic_statement logical_expression_helper assignment_statement variable program_block block_body program_statement scan_statement print_statement print_variable expression_statement conditional_statement logical_expression loop_statement 

%left OR
%left AND
%right NOT
%left '%'
%left '+' '-'
%left '*' '/'
%left DIV


%%

S : program '.' {
  head=$1;
  return 1;
  };

program: program_header var_structure program_block {
  $$=$3;
};


program_header : PROGRAM VAR_ID ';' ;


var_structure : VAR  var_line ;

var_line : var_ids ':' var_type ';'  var_line 
  | VAR_ID  ':' var_type ';'  var_line 
  | VAR_ID ':' ARRAY '[' INTEGER_CONST '.' '.' INTEGER_CONST ']' OF var_type ';' var_line 
  | 
  ;

var_ids : VAR_ID ','  VAR_ID  next_var  ;

next_var:  ',' VAR_ID next_var 
  | ;

var_type : INTEGER 
  | REAL 
  | CHAR 
  | BOOLEAN 
  ;



program_block :BEGIN_STMT block_body END_STMT {
  $$=$2;
};

block_body: program_statement ';' block_body {

  char * op=malloc(sizeof(char)*10);
  strcpy(op,";");
  $$=make_op_node(2,op,$1,$3);

  }|  {$$=NULL;};

program_statement : WRITE '(' print_statement ')' {
    char * op=malloc(sizeof(char)*10);
    strcpy(op,"write");
    $$=make_op_node(1,op,$3);
  
  }| READ '(' scan_statement ')' {
    char * op=malloc(sizeof(char)*10);
    strcpy(op,"read");
    $$=make_op_node(1,op,$3);
  
  }| assignment_statement {$$=$1;}| conditional_statement {$$=$1;}| loop_statement{$$=$1;};

print_statement : print_variable {$$=$1;}
  | TEXT  {replace_space($1);$$=make_leaf_node($1);}
  | {$$=NULL;}
  ;

print_variable : variable ',' print_variable {
    char * op=malloc(sizeof(char)*10);
    strcpy(op,",");
    $$=make_op_node(2,op,$1,$3);
}| variable {$$=$1;} ;

scan_statement : variable {$$=$1;};

assignment_statement: variable ':' '=' expression_statement {
    char * op=malloc(sizeof(char)*10);
    strcpy(op,":=");
    $$=make_op_node(2,op,$1,$4);
}
  | variable ':' '=' TEXT {
    char * op=malloc(sizeof(char)*10);
    strcpy(op,":=");
    Node* temp4=make_leaf_node($4);
    $$=make_op_node(2,op,$1,temp4);
  }
  ;

expression_statement:  arithmetic_statement {$$=$1;}
 | logical_expression {$$=$1;}
  ;

variable :  VAR_ID {
      Node* temp=make_leaf_node($1);
      char * op=malloc(sizeof(char)*10);
      strcpy(op,"var");
      $$=make_op_node(1,op,temp);
    }
  | ARR_ID '[' arithmetic_statement ']' {
      Node* temp=make_leaf_node($1);

      char * op=malloc(sizeof(char)*10);
      strcpy(op,"var");
      char * ob=malloc(sizeof(char)*10);
      strcpy(ob,"[");
      char * cb=malloc(sizeof(char)*10);
      strcpy(cb,"]");

      Node* obnode=make_leaf_node(ob);
      Node* cbnode=make_leaf_node(cb);
      $$=make_op_node(4,op,temp,obnode,$3,cbnode);
  }
  |'(' variable ')' {$$=$2;}
  ;

 
arithmetic_statement: arithmetic_statement '+' arithmetic_statement {
      char * op=malloc(sizeof(char)*10);
      strcpy(op,"+");
      $$=make_op_node(2,op,$1,$3);
    }
  | arithmetic_statement '*' arithmetic_statement {
      char * op=malloc(sizeof(char)*10);
      strcpy(op,"*");
      $$=make_op_node(2,op,$1,$3);
    }
  | arithmetic_statement '-' arithmetic_statement {
      char * op=malloc(sizeof(char)*10);
      strcpy(op,"-");
      $$=make_op_node(2,op,$1,$3);
    }
  | arithmetic_statement '/' arithmetic_statement {
      char * op=malloc(sizeof(char)*10);
      strcpy(op,"/");
      $$=make_op_node(2,op,$1,$3);
    }
  | arithmetic_statement DIV arithmetic_statement {
      char * op=malloc(sizeof(char)*10);
      strcpy(op,"DIV");
      $$=make_op_node(2,op,$1,$3);
    }
  | arithmetic_statement '%' arithmetic_statement {
      char * op=malloc(sizeof(char)*10);
      strcpy(op,"%");
      $$=make_op_node(2,op,$1,$3);
    }
  | '(' arithmetic_statement ')' {$$=$2;}
  | '+' '(' arithmetic_statement ')' {$$=$3;}
  | '-'  '(' arithmetic_statement ')' {
      char * op=malloc(sizeof(char)*10);
      strcpy(op,"-");
      $$=make_op_node(1,op,$3);
    }
  | variable {$$=$1;}
  | '-' variable {
      char * op=malloc(sizeof(char)*10);
      strcpy(op,"-");
      $$=make_op_node(1,op,$2);
    }
  | number_const {$$=$1;}
  | '-' number_const {
      char * op=malloc(sizeof(char)*10);
      strcpy(op,"-");
      $$=make_op_node(1,op,$2);
    }
  ; 

number_const: INTEGER_CONST {
    char * temp=malloc(sizeof(char)*10);
    sprintf(temp,"%d",$1);
    $$=make_leaf_node(temp);
  }
  | FLOAT_CONST {
    char * temp=malloc(sizeof(char)*10);
    sprintf(temp,"%f",$1);
    $$=make_leaf_node(temp);
  }
  ;



conditional_statement :IF logical_expression_helper THEN program_block ELSE program_block {
    char * op=malloc(sizeof(char)*10);
    strcpy(op,"IF_STMT");

    char * expr=malloc(sizeof(char)*10);
    strcpy(expr,"condn");
    Node* expr_node=make_op_node(1,expr,$2);

    char * then=malloc(sizeof(char)*10);
    strcpy(then,"then");
    Node* then_node=make_op_node(1,then,$4);

    char * else_stmt=malloc(sizeof(char)*10);
    strcpy(else_stmt,"else");
    Node* else_stmt_node=make_op_node(1,else_stmt,$6);

    $$=make_op_node(3,op,expr_node,then_node,else_stmt_node);

}
  | IF logical_expression_helper THEN program_block {
    char * op=malloc(sizeof(char)*10);
    strcpy(op,"IF_STMT");

    char * expr=malloc(sizeof(char)*10);
    strcpy(expr,"condn");
    Node* expr_node=make_op_node(1,expr,$2);

    char * then=malloc(sizeof(char)*10);
    strcpy(then,"then");
    Node* then_node=make_op_node(1,then,$4);

    $$=make_op_node(2,op,expr_node,then_node);
  }
  ;

logical_expression_helper: variable {$$=$1;}
  |logical_expression {$$=$1;}
  ;

logical_expression : NOT logical_expression {
      char * op=malloc(sizeof(char)*10);
      strcpy(op,"NOT");
      $$=make_op_node(1,op,$2);
  }
  | '(' logical_expression ')'{$$=$2;}
  | NOT variable {
      char * op=malloc(sizeof(char)*10);
      strcpy(op,"NOT");
      $$=make_op_node(1,op,$2);
  }
  |arithmetic_statement '>' arithmetic_statement {
      char * op=malloc(sizeof(char)*10);
      strcpy(op,">");
      $$=make_op_node(2,op,$1,$3);
      
    }
  | arithmetic_statement '>' '=' arithmetic_statement {
      char * op=malloc(sizeof(char)*10);
      strcpy(op,">=");
      $$=make_op_node(2,op,$1,$4);
    }
  | arithmetic_statement '<' '=' arithmetic_statement {
      char * op=malloc(sizeof(char)*10);
      strcpy(op,"<=");
      $$=make_op_node(2,op,$1,$4);
    }
  | arithmetic_statement '<' arithmetic_statement {
      char * op=malloc(sizeof(char)*10);
      strcpy(op,"<");
      $$=make_op_node(2,op,$1,$3);
    }
  | arithmetic_statement '=' arithmetic_statement {
      char * op=malloc(sizeof(char)*10);
      strcpy(op,"==");
      $$=make_op_node(2,op,$1,$3);
    }
  | arithmetic_statement '<' '>' arithmetic_statement {
      char * op=malloc(sizeof(char)*10);
      strcpy(op,"<>");
      $$=make_op_node(2,op,$1,$4);
    }
  | logical_expression AND logical_expression {
      char * op=malloc(sizeof(char)*10);
      strcpy(op,"AND");
      $$=make_op_node(2,op,$1,$3);
    }
  | logical_expression OR logical_expression {
      char * op=malloc(sizeof(char)*10);
      strcpy(op,"OR");
      $$=make_op_node(2,op,$1,$3);
    }
  | variable AND variable {
      char * op=malloc(sizeof(char)*10);
      strcpy(op,"AND");
      $$=make_op_node(2,op,$1,$3);
    }
  | logical_expression AND variable {
      char * op=malloc(sizeof(char)*10);
      strcpy(op,"AND");
      $$=make_op_node(2,op,$1,$3);
    }
  | variable AND logical_expression {
      char * op=malloc(sizeof(char)*10);
      strcpy(op,"AND");
      $$=make_op_node(2,op,$1,$3);
    }
  | variable OR logical_expression {
      char * op=malloc(sizeof(char)*10);
      strcpy(op,"OR");
      $$=make_op_node(2,op,$1,$3);
    }
  | variable OR variable {
      char * op=malloc(sizeof(char)*10);
      strcpy(op,"OR");
      $$=make_op_node(2,op,$1,$3);
    }
  | logical_expression OR variable {
      char * op=malloc(sizeof(char)*10);
      strcpy(op,"OR");
      $$=make_op_node(2,op,$1,$3);
    }
  | TRUE_CONST {
    char * temp=malloc(sizeof(char)*10);
    strcpy(temp,"TRUE");
    $$=make_leaf_node(temp);
  }
  | FALSE_CONST {
    char * temp=malloc(sizeof(char)*10);
    strcpy(temp,"FALSE");
    $$=make_leaf_node(temp);
  }
  ;

// changed this
loop_statement : WHILE logical_expression_helper DO program_block {
    char * op=malloc(sizeof(char)*10);
    strcpy(op,"WHILE");

    char * expr=malloc(sizeof(char)*10);
    strcpy(expr,"condn");
    Node* expr_node=make_op_node(1,expr,$2);

    char * do_str=malloc(sizeof(char)*10);
    strcpy(do_str,"do");
    Node* do_node=make_op_node(1,do_str,$4);

    $$=make_op_node(2,op,expr_node,do_node);
}
  | FOR assignment_statement TO arithmetic_statement DO program_block {
    char * op=malloc(sizeof(char)*10);
    strcpy(op,"FOR");

    char * assign=malloc(sizeof(char)*10);
    strcpy(assign,"assign");
    Node* assign_node=make_op_node(1,assign,$2);

    char * to=malloc(sizeof(char)*10);
    strcpy(to,"to");
    Node* to_node=make_op_node(1,to,$4);

    char * do_str=malloc(sizeof(char)*10);
    strcpy(do_str,"do");
    Node* do_node=make_op_node(1,do_str,$6);

    $$=make_op_node(3,op,assign_node,to_node,do_node);
}
  | FOR assignment_statement DOWNTO arithmetic_statement DO program_block {
    char * op=malloc(sizeof(char)*10);
    strcpy(op,"FOR");

    char * assign=malloc(sizeof(char)*10);
    strcpy(assign,"assign");
    Node* assign_node=make_op_node(1,assign,$2);

    char * downto=malloc(sizeof(char)*10);
    strcpy(downto,"downto");
    Node* downto_node=make_op_node(1,downto,$4);

    char * do_str=malloc(sizeof(char)*10);
    strcpy(do_str,"do");
    Node* do_node=make_op_node(1,do_str,$6);

    $$=make_op_node(3,op,assign_node,downto_node,do_node);
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

  /* Node* node1=malloc(sizeof(Node));
  Node* node2=malloc(sizeof(Node));
  strcpy(node1->val.leaf,"a");
  node1->type=2;

  strcpy(node2->val.leaf,"a");
  node2->type=2;
  
  Node* head=make_op_node(2,1,node1,node2); */
  //print_node(head);
  FILE*f=fopen("syntaxtree.txt", "w");
  print_node_to_file(f,head);
  fclose(file);

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