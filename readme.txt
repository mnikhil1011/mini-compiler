members: NIMISH AGRAWAL , DIVYANSHU KUMAR , MEHUL TAMBI , NIKHIL MOGALAPALLI , SARTHAK DHIMAN

compilation and running code part_3/semantic part_4 part_5:

flex DMNNP007.l
yacc -d DMNNP007.y
cc lex.yy.c y.tab.c -ll
./a.out filename

For part_3/AST
yacc -d DMNNP007.y
cc lex.yy.c y.tab.c -ll
./a.out filename
python3 tree.py