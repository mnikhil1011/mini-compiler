Compilation and running code part_3/semantic part_4 part_5:

flex DMNNP007.l
yacc -d DMNNP007.y
cc lex.yy.c y.tab.c -ll
./a.out filename

For part_3/AST
yacc -d DMNNP007.y
cc lex.yy.c y.tab.c -ll
./a.out filename
python3 tree.py

part1:lexical analysis
part2: syntax analysis
part3: semantic analysis and Abstract tree generation
part4: 3address code generation
part5: running the compiler on the code and generating output
