from nltk.tree import *

f = open('syntaxtree.txt', 'r')
text = f.readlines()[0]
f.close()
tree = Tree.fromstring(text) 
print(TreePrettyPrinter(tree).text())