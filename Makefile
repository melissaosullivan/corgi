OBJS = ast.cmo symtab.cmo parser.cmo scanner.cmo interpreter.cmo 
#OBJS = ast.cmo parser.cmo scanner.cmo interpreter.cmo 

interpreter: $(OBJS)
	ocamlc -o interpreter -g $(OBJS)

scanner.ml: scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	ocamlyacc parser.mly

%.cmo : %.ml
	ocamlc -g -c $<

%.cmi : %.mli
	ocamlc -g -c $<

.PHONY : clean
clean :
	rm -rf interpreter parser.ml parser.mli scanner.ml \
	*.cmo *.cmi

ast.cmo: 
ast.cmx: 

symtab.cmo: ast.cmo
symtab.cmx: ast.cmx

interpreter.cmo: scanner.cmo parser.cmi ast.cmo symtab.cmo 
interpreter.cmx: scanner.cmx parser.cmx ast.cmx symtab.cmx
interpreter.cmo: scanner.cmo parser.cmi ast.cmo
interpreter.cmx: scanner.cmx parser.cmx ast.cmx

parser.cmo: ast.cmo parser.cmi 
parser.cmx: ast.cmx parser.cmi 
scanner.cmo: parser.cmi 
scanner.cmx: parser.cmx 
parser.cmi: ast.cmo 
