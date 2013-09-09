OCAMLC = ocamlc
OCAMLOPT = ocamlopt
COMPFLAGS = -w +A-4-17-44-45 -I +compiler-libs

all: genlifter.exe dumpast.exe ppx_metaquot.exe

genlifter.exe: genlifter.cmo
	$(OCAMLC) $(COMPFLAGS) -o genlifter.exe ocamlcommon.cma genlifter.cmo

dumpast.exe: ast_lifter.cmo dumpast.cmo
	$(OCAMLC) $(COMPFLAGS) -o dumpast.exe ocamlcommon.cma ast_lifter.cmo dumpast.cmo


ppx_metaquot.exe: ast_lifter.cmo ppx_metaquot.cmo
	$(OCAMLC) $(COMPFLAGS) -o dumpast.exe ocamlcommon.cma ast_lifter.cmo ppx_metaquot.cmo


ast_lifter.ml: genlifter.exe
	./genlifter.exe -I +compiler-libs Parsetree.expression > ast_lifter.ml || rm -rf ast_lifter.ml

clean:
	rm -f *.cm* *.exe *~ *.o *.obj
	rm -f ast_lifter.ml

# Default rules

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(COMPFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(COMPFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(COMPFLAGS) -c $<
