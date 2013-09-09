#  This file is part of the ppx_tools package.  It is released
#  under the terms of the MIT license (see LICENSE file).
#  Copyright 2013  Alain Frisch and LexiFi

PACKAGE = ppx_tools
VERSION = 0.1
# Don't forget to change META file as well

OCAMLC = ocamlc
OCAMLOPT = ocamlopt
COMPFLAGS = -w +A-4-17-44-45 -I +compiler-libs

.PHONY: all
all: genlifter.exe dumpast.exe ppx_metaquot.exe

genlifter.exe: genlifter.cmo
	$(OCAMLC) $(COMPFLAGS) -o genlifter.exe ocamlcommon.cma genlifter.cmo

dumpast.exe: ast_lifter.cmo dumpast.cmo
	$(OCAMLC) $(COMPFLAGS) -o dumpast.exe ocamlcommon.cma ast_lifter.cmo dumpast.cmo


ppx_metaquot.exe: ast_lifter.cmo ppx_metaquot.cmo
	$(OCAMLC) $(COMPFLAGS) -o ppx_metaquot.exe ocamlcommon.cma ast_lifter.cmo ppx_metaquot.cmo


ast_lifter.ml: genlifter.exe
	./genlifter.exe -I +compiler-libs Parsetree.expression > ast_lifter.ml || rm -rf ast_lifter.ml

.PHONY: clean
clean:
	rm -f *.cm* *.exe *~ *.o *.obj *.tar.gz
	rm -f ast_lifter.ml

# Default rules

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(COMPFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(COMPFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(COMPFLAGS) -c $<


# Install/uninstall

INSTALL = META genlifter.exe dumpast.exe ppx_metaquot.exe

.PHONY: install
install:
	ocamlfind install $(PACKAGE) $(INSTALL)

.PHONY: uninstall
uninstall:
	ocamlfind remove $(PACKAGE)

# Packaging

DISTRIB = \
  README.md LICENSE META \
  Makefile \
  dumpast.ml \
  genlifter.ml \
  ppx_metaquot.ml

FPACKAGE = $(PACKAGE)-$(VERSION)

.PHONY: package
package: clean
	rm -rf files.tar.gz $(FPACKAGE) $(FPACKAGE).tar.gz
	tar czf files.tar.gz $(DISTRIB)
	mkdir $(FPACKAGE)
	cd $(FPACKAGE) && tar xzf ../files.tar.gz
	tar czf $(FPACKAGE).tar.gz $(FPACKAGE)
	cd $(FPACKAGE) && make all
	rm -rf files.tar.gz $(FPACKAGE)

TARGET=foo:bar/ppx_tools_data
upload:
	scp $(FPACKAGE).tar.gz $(TARGET)/


