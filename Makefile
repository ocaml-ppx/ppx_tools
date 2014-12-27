#  This file is part of the ppx_tools package.  It is released
#  under the terms of the MIT license (see LICENSE file).
#  Copyright 2013  Alain Frisch and LexiFi

include $(shell ocamlc -where)/Makefile.config

PACKAGE = ppx_tools
VERSION = 0.99
# Don't forget to change META file as well

CPPO      = cppo
OCAML     = ocaml
OCAMLC    = ocamlc
OCAMLOPT  = ocamlopt
COMPFLAGS = -w +A-4-17-44-45 -I +compiler-libs -safe-string \
						-pp '$(CPPO) -V OCAML:$(OCAML_VERSION)'

OCAML_VERSION := $(shell $(OCAML) -vnum)

.PHONY: all
all: genlifter$(EXE) dumpast$(EXE) ppx_metaquot$(EXE) rewriter$(EXE) ast_mapper_class.cmo ppx_tools.cma
all: ppx_tools.cmxa ppx_tools.cmxs

genlifter$(EXE): ppx_tools.cma genlifter.cmo
	$(OCAMLC) $(COMPFLAGS) -o genlifter$(EXE) ocamlcommon.cma ppx_tools.cma genlifter.cmo

dumpast$(EXE): dumpast.cmo
	$(OCAMLC) $(COMPFLAGS) -o dumpast$(EXE) ocamlcommon.cma ocamlbytecomp.cma ast_lifter.cmo dumpast.cmo

ppx_metaquot$(EXE): ppx_metaquot.cmo
	$(OCAMLC) $(COMPFLAGS) -o ppx_metaquot$(EXE) ocamlcommon.cma ppx_tools.cma ast_lifter.cmo ppx_metaquot.cmo

rewriter$(EXE): rewriter.cmo
	$(OCAMLC) $(COMPFLAGS) -o rewriter$(EXE) ocamlcommon.cma rewriter.cmo

ast_lifter.ml: genlifter$(EXE)
	./genlifter$(EXE) -I +compiler-libs Parsetree.expression > ast_lifter.ml || rm -rf ast_lifter.ml


OBJS = ast_convenience.cmo ast_mapper_class.cmo

ppx_tools.cma: $(OBJS)
	$(OCAMLC) -a -o ppx_tools.cma $(OBJS)
ppx_tools.cmxa: $(OBJS:.cmo=.cmx)
	$(OCAMLOPT) -a -o ppx_tools.cmxa $(OBJS:.cmo=.cmx)
ppx_tools.cmxs: $(OBJS:.cmo=.cmx)
	$(OCAMLOPT) -shared -o ppx_tools.cmxs -linkall ppx_tools.cmxa


.PHONY: depend
depend:
	touch ast_lifter.ml
	ocamldep *.ml *.mli > .depend
-include .depend


.PHONY: clean
clean:
	rm -f *.cm* *~ *.o *.obj *.a *.lib *.tar.gz *.cmxs
	rm -f genlifter$(EXE) dumpast$(EXE) ppx_metaquot$(EXE)
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

INSTALL = META \
   genlifter$(EXE) dumpast$(EXE) ppx_metaquot$(EXE) rewriter$(EXE) \
   ppx_tools.cma ppx_tools.cmxa ppx_tools$(EXT_LIB) \
   ppx_tools.cmxs \
   ast_convenience.cmi ast_convenience.cmx \
   ast_mapper_class.cmi ast_mapper_class.cmx

.PHONY: install
install:
	ocamlfind install $(PACKAGE) $(INSTALL)

.PHONY: uninstall
uninstall:
	ocamlfind remove $(PACKAGE)

# Packaging

DISTRIB = \
  README.md LICENSE META \
  Makefile .depend \
  dumpast.ml \
  genlifter.ml \
  ppx_metaquot.ml \
  rewriter.ml \
  ast_mapper_class.ml ast_mapper_class.mli

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


