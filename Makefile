#  This file is part of the ppx_tools package.  It is released
#  under the terms of the MIT license (see LICENSE file).
#  Copyright 2013  Alain Frisch and LexiFi

include $(shell ocamlc -where)/Makefile.config

PACKAGE = ppx_tools
VERSION = 5.0
# Don't forget to change META file as well

OCAMLC = ocamlc -bin-annot
OCAMLOPT = ocamlopt
COMPFLAGS = -w +A-4-17-44-45-105 -I +compiler-libs -safe-string

.PHONY: all
all: genlifter$(EXE) gencopy$(EXE) dumpast$(EXE) ppx_metaquot$(EXE) rewriter$(EXE) ast_mapper_class.cmo ppx_tools.cma migrate_parsetree.cma

ifneq ($(ARCH),none)
all: ppx_tools.cmxa migrate_parsetree.cmxa
ifeq ($(NATDYNLINK),true)
all: ppx_tools.cmxs
endif
endif

genlifter$(EXE): ppx_tools.cma genlifter.cmo
	$(OCAMLC) $(COMPFLAGS) -o genlifter$(EXE) ocamlcommon.cma ppx_tools.cma genlifter.cmo

gencopy$(EXE): ppx_tools.cma gencopy.cmo
	$(OCAMLC) $(COMPFLAGS) -o gencopy$(EXE) ocamlcommon.cma ppx_tools.cma gencopy.cmo

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


.PHONY: clean
clean:
	rm -f *.cm* *~ *.o *.obj *.a *.lib *.tar.gz *.cmxs *.cmt *.cmti
	rm -f genlifter$(EXE) gencopy$(EXE) dumpast$(EXE) ppx_metaquot$(EXE) rewriter$(EXE)
	rm -f OCamlFrontend*.ml OCamlFrontend*.mli
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
targets = $(1).mli $(1).cmi $(1).cmt $(1).cmti $(wildcard $(1).cmx)
INSTALL = META \
   genlifter$(EXE) dumpast$(EXE) ppx_metaquot$(EXE) rewriter$(EXE) \
   ppx_tools.cma $(wildcard ppx_tools.cmxa ppx_tools$(EXT_LIB)) \
   $(wildcard ppx_tools.cmxs) \
   $(call targets,ast_convenience) \
   $(call targets,ast_mapper_class)

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


# Snapshoted versions of Parsetree (and related modules) for different versions of OCaml

OCAMLCURRENT=404
OCAML_FRONTENDS=OCamlFrontend404.mli OCamlFrontend403.mli OCamlFrontend402.mli

$(OCAML_FRONTENDS):
	dos2unix build_frontends.sh
	./build_frontends.sh $(OCAMLCURRENT)

## ./gencopy -I . -map OCamlFrontend403:OCamlFrontend404 OCamlFrontend403.Parsetree.expression > migrate_parsetree_403_404.ml
## ./gencopy -I . -map OCamlFrontend404:OCamlFrontend403 OCamlFrontend404.Parsetree.expression > migrate_parsetree_404_403.ml
## ./gencopy -I . -map OCamlFrontend402:OCamlFrontend403 OCamlFrontend402.Parsetree.expression > migrate_parsetree_402_403.ml

MIGRATE_PARSETREE = \
	migrate_parsetree_403_404.cmo \
	migrate_parsetree_404_403.cmo \
	migrate_parsetree_402_403.cmo

migrate_parsetree.cma: $(MIGRATE_PARSETREE)
	$(OCAMLC) -a -o migrate_parsetree.cma $(MIGRATE_PARSETREE)
migrate_parsetree.cmxa: $(MIGRATE_PARSETREE:.cmo=.cmx)
	$(OCAMLOPT) -a -o migrate_parsetree.cmxa $(MIGRATE_PARSETREE:.cmo=.cmx)



.PHONY: depend
depend: $(OCAML_FRONTENDS)
	touch ast_lifter.ml
	ocamldep *.ml *.mli > .depend
	dos2unix .depend
-include .depend
