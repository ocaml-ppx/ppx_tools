#  This file is part of the ppx_tools package.  It is released
#  under the terms of the MIT license (see LICENSE file).
#  Copyright 2013  Alain Frisch and LexiFi

include $(shell ocamlc -where)/Makefile.config

PACKAGE = ppx_tools
VERSION = 5.0

COMPFLAGS = -w +A-4-17-44-45 -I +compiler-libs -safe-string

.PHONY: all
all: _obuild
	ocp-build

_obuild: Makefile
	ocp-build init

.PHONY: clean
clean:
	rm -rf ocp-build

# Install/uninstall
targets = $(1).mli $(1).cmi $(1).cmt $(1).cmti $(wildcard $(1).cmx)
INSTALL = META \
   _obuild/ppx_tools/ppx_tools.cma \
   $(wildcard _obuild/ppx_tools/ppx_tools.cmxa _obuild/ppx_tools/ppx_tools$(EXT_LIB)) \
   $(wildcard _obuild/ppx_tools/ppx_tools.cmxs) \
   $(call targets,_obuild/ppx_tools/ast_convenience) \
   $(call targets,_obuild/ppx_tools/ast_mapper_class)

.PHONY: install
install:
	ocamlfind install $(PACKAGE) $(INSTALL)
	cp -f _obuild/genlifter/genlifter.asm $(BINDIR)/genlifter$(EXE)
	cp -f _obuild/dumpast/dumpast.asm $(BINDIR)/dumpast$(EXE)
	cp -f _obuild/ppx_metaquot/ppx_metaquot.asm $(BINDIR)/ppx_metaquot$(EXE)
	cp -f _obuild/rewriter/rewriter.asm $(BINDIR)/rewriter$(EXE)

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
  ast_mapper_class.ml ast_mapper_class.mli \
  build.ocp

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
