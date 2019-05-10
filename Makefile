#  This file is part of the ppx_tools package.  It is released
#  under the terms of the MIT license (see LICENSE file).
#  Copyright 2013  Alain Frisch and LexiFi

PACKAGE = ppx_tools
VERSION = 5.0
# Don't forget to change META file as well

.PHONY: all
all:
	dune build @install

clean:
	dune clean

# Packaging

DISTRIB = \
  README.md LICENSE \
  Makefile dune ppx_tools.opam \
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
