#  This file is part of the ppx_tools package.  It is released
#  under the terms of the MIT license (see LICENSE file).
#  Copyright 2013  Alain Frisch and LexiFi

INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

default:
	dune build

release:
	dune build -p ppx_tools

install:
	dune install $(INSTALL_ARGS)

uninstall:
	dune uninstall $(INSTALL_ARGS)

clean:
	dune clean

all-supported-ocaml-versions:
	dune build @install @runtest --workspace dune-workspace.dev
