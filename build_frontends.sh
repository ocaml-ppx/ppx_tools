#!/bin/bash

OCAML_VERSION=$1
OCAML_VERSIONS="402 403 404"

for V in $OCAML_VERSIONS
do
    if [ "$OCAML_VERSION" = "$V" ]; then
        echo "=> OCamlFrontend$V.mli (current version)"
        (cd ocaml_parsetrees/$V &&
            echo "(* GENERATED FILE.  DO NOT MODIFY BY HAND *)" &&
            echo "module Location = Location"
            echo "module Longident = Longident"
            echo "module Asttypes = Asttypes"
            echo "module Parsetree = Parsetree"
            echo ""
        ) > OCamlFrontend$V.mli
    else
        echo "=> OCamlFrontend$V.mli"
        (cd ocaml_parsetrees/$V &&
            echo "(* GENERATED FILE.  DO NOT MODIFY BY HAND *)" &&
            echo "module Location : sig" && cat location.mli && echo "end" &&
            echo "module Longident : sig" && cat longident.mli && echo "end" &&
            echo "module Asttypes : sig" && cat asttypes.mli && echo "end" &&
            echo "module Parsetree : sig" && cat parsetree.mli && echo "end" &&
            echo ""
        ) > OCamlFrontend$V.mli
    fi
done
