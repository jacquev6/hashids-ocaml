#!/bin/bash

# Copyright 2016-2017 Vincent Jacques <vincent@vincent-jacques.net>

set -o errexit

eval `opam config env`
opam install --yes core bisect_ppx
pip3 install --user hashids
clear

# Debug, tests, coverage
# ======================

corebuild -no-links -use-ocamlfind -package bisect_ppx -tag debug unit_tests.byte

rm -f bisect????.out
_build/unit_tests.byte
bisect-summary bisect????.out
bisect-ppx-report -html _build/bisect bisect????.out
echo "See coverage report (for General's unit tests) in $(pwd)/_build/bisect/index.html"
rm -f bisect????.out

# OPAM package
# ============

opam pin add --yes --no-action .
opam reinstall --yes hashids

# Examples
# ========

cd examples
# Library doesn't depend on oUnit: no need to link oUnit here. Keep it this way.
corebuild -no-links -use-ocamlfind -package hashids example.byte example.native
diff <(_build/example.native) <(echo "Jys1FWfnhqHy")
cd ..

# Documentation
# =============

if (which sphinxcontrib-ocaml-autodoc && which sphinx-build) >/dev/null
then
    echo
    rm -rf docs _build/sphinx/doctrees
    sphinx-build doc docs -d _build/sphinx/doctrees
    rm -f docs/.buildinfo
    echo "See documentation in $(pwd)/docs/index.html"
fi

echo
echo "Development cycle OK"
