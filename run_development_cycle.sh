#!/bin/bash

# Copyright 2016 Vincent Jacques <vincent@vincent-jacques.net>

set -o verbose
set -o errexit

# Debug, tests, coverage
# ======================

corebuild -no-links -use-ocamlfind -pkg oUnit -pkg bisect -syntax camlp4o -tag debug -cflags -w,@a-44,-strict-sequence unit_tests.byte
cd _build
rm -f bisect????.out
./unit_tests.byte -shards 1  # Only one shard because I don't know how to use bisect with multiple threads
bisect-report -html coverage bisect0001.out
bisect-report -text >(grep total | head -n 1) bisect0001.out
echo "See test coverage in $(pwd)/coverage/index.html"
cd ..

# OPAM package
# ============

opam remove hashids || echo "Not yet installed. OK"
opam pin --yes add hashids .
cd examples
# Library doesn't depend on oUnit: no need to link oUnit here. Keep it this way.
corebuild -no-links -use-ocamlfind -package hashids example.byte example.native
diff <(_build/example.native) <(echo "Jys1FWfnhqHy")
cd ..

# Documentation
# =============

rm -rf _build/doc
if which sphinx-build
then
    sphinx-build doc _build/doc
    echo "See documentation in $(pwd)/_build/doc/index.html"
fi

echo
echo "Development cycle OK"
