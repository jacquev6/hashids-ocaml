#!/usr/bin/env python3

# Copyright 2018 Vincent Jacques <vincent@vincent-jacques.net>

import sys


def gen(flavor):
    yield '(library'
    yield '  (name Hashids)'
    yield '  (public_name hashids)'
    yield '  (modules (:standard \ Unit_test))'
    yield '  (libraries General)'
    if flavor == "coverage":
        yield "  (preprocess (pps bisect_ppx))"
    yield ')'
    yield ''
    yield '(executable'
    yield '  (name unit_test)'
    yield '  (modules Unit_test)'
    yield '  (libraries hashids)'
    yield ')'
    yield ''
    yield '(rule'
    yield '  (targets unit_test.sentinel)'
    yield '  (deps unit_test.bc)'
    yield '  (action (progn'
    yield '    (run %{exe:unit_test.bc})'
    yield '    (write-file unit_test.sentinel sentinel)'
    yield '  ))'
    yield ')'
    yield ''
    yield '(alias'
    yield '  (name runtest)'
    yield '  (deps unit_test.sentinel)'
    yield ')'

for line in gen(sys.argv[1]):
    print(line)
