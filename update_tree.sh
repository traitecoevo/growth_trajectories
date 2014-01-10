#!/bin/sh
make -C tree
mkdir -p lib
R CMD INSTALL -l lib --preclean tree
./R/test_tree.R
