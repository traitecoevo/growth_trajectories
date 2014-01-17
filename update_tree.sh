#!/bin/sh
make -C tree
mkdir -p lib
R CMD INSTALL -l lib tree
./R/test_tree.R
