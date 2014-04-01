#!/bin/sh
make -C tree
# mkdir -p lib
# R CMD INSTALL -l lib tree
R CMD INSTALL tree
./analysis/test_tree.R
