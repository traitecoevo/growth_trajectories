#!/bin/sh
mkdir -p lib
R CMD INSTALL -l lib --preclean tree
