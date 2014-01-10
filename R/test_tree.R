#!/usr/bin/env Rscript

.libPaths(c("lib", .libPaths()))  # will cause us to use local version of tree
library(tree)
library(testthat)
test_dir('tree/inst/tests')
