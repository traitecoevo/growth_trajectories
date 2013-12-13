#!/usr/bin/env Rscript
library(testthat)
.libPaths(c("lib", .libPaths()))  # will cause us to use local version of tree
test_dir("tree/inst/tests")
