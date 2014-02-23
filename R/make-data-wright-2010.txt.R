#!/usr/bin/env Rscript
dir.create("data", FALSE)
url <- "http://www.esapubs.org/archive/ecol/E091/257/Supplement_20100505.txt"
file.txt <- "data/wright-2010.txt"
hash.file.md5 <- "5c9728eb3aa581c0979d7dcce0a24167"
download.file(url, file.txt)
if (tools::md5sum(file.txt) != hash.file.md5)
  warning("Downloaded txt file did not match expected hash",
          immediate.=TRUE)
