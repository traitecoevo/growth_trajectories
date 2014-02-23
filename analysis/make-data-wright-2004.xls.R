#!/usr/bin/env Rscript
dir.create("data", FALSE)
url <-
  "http://www.nature.com/nature/journal/v428/n6985/extref/nature02403-s2.xls"
file.xls <- "data/wright-2004.xls"
hash.file.md5 <- "18e91ac6a868300729ebd74262e0f287"
download.file(url, file.xls)
if (tools::md5sum(file.xls) != hash.file.md5)
  warning("Downloaded xls file did not match expected hash",
          immediate.=TRUE)
