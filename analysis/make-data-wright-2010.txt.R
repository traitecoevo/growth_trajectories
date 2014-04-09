#!/usr/bin/env Rscript
dir.create("data", FALSE)
url <- "http://www.esapubs.org/archive/ecol/E091/257/Supplement_20100505.txt"
file.txt <- "data/wright-2010.txt"
hash.file.md5 <- "5c9728eb3aa581c0979d7dcce0a24167"
download.file(url, file.txt)
if (tools::md5sum(file.txt) != hash.file.md5)
  warning("Downloaded txt file did not match expected hash",
          immediate.=TRUE)

# Correct names of species, anything with length > 12 was clipped
corrections <- data.frame(OLD=NULL, NEW=NULL)

corrections <- data.frame(rbind(c("costaricensi","costaricensis"),
	c("Beilschmiedi","Beilschmiedia"),
	c("megalophyllu","megalophyllum"),
	c("Chrysochlamy","Chrysochlamys"),
	c("Chrysophyllu","Chrysophyllum"),
	c("manzinellens","manzinellensis"),
	c("billbergianu","billbergianus"),
	c("costaricensi","costaricensis"),
	c("coloradoensi","coloradoensis"),
	c("alchorneoide","alchorneoides"),
	c("Tabernaemont","Tabernaemontana"),
	c("Trattinnicki","Trattinnickia"),
	c("$",""),
	c("-99.000000", "NA"),
	c("-99.00000", "NA"),
	c("-99.0000", "NA"),
	c("-99.000", "NA"),
	c("-99.00", "NA"),
	c("-99.0", "NA"),
	c("-99", "NA")
	), stringsAsFactors=FALSE)
names(corrections) <- c("OLD", "NEW")

x <- readLines(file.txt)[26:160]

for(i in 1:nrow(corrections))
	x <- gsub(corrections$OLD[i], corrections$NEW[i],x, fixed=TRUE)

cat(x, file=file.txt, sep="\n")
