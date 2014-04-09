library(plyr)
library(dplyr)
source("R/quant-reg.R")
source('R/BCI-fun.R')
source("R/plot-utils.R")

load("data/BCI_indiv.50ha.Rdata")

data <- mutate(tbl_df(growth.indiv),  # imported data as dplyr tbl object
	keep=trim.data.flag(dbh, dbh.incr, dbh.gr, pom.gr, stemID.change)) %.% # test for bad data
	select(species, keep, status, dbh, dbh.gr, basal.area, basal.area.gr) %.%
	filter(keep & status=="A" & !is.na(dbh.gr))

# bands in which to calculate growth rate
# In all but first band we estimate growth at centre of band.
# The first interval is repeated so that we can estimate growth at LHS also
size.min <- c(10,10,15,20,25,30,40,50,60)
size.range <- data.frame(at=2*size.min,min=size.min,max=4*size.min)
size.range[1,"at"] <- 10

get.species.data <- function(data, min, max, at){
	data %.%
	group_by(species) %.%
	filter(dbh >= min & dbh < max) %.%
	summarise(
		count=n(),
		dbh.gr=quantreg95(dbh, dbh.gr,predict.at=at),
		ba.gr=quantreg95(dbh, basal.area.gr,predict.at=at)
		)
}

out <- ddply(size.range, 1, function(x) get.species.data(data, x$min, x$max, x$at))

# merge with data from Wright 2010
wright.data <- read.table("data/wright-2010.txt", fill=TRUE, header=TRUE,
	stringsAsFactors=FALSE, sep="\t") %.%
	mutate(species=paste(GENUS,SPECIES))

species.data <- merge(select(wright.data,species, LMA, WSG, HEIGHT), out, by="species") %.%
	filter(!is.na(dbh.gr)) %.%
	arrange(species, at)

names(species.data) <- tolower(names(species.data))
write.csv(species.data,"data/BCI_species.csv", quote=FALSE, row.names=FALSE)

# Example plots, shows the relationship between dbh and dbh growth rate for a single species. The red line gives the estimate using the method of Wright 2010, whereas the blue solid line gives the new proposed method. Once the the quantile regression has been fit, we can estimate the growth rate at a given size (blue dot).

# myfun <- function(df, predict.at) quantreg.plot(df$dbh, df$dbh.gr, title=df$species[1], predict.at=predict.at, xlab="DBH", ylab=expression(paste("growth rate (mm ",yr^-1,")")), ylim=c(-5,20))

# to.pdf(
# 	myfun(filter(data, dbh >= 10 & dbh < 40 & species=="Beilschmiedia pendula"), predict.at=20),
# 	file="figs/example-20.pdf", height=6, width=6)

# to.pdf(
# 	myfun(filter(data, dbh >= 20 & dbh < 80 & species=="Beilschmiedia pendula"), predict.at=40),
# 	file="figs/example-40.pdf", height=6, width=6)

# three_plot <- function(){
# 	par(mfrow=c(1,3))
# 	myfun(filter(data, dbh >= 10 & dbh < 40 & species=="Beilschmiedia pendula"), predict.at=20)
# 	myfun(filter(data, dbh >= 20 & dbh < 80 & species=="Beilschmiedia pendula"), predict.at=40)
# 	myfun(filter(data, dbh >= 40 & dbh < 160 & species=="Beilschmiedia pendula"), predict.at=80)
# }

# to.pdf(three_plot(), file="figs/example-three.pdf", height=6, width=9)
