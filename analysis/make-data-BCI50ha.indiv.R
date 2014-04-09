library(plyr)
library(dplyr)
source('R/BCI-fun.R')

path_to_BCI_data <- "~/Dropbox/data/plant/BCI/data_BCI/rawdata/50ha"

#Load 50ha census data
files <- data.frame(year = c(1990, 1995, 2000, 2005, 2010),
					name = paste0(path_to_BCI_data, "/bci.full", 3:7, ".csv"), stringsAsFactors = FALSE)
data <-  tbl_df(ddply(files, "year", function(x) read.csv(x$name, stringsAsFactors = FALSE)))

# sort data so that ordered by species
data <- data %.% arrange(sp, treeID, year) %.%
	select(sp, treeID, stemID, year, date, status, pom, dbh, agb) %.%
	mutate(species=lookup.species(sp)) # get full species name)

# set agb to NA when dbh = NA
data$agb[is.na(data$dbh)] <- NA

# calculate growth for each tree in each census
growth.indiv <- data %.%
	group_by(treeID) %.%
	mutate(
		dbh.incr = c(diff(dbh), NA),
		pom.gr = c(diff(pom), NA)/pom,
		stemID.change = c(diff(stemID),NA),
		dbh.gr = growth.rate(dbh, date),
		dbh.rgr = growth.rate(dbh, date, log),
		basal.area = 0.25*pi*dbh^2,
		basal.area.gr = growth.rate(basal.area, date))

# save to file
save(growth.indiv, file = "data/BCI_indiv.50ha.Rdata")
