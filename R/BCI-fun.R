
# function to calculate growth rate of tree, either absolute (default) or relative rgowth rate (set f=log)
growth.rate <- function(x, t, f=function(y) y){
  dt = diff(t)/365.25
  if(any(dt < 0, na.rm=TRUE)){
    stop("time must be sorted")
	}
  c(diff(f(x))/dt, NA)
}

# Function to identify bad data. Adapted from function in CTFS R package
trim.data.flag <- function(dbh, dbh.incr, dbh.gr, pom.gr, stemID.change,
	slope=0.006214, intercept=.9036, err.limit=4, maxgrow=75, pomcut=0.05) {
	accept <- rep(TRUE,length(dbh))
	# Remove records based on max growth rate
	accept[!is.na(dbh.gr) & dbh.gr > maxgrow] <- FALSE
	# Remove records based on min growth rate, estimated from allowbale error
	allowable.decrease <- -err.limit*(slope*dbh+intercept)
	accept[!is.na(dbh.incr) & dbh.incr < allowable.decrease] <- FALSE
	# Remove trees where point of measurement changes substantially
	accept[!is.na(pom.gr) & pom.gr > pomcut] <- FALSE
	# # remove records where stem change
	#accept[stemID.change !=0] <- FALSE
	accept
}

# lookup full Latin name given 4 or 6 letter abbreviation
lookup.species<-function(tag, chars=6, path='data/Nomenclature_R_20101129_Rready.csv'){

  if(chars == 6) COL <- 6
  if(chars == 4) COL <- 5

  #load lookup table
  nomen <- read.csv(path,fill=TRUE, header=TRUE, stringsAsFactors=FALSE)

  #sort table
  nomen <- nomen[!is.na(nomen[,COL]) &  nomen[,COL]!='',]
  nomen <- nomen[order(nomen[,COL]),]

  i <- match(tolower(tag), tolower(nomen[,COL]))
  paste(nomen$genus, nomen$species)[i]
}
