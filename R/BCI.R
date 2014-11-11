
BCI_download_50ha_plot <- function(dest) {
  if(!file.exists(dest))
   stop(sprintf("Please visit %s and save file as %s. See Readme file for furtehr instructions.",
    "http://ctfs.arnarb.harvard.edu/webatlas/datasets/bci/",
    dest))
}

BCI_load_50ha_plot <- function(path){
  path_to_BCI_data= dirname(path)
  files <- data.frame(year = c(1990, 1995, 2000, 2005, 2010),
            name = paste0(path_to_BCI_data, "/bci.full", 3:7, ".csv"), stringsAsFactors = FALSE)
  tbl_df(ddply(files, "year", function(x) read.csv(x$name, stringsAsFactors = FALSE)))
}

BCI_get_individual_growth <- function(data_in){

  # sort data so that ordered by species
  data <- data_in %.% arrange(sp, treeID, year) %.%
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

  growth.indiv
}

BCI_get_species_maximums <- function(growth.indiv, wright_2010) {

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
  wright_2010 <- wright_2010 %.%
    mutate(species=paste(genus, species))

  species.data <- merge(select(wright_2010, species, lma, rho, hmat), out, by="species") %.%
    filter(!is.na(dbh.gr)) %.%
    arrange(species, at)

  names(species.data) <- tolower(names(species.data))

  names(species.data )[names(species.data) == "dbh.gr"] <- "dbasal_diam_dt"
  species.data$dbasal_diam_dt <- species.data$dbasal_diam_dt/1000 #(convert from mm to m)

  species.data
}

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

# fits quantile regression to data
fit.quantreg <- function(y,x,tau){
	rq(y~log10(x),tau=tau)
}

# Estimates value for y at given x (predict.at) by fitting quantile regression.
# The cutrade-off value (p) for the gression is adjusted according to the number of points in dataset. If there are few points, use default value for p. Otherwise cutrade-off based on top 20 points
# Returns predicted value of y at given value of x (predict.at)
quantreg95 <- function(x,y, p=0.99, nmin=50, predict.at=10){
  n=length(y)
  if(n<nmin) return(as.numeric(NA))
  f <- fit.quantreg(y, x, p)
  predict(f, newdata=data.frame(x=predict.at))
}

# plot to demonstarte the qunatile gression method for estimating growth rates
quantreg.plot <- function(x,y, p=0.99, nmin=100, predict.at=10,add.legend=TRUE, title,...){
  n=length(y)
  plot(y~x, log="x", main=paste0(title,", n=",n),...)
  if(n > nmin){
    f <- fit.quantreg(y, x, p)
    abline(h=quantile(y, probs=0.95), col="red")
    abline(f, col="blue")
    points(predict.at, predict(f, newdata=data.frame(x=predict.at)), col="blue", pch=16)
  }
  if(add.legend)
    legend("topright", legend = c("Wright2010", "QuantReg"), col=c("red", "blue"), lty=c("solid", "solid"), bty = "n")
}

GR_plot <- function(X, Y, cex=1,col = "darkgreen", pch=1, ylim= range(Y, na.rm=TRUE), axes=c("lma","dbasal_diam_dt"), ytick.lab = TRUE, ...){
  if(ytick.lab)
    new_plot(axes[1],axes[2], log="xy", ylim=ylim, xlab=NULL, ylab=NULL, ...)
  else
    new_plot(axes[1],axes[2], log="xy", ylim=ylim, xlab=NULL, ylab=NULL, ytick.lab=NA, ...)

  sm <- sma(Y~X,log='xy', method="OLS")
  points(X, Y, col = "black", pch=pch, type='p', cex=cex)
  plot(sm, add=T,  col = col,pch=pch, lwd = 2, type='l', p.lines.transparent = 0.1)

  lab = paste0(sum(!is.na(Y) &!is.na(X) ), " species\nr2 = ", format(sm$groupsummary$r2, digits=2))
  legend("topright", legend = lab, bty = "n")
}

panel <-function(dat, axes=c("lma","dbasal_diam_dt"), xlim=c(0.02, 0.16), ylim=c(1,20)/1000, at=c(10,20,60,120), title=FALSE) {

  trait <- axes[[1]]
  for(a in at){
      dat.sub <- dat[dat$at==a & dat$count > 800,]
      if(a == at[1])
        ytick <- TRUE
      else
        ytick <- FALSE
      GR_plot(X = dat.sub[[trait]], Y = dat.sub[[axes[2]]], xlim=xlim, ylim=ylim,
        cex=linear.rescale(log10(dat.sub$count),c(0.2, 2.5), log10(c(800,10000))),
        ytick.lab = ytick)
      if(title)
        title(paste0("dbh=",a,"mm"))
  }
}


fig_BCI_data <- function(data, at=c(10,20,60,120)) {

  op <- par(oma=c(4,6,2,1), mar=c(1,1,2,1))

  nrows <- 3
  ncols <- length(at)

  m <- matrix(
    rep(c(1:ncols, rep(ncols+1, ncols)), nrows) + sort(rep(0:(nrows-1), ncols*2))*(ncols+1),
    ncol = ncols, byrow = TRUE)
  layout(m, widths = rep(1/ncols, ncols),heights = rep(c(0.8, 0.2)/nrows, nrows))

  panel(data, axes=c("lma","dbasal_diam_dt"), at=at)
  headerplot(get.axis.info("lma","lab"))

  panel(data, axes=c("rho","dbasal_diam_dt"), xlim=c(200,1000), at=at)
  headerplot(get.axis.info("rho","lab"))

  panel(data, axes=c("hmat","dbasal_diam_dt"), xlim=c(2,50), at=at)
  headerplot(get.axis.info("hmat","lab"))

  mtext(get.axis.info("dbasal_diam_dt","lab"), line = 4, side = 2, cex=1, outer = TRUE)
  par(op)
}
