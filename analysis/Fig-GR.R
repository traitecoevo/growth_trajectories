#!/usr/bin/env Rscript

source("R/plot-utils.R")
source("R/axis-fun.R")
require(smatr)

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


fig1 <- function(){

  data <- read.csv("data/BCI_species.csv", header = TRUE, stringsAsFactors=FALSE)

  names(data)[names(data) == "wsg"] <- "rho"
  names(data)[names(data) == "height"] <- "hmat"
  names(data)[names(data) == "dbh.gr"] <- "dbasal_diam_dt"

  data$lma <-  data$lma/1000 #(convert from g/2 to kg/m2)
  data$dbasal_diam_dt <-  data$dbasal_diam_dt/1000 #(convert from mm to m)
  data$rho <- data$rho*1000

  at <- c(10,20,60,120)

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

to.pdf(fig1(), paste0("output/figs/GR-LCC.pdf"), height=12, width=12)
