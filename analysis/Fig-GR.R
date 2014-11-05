#!/usr/bin/env Rscript

source("R/plot-utils.R")
source("R/axis-fun.R")
require(smatr)

GR_plot <- function(X, Y, cex=1,col = "darkgreen", pch=1, ylim= range(Y, na.rm=TRUE), axes=c("lma","dbh_gr"), ytick.lab = TRUE, ...){
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

panel <-function(dat, axes=c("lma","dbh_gr"), xlim=c(0.02, 0.16), ylim=c(1,20)/1000, at=c(10,20,60,120), title=FALSE) {

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

headerplot <- function(x){
  oldpar <- par("mar")
  par(mar = c(0,0,1,0))
  plot(1,1,type = "n",frame.plot = FALSE,axes = FALSE)
  u <- par("usr")
  text(1,u[4],labels = x,pos = 1, cex = 1.5)
  par(mar = oldpar)
}



fig1 <- function(){

  data <- read.csv("data/BCI_species.csv", header = TRUE, stringsAsFactors=FALSE)

  names(data)[names(data) == "wsg"] <- "rho"
  names(data)[names(data) == "height"] <- "hmat"
  names(data)[names(data) == "dbh.gr"] <- "dbh_gr"

  data$lma <-  data$lma/1000 #(convert from g/2 to kg/m2)
  data$dbh_gr <-  data$dbh_gr/1000 #(convert from mm to m)
  data$rho <- data$rho*1000

  at <- c(10,20,60,120)

  op <- par(oma=c(4,6,2,1), mar=c(1,1,2,1))

  m <- matrix(c(1:4, rep(5,4), 6:9, rep(10,4),  11:14, rep(15,4)),ncol = 4, byrow = TRUE)
  layout(m,widths = rep(0.25, 4),heights = c(0.27,0.063,0.27,0.063,0.27,0.063))

  panel(data, axes=c("lma","dbh_gr"), at=at)
  headerplot(get.axis.info("lma","lab"))

  panel(data, axes=c("rho","dbh_gr"), xlim=c(200,1000), at=at)
  headerplot(get.axis.info("rho","lab"))

  panel(data, axes=c("hmat","dbh_gr"), xlim=c(2,50), at=at)
  headerplot(get.axis.info("hmat","lab"))

  mtext(get.axis.info("dbh_gr","lab"), line = 4, side = 2, cex=1, outer = TRUE)
  par(op)
}

to.pdf(fig1(), paste0("output/figs/GR-LCC.pdf"), height=12, width=12)
