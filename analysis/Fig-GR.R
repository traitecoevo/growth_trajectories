#!/usr/bin/env Rscript

source("R/plot-utils.R")
source("R/axis-fun.R")
require(smatr)

GR_plot <- function(X, Y, cex=1,col = "darkgreen", pch=1, ylim= range(Y, na.rm=TRUE),axes=c(0,5), ytick.lab = TRUE, ...){
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

panel <-function(dat, trait, axes=c(0,5), xlim=c(0.02, 0.16), ylim=c(1,20), at=c(10,20,60,120), title=FALSE) {

  for(a in at){
      dat.sub <- dat[dat$at==a & dat$count > 800,]
      if(a == at[1])
        ytick <- TRUE
      else
        ytick <- FALSE
      GR_plot(X = dat.sub[[trait]], Y = dat.sub$dbh.gr, xlim=xlim, ylim=ylim,
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
  data$lma <-  data$lma/1000 #(convert from g/2 to kg/m2)

  at <- c(10,20,60,120)

  op <- par(oma=c(4,4,2,1), mar=c(1,1,2,1))

  m <- matrix(c(1:4, rep(5,4), 6:9, rep(10,4),  11:14, rep(15,4)),ncol = 4, byrow = TRUE)
  layout(m,widths = rep(0.25, 4),heights = c(0.27,0.063,0.27,0.063,0.27,0.063))

  panel(data, "lma", axes=c(0,5), at=at)
  headerplot(get.axis.info(0,"lab"))

  panel(data, "wsg", axes=c(7,5), xlim=c(0.2,1), at=at)
  headerplot(get.axis.info(7,"lab"))

  panel(data, "height", axes=c(6,5), xlim=c(2,50), at=at)
  headerplot(get.axis.info(6,"lab"))

  mtext(get.axis.info(5,"lab"), line = 2, side = 2, cex=1, outer = TRUE)
  par(op)
}

fig1()

# to.pdf(, paste0("output/figs/GR-LCC.pdf"), height=4, width=12)
# to.pdf(panel(data, "height", axes=c(6,5), xlim=c(5,40)), paste0("output/figs/GR-Hmax.pdf"), height=4, width=12)
# to.pdf(panel(data, "wsg", axes=c(7,5), xlim=c(0.2,1)), paste0("output/figs/GR-WCC.pdf"), height=4, width=12)
