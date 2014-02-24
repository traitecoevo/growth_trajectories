#!/usr/bin/env Rscript

source("R/plot-utils.R")
source("R/axis-fun.R")

require(smatr)

RGR_plot <- function(X, Y, col = "darkgreen", pch=19, ylim= range(Y, na.rm=TRUE)){

  new_plot(0,5, log="xy", xlim=c(0.01, 0.16), ylim=ylim, xlab=NULL, ylab=NULL)

  sm <- sma(Y~X,log='xy')
  points(X, Y, col = make.transparent("grey", 1), pch=pch, type='p', lwd=0, cex=1.2)
  plot(sm, add=T,  col = col,pch=pch, lwd = 2, type='l', p.lines.transparent = 0.1)

  lab = paste0(sum(!is.na(Y) &!is.na(X) ), " species\nr2 = ", format(sm$groupsummary$r2, digits=2))
  legend("topright", legend = lab, bty = "n")
}

RGR_panel <-function(){

  dat1 <- read.csv("data/Wright&Westoby2001.csv", header = TRUE, stringsAsFactors=FALSE)
  dat1$lma <-  1/ dat1$sla #(convert from mm2/mg to kg/m2)

  dat2 <- read.table("data/Wright-2010.txt", header = TRUE, sep="\t", skip=25, na.strings = c("NA","-99", "-99.0", "-99.00", "-99.000", "-99.0000" ), stringsAsFactors=FALSE)
  dat2$LMA <-  dat2$LMA/1000 #(convert from g/2 to kg/m2)

  op <- par(mfrow=c(1,3), oma=rep(6,4))
  RGR_plot(X = dat1$lma, Y = dat1$rgr*365)
  title("Seedlings, mass growth")
  RGR_plot(X =dat2$LMA, Y = dat2$RGR95SAP, ylim=c(0.01, 0.4))
  title("Saplings, diameter growth")

  RGR_plot(X =dat2$LMA, Y = dat2$RGR95TRE, ylim=c(0.01, 0.4))
  title("Trees, diameter growth")

  mtext(get.axis.info(0,"lab"), line =1, side = 1, cex=1.5, outer = TRUE)
  mtext(get.axis.info(5,"lab"), line =1, side = 2, cex=1.5, outer = TRUE)

  par(op)
}

to.pdf(RGR_panel()
  , paste0("figs/RGR.pdf"), height=6, width =12)
