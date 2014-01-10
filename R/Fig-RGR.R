#!/usr/bin/env Rscript

source("R/plot-utils.R")

require(smatr)

RGR_plot <- function(X, Y, col = "darkgreen", pch=19, ylim= range(Y, na.rm=TRUE)){

  sm <- sma(Y~X,log='xy')
  plot(X, Y, col = make.transparent("grey", 1), pch=pch, type='p', lwd=0, cex=1.2, log="xy", ann=FALSE,
    ylim = ylim, xlim = c(10, 200), las=1)
  plot(sm, add=T,  col = col,pch=pch, lwd = 2, type='l', p.lines.transparent = 0.1)

  lab = paste0(sum(!is.na(Y) &!is.na(X) ), " species\nr2 = ", format(sm$groupsummary$r2, digits=2))
  legend("topright", legend = lab, bty = "n")
}

dat1 <- read.csv("data/Wright&Westoby2001.csv", header = TRUE, stringsAsFactors=FALSE)
dat1$lma <-  1/ dat1$sla *10^3 #(convert from mm2/mg to g/m2)

dat2 <- read.table("data/Wright2010.txt", header = TRUE, sep="\t")


RGR_panel <-function(){

  par(mfrow=c(1,3), oma=rep(6,4))
  RGR_plot(X = dat1$lma, Y = dat1$rgr*365)
  title("Seedlings, mass growth")
  RGR_plot(X =dat2$LMA, Y = dat2$RGR95SAP, ylim=c(0.01, 0.4))
  title("Saplings, diameter growth")

  RGR_plot(X =dat2$LMA, Y = dat2$RGR95TRE, ylim=c(0.01, 0.4))
  title("Trees, diameter growth")
  mtext(expression(paste("Leaf mass per area (g ", m^-2,")")), line =1, side = 1, cex=1.5, outer = TRUE)
  mtext(expression(paste("Relative growth rate (",yr^-1,")")), line =1,side = 2, cex=1.5, outer = TRUE)
}

to.pdf(RGR_panel()
  , paste0("figs/RGR.pdf"), height=6, width =12)
