#!/usr/bin/env Rscript

source("R/plot-utils.R")
source("R/axis-fun.R")
require(smatr)

GR_plot <- function(X, Y, cex=1,col = "darkgreen", pch=1, ylim= range(Y, na.rm=TRUE),axes=c(0,5),...){

  new_plot(axes[1],axes[2], log="xy", ylim=ylim, xlab=NULL, ylab=NULL,...)

  sm <- sma(Y~X,log='xy', method="OLS")
  points(X, Y, col = "black", pch=pch, type='p', lwd=0, cex=cex)
  plot(sm, add=T,  col = col,pch=pch, lwd = 2, type='l', p.lines.transparent = 0.1)

  lab = paste0(sum(!is.na(Y) &!is.na(X) ), " species\nr2 = ", format(sm$groupsummary$r2, digits=2))
  legend("topright", legend = lab, bty = "n")
}

panel <-function(dat, trait, axes=c(0,5), xlim=c(0.02, 0.16), ylim=c(1,20), at=c(10,20,60,120)) {

  op <- par(mfrow=c(1,length(at)), oma=c(4,4,2,1))

  for(a in at){
      dat.sub <- dat[dat$at==a & dat$count > 800,]
      GR_plot(X = dat.sub[[trait]], Y = dat.sub$dbh.gr, xlim=xlim, ylim=ylim,
        cex=linear.rescale(log10(dat.sub$count),c(0.2, 2.5), log10(c(800,10000))) )
      title(paste0("dbh=",a,"mm"))
  }

  mtext(get.axis.info(axes[1],"lab"), line =1, side = 1, cex=1, outer = TRUE)
  mtext(get.axis.info(axes[2],"lab"), line =1, side = 2, cex=1, outer = TRUE)

  par(op)
}

data <- read.csv("data/BCI_species.csv", header = TRUE, stringsAsFactors=FALSE)
data$lma <-  data$lma/1000 #(convert from g/2 to kg/m2)

to.pdf(panel(data, "lma"), paste0("output/figs/GR-LCC.pdf"), height=4, width=12)
to.pdf(panel(data, "height", axes=c(6,5), xlim=c(5,40)), paste0("output/figs/GR-Hmax.pdf"), height=4, width=12)
to.pdf(panel(data, "wsg", axes=c(7,5), xlim=c(0.2,1)), paste0("output/figs/GR-WCC.pdf"), height=4, width=12)
