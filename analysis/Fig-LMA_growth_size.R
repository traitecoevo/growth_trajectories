#!/usr/bin/env Rscript

source("R/tree-fun.R")
source("R/axis-fun.R")


figure <- function(yvar="height_growth_rate", ylab=get.axis.info(2,"lab"), log="x", ...){

	op <- par(oma=c(4,4,1,1), mfrow = c(1,3))

	lma <- 10^seq(-1.5, 0.5, length.out=50)
	for(h in c(0.25, 2, 15)){
		x <- change_with_trait(lma, "lma", h=h)
		new_plot(0,2, log=log, xlab=NULL, ylab=NULL, ...)
		points(lma, x$vars[[yvar]], type='l')
	}

	mtext(get.axis.info(0,"lab"), line =1, side = 1, cex=1.5, outer = TRUE)
	mtext(ylab, line =1, side = 2, cex=1.5, outer = TRUE)

	par(op)
}

to.pdf(figure("height_growth_rate"),
	paste0("figs/growth-height.pdf"), height=4, width =12)
to.pdf(figure("dleaf_area_dt", ylab="leaf area growth (m2/yr)", ylim=c(0, 4)),
	paste0("figs/growth-leaf.pdf"), height=4, width =12)
to.pdf(figure("dbasal_area_dt", ylab="basal area growth (m2/yr)", ylim=c(0, 0.0003)),
	paste0("figs/growth-stem.pdf"), height=4, width =12)
to.pdf(figure("net_production", ylab="net production (kg/yr)", ylim=c(0, 1E-1)),
	paste0("figs/growth-production.pdf"), height=4, width =12)
