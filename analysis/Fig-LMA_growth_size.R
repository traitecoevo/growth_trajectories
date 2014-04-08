#!/usr/bin/env Rscript

source("R/tree-fun.R")
source("R/axis-fun.R")


figure <- function(yvar="height_growth_rate", ylab=get.axis.info(2,"lab"), log="x", ...){

	op <- par(oma=c(4,4,1,1), mfrow = c(1,3))

	lma <- 10^seq(-1.5, 0.5, length.out=50)
	for(h in c(0.25, 2, 15)){
		x <- change_with_trait(lma, "lma", h=h)
		ylim <- c(0, max(x$vars[[yvar]], na.rm=TRUE)*1.2)
		new_plot(0,2, log=log, xlab=NULL, ylab=NULL, ylim=ylim,ytick=pretty(ylim),...)
		points(lma, x$vars[[yvar]], type='l')
		title(paste0("h=", h,"m"))
	}

	mtext(get.axis.info(0,"lab"), line =1, side = 1, cex=1, outer = TRUE)
	mtext(ylab, line =1, side = 2, cex=1, outer = TRUE)

	par(op)
}

to.pdf(figure("height_growth_rate"),
	paste0("figs/growth-height.pdf"), height=4, width=8)
to.pdf(figure("dleaf_area_dt", ylab=expression(paste("Leaf area growth (", m^2, " ",yr^-1,")"))),
	paste0("figs/growth-leaf.pdf"), height=4, width=8)
to.pdf(figure("dbasal_area_dt", ylab=expression(paste("Basal area growth (", m^2, " ", yr^-1,")"))),
	paste0("figs/growth-stem.pdf"), height=4, width=8)
to.pdf(figure("net_production", ylab=expression(paste("Net production (kg ", yr^-1,")"))),
	paste0("figs/growth-production.pdf"), height=4, width =8)
