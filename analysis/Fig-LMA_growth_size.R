#!/usr/bin/env Rscript

source("R/tree-fun.R")
source("R/axis-fun.R")


# growth decomp
figure <- function(){

	op <- par(oma=c(4,4,1,1), mfrow = c(1,3))

	lma <- 10^seq(-1.5, 0.5, length.out=50)
	for(h in c(0.25, 4, 15)){
		x <- change_with_trait(lma, "lma", h=h)
		new_plot(0,2, log="x", xlab=NULL, ylab=NULL)
		points(lma, x$vars_phys[["height_growth_rate"]], type='l')
	}

	mtext(get.axis.info(0,"lab"), line =1, side = 1, cex=1.5, outer = TRUE)
	mtext(get.axis.info(2,"lab"), line =1, side = 2, cex=1.5, outer = TRUE)

	par(op)
}

to.pdf(figure(), paste0("figs/LMA_growth_size.pdf"), height=4, width =12)
