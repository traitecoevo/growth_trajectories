#!/usr/bin/env Rscript

source("R/tree-fun.R")
source("R/axis-fun.R")

# growth decomp
figure <- function(){

	op <- par(oma=c(4,4,1,1))

	new_plot(0,4, log="x")

	lma <- 10^seq(-1.5, 0.5, length.out=50)
	for(h in c(0.25, 1, 5,10,20)){
		x <- wplcp_with_trait(lma, "lma", h=h)
		points(lma, x, type='l')
	}
	par(op)
}

to.pdf(figure(), paste0("figs/LMA_wplcp.pdf"), height=6, width=6)
