#!/usr/bin/env Rscript

source("R/tree-fun.R")
source("R/axis-fun.R")

# growth decomp
figure <- function(){

	op <- par(oma=c(4,4,1,1))

	new_plot(0,4, log="xy")

	lma <- 10^seq(-2, 0.5, length.out=50)
	for(h in c(0.1, 2, 10, 20)){
		x <- log(wplcp_with_trait(lma, "lma", h=h))/-0.5
		points(lma, x, type='l')
		i <- 25-4*floor(log2(22-h))
		text(lma[i], x[i], pos=3, labels=paste0("h=", h), col= "grey")
	}
	par(op)
}

to.pdf(figure(), paste0("output/figs/LMA_wplcp.pdf"), height=6, width=6)
