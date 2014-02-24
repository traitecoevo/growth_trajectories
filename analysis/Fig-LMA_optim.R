#!/usr/bin/env Rscript

source("R/tree-fun.R")
source("R/axis-fun.R")

figure <- function(){

	op <- par(oma=c(4,4,1,1))
	new_plot(1,0, log="xy")

	h <- 10^seq(log10(0.2), log10(30), length.out=20)
	for(E in seq(0.2,1.0, by = 0.05)){
		x <- trait_maximimum_with_size(h=h, "lma", c(1E-5, 20), E=E)
		points(h, x, type='l')
	}
	par(op)
}

to.pdf(figure(), paste0("figs/LMA_optim.pdf"), height=6, width=6)
