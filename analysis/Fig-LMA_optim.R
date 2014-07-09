#!/usr/bin/env Rscript

source("R/tree-fun.R")
source("R/axis-fun.R")

figure <- function(){

	op <- par(oma=c(4,4,1,1))
	new_plot(1,0, log="xy",  ylim=c(0.01,2.6))

	h <- 10^seq(log10(0.2), log10(30), length.out=50)
	for(E in exp(-0.5*c(3, 2, 1, 0.1))) {
		x <- trait_maximimum_with_size(h=h, "lma", c(1E-5, 20), E=E)
		points(h, x, type='l')
		i <- 20
		text(h[i], x[i], pos=3, labels=paste0( format(-log(E)/0.5,digits=1),"m2"), col= "grey")
	}
	par(op)
}

to.pdf(figure(), paste0("output/figs/LMA_optim.pdf"), height=6, width=6)
