#!/usr/bin/env Rscript

source("R/tree-fun.R")

# growth decomp
figure <- function(){

	h <- seq(0.1, 50, length.out=30)

	plot(h,h*0, type='n', log="y", ylim = c(0.01,10),
		ylab=expression(paste("leaf mass per area (kg ", m^-2,")")),
		xlab=expression(paste("height(m)")))

	for(E in c(0.2, 0.4, 0.6, 0.8, 1.0)){
		x <- trait_maximimum_with_size(h=h, "lma", c(1E-5, 20), E=E)

		points(h, x, type='l')
	}
}

to.pdf(figure(), paste0("figs/LMA_optim.pdf"), height=6, width=6)
