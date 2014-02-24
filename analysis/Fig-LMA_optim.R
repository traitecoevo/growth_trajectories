#!/usr/bin/env Rscript

source("R/tree-fun.R")

# growth decomp
figure <- function(){

	h <- 10^seq(log10(0.2), log10(30), length.out=50)

	plot(h,h*0, type='n', log="y", ylim = c(0.02,2),
		ylab=expression(paste("Leaf mass per area (kg ", m^-2,")")),
		xlab=expression(paste("height(m)")), las=1)

	for(E in seq(0.2,1.0, by = 0.05)){
		x <- trait_maximimum_with_size(h=h, "lma", c(1E-5, 20), E=E)

		points(h, x, type='l')
	}
}

to.pdf(figure(), paste0("figs/LMA_optim.pdf"), height=6, width=6)
