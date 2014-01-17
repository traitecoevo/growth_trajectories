#!/usr/bin/env Rscript

source("R/tree-fun.R")

# growth decomp
figure <- function(){

	par(mfrow = c(1,3))
	lma <- 10^seq(-1.5, 0.5, length.out=50)
	for(h in c(0.25, 4, 15)){
		x <- change_with_trait(lma, "lma", h=h)
		plot(lma, x$vars_phys[["height_growth_rate"]], type='l', log="x", ylim = c(0,1.5), main = paste0("height =", h,"m"),
			ylab=expression(paste(" height growth rate (m ",yr^{-1},")")),
			xlab=expression(paste("leaf mass per area (kg", m^{-2},")")))
	}
}

to.pdf(figure(), paste0("figs/LMA_growth_size.pdf"), height=4, width =12)
