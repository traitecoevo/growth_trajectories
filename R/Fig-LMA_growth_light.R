#!/usr/bin/env Rscript

source("R/tree-fun.R")

# growth decomp
figure <- function(){

	lma <- 10^seq(-1.5, 0.5, length.out=50)

	plot(lma,lma*0, type='n', log="x", ylim = c(0,0.5),
		ylab=expression(paste(" height growth rate (m ",yr^{-1},")")),
		xlab=expression(paste("leaf mass per area (kg", m^{-2},")")))

	for(E in seq(0.2, 1, by = 0.2)){
		x <- change_with_trait(lma, "lma", h=0.25, E=E)
		points(lma, x$vars_phys[["height_growth_rate"]], type='l')
	}
}

to.pdf(figure(), paste0("figs/LMA_growth_light.pdf"), height=6, width =6)
