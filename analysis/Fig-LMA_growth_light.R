#!/usr/bin/env Rscript

source("R/tree-fun.R")
source("R/axis-fun.R")

# growth decomp
figure <- function(h=0.25){
	op <- par(oma=c(4,4,1,1))

	new_plot(0,2, log="x", ylim = c(0, 0.5),ytick=seq(0, 0.5, by=0.1), ytick.lab=seq(0, 0.5, by=0.1))

	lma <- 10^seq(-2, 1, length.out=50)
	for(E in seq(0.2, 1, by = 0.2)){
		x <- change_with_trait(lma, "lma", h=h, E=E)
		points(lma, x$vars[["height_growth_rate"]], type='l')
	}
	par(op)
}

to.pdf(figure(h=0.25), paste0("figs/growth-light-1.pdf"), height=8, width=8)
to.pdf(figure(h=2), paste0("figs/growth-light-2.pdf"), height=8, width=8)
to.pdf(figure(h=10), paste0("figs/growth-light-10.pdf"), height=8, width=8)
