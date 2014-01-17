#!/usr/bin/env Rscript

source("R/tree-fun.R")

# growth decomp
figure <- function(){

	lma <- 10^seq(-1.5, 0.5, length.out=50)

	plot(lma,lma*0, type='n', log="x", ylim = c(0,0.5),
		ylab=expression(paste("light compensation point (0-1)")),
		xlab=expression(paste("leaf mass per area (kg", m^{-2},")")))

	for(h in c(0.25, 1, 5,10,20)){
		x <- wplcp_with_trait(lma, "lma", h=h)
		points(lma, x, type='l')
	}
}

to.pdf(figure(), paste0("figs/LMA_wplcp.pdf"), height=6, width=6)
