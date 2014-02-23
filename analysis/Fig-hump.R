#!/usr/bin/env Rscript

source("R/tree-fun.R")

# growth decomp
figure <- function(){
	h <- seq(0.1, 50,length.out=50)
	x <- change_with_size(h=h)
	par(mfrow = c(1,5))

	for(v in c("height_growth_rate","dheight_dleaf_area","leaf_fraction", "growth_fraction","net_production")){
		plot(h, x$vars_growth_decomp[[v]], type='l', xlab = "height (m)", ylab=v)
	}
}

to.pdf(figure(), paste0("figs/hump.pdf"), height=4, width =12)
