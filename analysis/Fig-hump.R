#!/usr/bin/env Rscript

source("R/tree-fun.R")

# growth decomp
figure_height <- function(){
	h <- seq(0.1, 50,length.out=50)
	x <- change_with_size(h=h)
	par(mfrow = c(1,5))

	for(v in c("height_growth_rate","dheight_dleaf_area","leaf_fraction", "growth_fraction","net_production")){
		plot(h, x$vars_growth_decomp[[v]], type='l', xlab = "height (m)", ylab=v)
	}
}

to.pdf(figure_height(), paste0("figs/hump.pdf"), height=4, width =12)


# decomposition of leaf area growth rate
figure_leaf <- function(){
	h <- seq(0.1, 50,length.out=50)
	x <- change_with_size(h=h)
	par(mfrow = c(1,4))

	for(v in c("dleaf_area_dt","leaf_fraction", "growth_fraction","net_production")){
		plot(h, x$vars_growth_decomp[[v]], type='l', xlab = "height (m)", ylab=v)
	}
}

to.pdf(figure_leaf(), paste0("figs/hump-L.pdf"), height=4, width =12)

# decomposition of leaf area growth rate
figure_basal_area <- function(){
	h <- seq(0.1, 50,length.out=50)
	x <- change_with_size(h=h)
	par(mfrow = c(1,4))

	for(v in c("dbasal_area_dt","dsapwood_area_dt", "dbark_area_dt","dheartwood_area_dt")){
		plot(h, x$vars_growth_decomp[[v]], type='l', xlab = "height (m)", ylab=v)
	}
}

to.pdf(figure_basal_area(), paste0("figs/hump-Basal.pdf"), height=4, width =12)
