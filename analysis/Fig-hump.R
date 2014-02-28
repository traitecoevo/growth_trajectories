#!/usr/bin/env Rscript

source("R/tree-fun.R")

strategy = new(Strategy)
strategy$set_parameters(structure(list(0), names="c_r1"))

# growth decomp
figure_height <- function(...){
	h <- seq(0.1, 50,length.out=50)
	x <- change_with_size(h=h,...)
	par(mfrow = c(1,5))

	for(v in c("height_growth_rate","dheight_dleaf_area","leaf_fraction", "growth_fraction","net_production")){
		plot(h, x$vars_growth_decomp[[v]], type='l', xlab = "height (m)", ylab=v)
	}
}

to.pdf(figure_height(strategy=strategy), paste0("figs/hump.pdf"), height=4, width =12)


# decomposition of leaf area growth rate
figure_leaf <- function(...){
	h <- seq(0.1, 50,length.out=50)
	x <- change_with_size(h=h,...)
	par(mfrow = c(1,4))

	for(v in c("dleaf_area_dt","leaf_fraction", "growth_fraction","net_production")){
		plot(h, x$vars_growth_decomp[[v]], type='l', xlab = "height (m)", ylab=v)
	}
}

to.pdf(figure_leaf(strategy=strategy), paste0("figs/hump-leaf.pdf"), height=4, width =12)

# decomposition of leaf area growth rate
figure_basal_area <- function(...){
	h <- seq(0.1, 50,length.out=50)
	x <- change_with_size(h=h,...)
	par(mfrow = c(1,4))

	for(v in c("dbasal_area_dt","dsapwood_area_dt", "dbark_area_dt","dheartwood_area_dt")){
		plot(h, x$vars_growth_decomp[[v]], type='l', xlab = "height (m)", ylab=v)
	}
}

to.pdf(figure_basal_area(strategy=strategy), paste0("figs/hump-basal_area.pdf"), height=4, width =12)

# decomposition of diameter growth rate
figure_basal_diam <- function(...){
	h <- seq(0.1, 50,length.out=50)
	x <- change_with_size(h=h,...)
	par(mfrow = c(1,3))

	for(v in c("dbasal_diam_dt","dbasal_diam_dbasal_area","dbasal_area_dt")){
		plot(h, x$vars_growth_decomp[[v]], type='l', xlab = "height (m)", ylab=v)
	}
}

to.pdf(figure_basal_diam(strategy=strategy), paste0("figs/hump-basal_diam.pdf"), height=4, width =12)
