#!/usr/bin/env Rscript

source("R/tree-fun.R")

strategy = new(Strategy)
strategy$set_parameters(structure(list(0, 0.2), names=c("c_r1", "k_s")))

# growth decomp
figure_size <- function(yvars=c("height_growth_rate","dheight_dleaf_area","leaf_fraction", "growth_fraction","net_production"), E=1, strategy = new(Strategy),...){

	h <- seq(0.1, 50,length.out=50)
	plants <- change_with_height(h=h,E=E,strategy=strategy)

	i <- !sapply(plants, is.null)  	# check for null values
	x <- h[i]
	y <- do.call(rbind,plants[i])[,yvars]


	i <- y[["net_production"]] > 0

	par(mfrow = c(1,length(yvars)))
	for(v in yvars){
		plot(h[i], y[[v]][i], type='l', xlab = "height (m)", ylab=v,...)
	}
}

# dP_dt
to.pdf(figure_size(strategy=strategy, ylim=c(0,500),
	yvars=c("net_production","assimilation","respiration","turnover")
	), paste0("output/figs/SI-size-dPdt.pdf"), height=4, width =12)


# dh_dt
to.pdf(figure_size(strategy=strategy,
	yvars=c("height_growth_rate","dheight_dleaf_area","leaf_fraction", "growth_fraction","net_production")
	), paste0("output/figs/SI-size-dhdt.pdf"), height=4, width =12)


# dal_dt
to.pdf(figure_size(strategy=strategy,
	yvars=c("dleaf_area_dt","leaf_fraction", "growth_fraction","net_production")
	), paste0("output/figs/SI-size-daldt.pdf"), height=4, width =12)
