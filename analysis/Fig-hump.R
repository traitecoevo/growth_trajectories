#!/usr/bin/env Rscript

source("R/tree-fun.R")

strategy = new(Strategy)
strategy$set_parameters(structure(list(0, 0.2), names=c("c_r1", "k_s")))

# growth decomp
figure_size <- function(yvars=c("height_growth_rate","dheight_dleaf_area","leaf_fraction", "growth_fraction","net_production"), E=1, strategy = new(Strategy),...){

	h <- seq(0.1, 50,length.out=50)
	x <- change_with_size(h=h,E=E,strategy=strategy)
	i <- x$vars[["net_production"]] > 0

	par(mfrow = c(1,length(yvars)))
	for(v in yvars){
		plot(h[i], x$vars[[v]][i], type='l', xlab = "height (m)", ylab=v,...)
	}
}

# dP_dt
to.pdf(figure_size(strategy=strategy, ylim=c(0,500),
	yvars=c("net_production","assimilation","respiration","turnover")
	), paste0("figs/size-dPdt.pdf"), height=4, width =12)


# dh_dt
to.pdf(figure_size(strategy=strategy,
	yvars=c("height_growth_rate","dheight_dleaf_area","leaf_fraction", "growth_fraction","net_production")
	), paste0("figs/size-dhdt.pdf"), height=4, width =12)


# dal_dt
to.pdf(figure_size(strategy=strategy,
	yvars=c("dleaf_area_dt","leaf_fraction", "growth_fraction","net_production")
	), paste0("figs/size-daldt.pdf"), height=4, width =12)


# dast_dt
to.pdf(figure_size(strategy=strategy,
	yvars=c("dbasal_area_dt","dleaf_area_dt","dsapwood_area_dt", "dheartwood_area_dt")
	), paste0("figs/size-dbadt.pdf"), height=4, width =12)


# ddst_dt

to.pdf(figure_size(strategy=strategy,
	yvars=c("dbasal_diam_dt","dbasal_diam_dbasal_area","dbasal_area_dt")
	), paste0("figs/size-ddstdt.pdf"), height=4, width =12)
