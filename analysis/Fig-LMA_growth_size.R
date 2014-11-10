#!/usr/bin/env Rscript

source("R/tree-fun.R")
source("R/axis-fun.R")

strategy = new(Strategy)
strategy$set_parameters(structure(list(0.8, 10, 0.2, 2.17, 0.546), names=c("c_r1", "c_r2", "k_s0", "a1", "B1")))

plot.trait.effect.at.given.size <- function(trait, growth_measure, size_measure, size, strategy, E=1, ylim=NULL, title=TRUE, ylab=NULL, ...){

	x.range <- get.axis.info(trait, "lim")
	xx <- seq_log_range(x.range, 50)

	distance.from.target.fn <- function(plant){plant$vars_size[[size_measure]] - size}

	plants <- change_with_trait(xx, trait, E=E, distance.from.target.fn, strategy=strategy)

	i <- sapply(plants, function(x) !is.null(x) &&  !is.infinite(x[[growth_measure]]))  	# check for null values
	x <- xx[i]
	y <- do.call(rbind,plants[i])[[growth_measure]]

	if(is.null(ylim))
		ylim <- c(0, max(y)*1.2)

	new_plot(trait, growth_measure, log="x", xlab=NULL, ylab=ylab, ylim=ylim, ytick=pretty(ylim),
			...)

	points(x, y, type='l')
	if(title){
		# if(size<  0.01) TRUE else FALSE
		# 	main <- format(size, digits=2, scientific=TRUE)
		# else
		main <- size
		title(main)
	}
}

plot.panel.single <- function(trait, growth_measure, size_measure, sizes, ...) {
	op <- par(oma=c(4,4,1,1), mfrow = c(1,length(sizes)))
	for(size in sizes)
		plot.trait.effect.at.given.size(trait, growth_measure, size_measure, size, ...)
	mtext(get.axis.info(trait,"lab"), line =1, side = 1, cex=1, outer = TRUE)
	mtext(get.axis.info(growth_measure,"lab"), line =1, side = 2, cex=1, outer = TRUE)
	par(op)
}

plot.panel.all <- function(trait, size_measure, sizes,
	growth_measures=c("height_growth_rate", "dbasal_area_dt", "dbasal_diam_dt", "dmass_above_ground_dt",
		"height_growth_rate_relative", "dbasal_area_dt_relative","dbasal_diam_dt_relative",
		"dmass_above_ground_dt_relative"),...) {


	nrows <- length(growth_measures)
	ncols <- length(sizes)

	op <- par(oma=c(6,6,2,1), mar=c(1,1,2,1), mfrow=c(nrows, ncols))

	for(growth_measure in growth_measures){
		if(growth_measure==growth_measures[1]){
			title <- TRUE
		} else {
			title <- FALSE
		}
		if(growth_measure==last(growth_measures)) {
			xtick.lab <- TRUE
		} else {
			xtick.lab <- FALSE
		}

		for(size in sizes){
			if(size==sizes[1]){
				ylab <- get.axis.info(growth_measure,"lab")
				ytick.lab <- TRUE
			}
			else{
				ylab <- NULL
				ytick.lab <- FALSE
			}

			plot.trait.effect.at.given.size(trait, growth_measure, size_measure, size, title=title,
				ylab=ylab, ylim=get.axis.info(growth_measure,"lim"), ytick.lab=ytick.lab,
				xtick.lab=xtick.lab, line=6,...)
			}
	}
	mtext(get.axis.info(trait,"lab"), line =4, side = 1, cex=1, outer = TRUE)
	par(op)
}

growth_measures_abs <- c("height_growth_rate", "dbasal_area_dt", "dbasal_diam_dt", "dmass_above_ground_dt")
growth_measures_rel <- c("height_growth_rate_relative", "dbasal_area_dt_relative",
	"dbasal_diam_dt_relative", "dmass_above_ground_dt_relative")

for(trait in c("lma", "rho", "hmat")){
	to.pdf(
		plot.panel.all(trait, "height", sizes=c(0.25, 2, 8, 16), strategy=strategy,
		growth_measures=growth_measures_abs)
		,paste0("output/figs/growth-", trait, "_abs_height.pdf"), height=12, width=12)
}

for(trait in c("lma", "rho", "hmat")){
	to.pdf(
		plot.panel.all(trait, "height", sizes=c(0.25, 2, 8, 16), strategy=strategy,
		growth_measures=growth_measures_rel)
		,paste0("output/figs/growth-", trait, "_rel_height.pdf"), height=12, width=12)
}

for(trait in c("lma", "rho", "hmat")){
	to.pdf(
		plot.panel.all(trait, "diameter", sizes=c(0.0025, 0.01, 0.1, 0.5), strategy=strategy,
		growth_measures=growth_measures_abs)
		,paste0("output/figs/growth-", trait, "_abs_diameter.pdf"), height=12, width=12)
}

for(trait in c("lma", "rho", "hmat")){
	to.pdf(
		plot.panel.all(trait, "diameter", sizes=c(0.0025, 0.01, 0.1, 0.5), strategy=strategy,
		growth_measures=growth_measures_rel)
		,paste0("output/figs/growth-", trait, "_rel_diameter.pdf"), height=12, width=12)
}
