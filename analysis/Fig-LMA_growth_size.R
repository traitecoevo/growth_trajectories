#!/usr/bin/env Rscript

source("R/tree-fun.R")
source("R/axis-fun.R")

strategy = new(Strategy)
strategy$set_parameters(structure(list(0, 0.2), names=c("c_r1", "k_s0")))

plot.trait.effect.at.given.size <- function(trait, growth_measure, size_measure, size, strategy, E=1, ylim=NULL){

	x.range <- get.axis.info(trait, "lim")
	xx <- seq_log_range(x.range, 50)

	if(size_measure=="area_basal"){
		distance.from.target.fn <- function(plant){plant$vars_size[["area_basal"]] - size}
		main <- paste0("b=", format(size, digits=2, scientifIc=TRUE),"mm^2")
	}
	if(size_measure=="height"){
		distance.from.target.fn <- function(plant){plant$height - size}
		main <- paste0("h=", size, "m")
	}

	plants <- change_with_trait(xx, trait, E=E, distance.from.target.fn, strategy=strategy)

	i <- !sapply(plants, is.null)  	# check for null values
	x <- xx[i]
	y <- do.call(rbind,plants[i])[[growth_measure]]

	if(!is.null(ylim))
		ylim <- c(0, max(y)*1.2)

	new_plot(trait, growth_measure, log="x", xlab=NULL, ylab=NULL, ylim=ylim, ytick=pretty(ylim))
	points(x, y, type='l')
	title(main)
}

plot.panel <- function(trait, growth_measure, size_measure, sizes, ...) {
	op <- par(oma=c(4,4,1,1), mfrow = c(1,length(sizes)))
	for(size in sizes)
		plot.trait.effect.at.given.size(trait, growth_measure, size_measure, size, ...)
	mtext(get.axis.info(trait,"lab"), line =1, side = 1, cex=1, outer = TRUE)
	mtext(get.axis.info(growth_measure,"lab"), line =1, side = 2, cex=1, outer = TRUE)
	par(op)
}

to.pdf(
	plot.panel("lma", "height_growth_rate", "height", sizes=c(0.5, 2,8,16), strategy=strategy, ylim=c(0, 1.4)),
	paste0("output/figs/growth-height.pdf"), height=4, width=8)
