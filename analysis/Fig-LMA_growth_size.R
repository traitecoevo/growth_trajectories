#!/usr/bin/env Rscript

source("R/tree-fun.R")
source("R/axis-fun.R")

strategy = new(Strategy)
strategy$set_parameters(structure(list(0, 0.2), names=c("c_r1", "k_s")))

plot.trait.effect.at.given.size <- function(focus, size, strategy){
	lma <- 10^seq(-1.5, 0.5, length.out=50)

	if(focus=="b"){
		distance.from.target.fn <- function(plant){plant$vars_size[["area_basal"]] - size}
		yvar <- "dbasal_area_dt"
		main <- paste0("b=", format(size, digits=2, scientifIc=TRUE),"mm^2")
	}
	if(focus=="h"){
		distance.from.target.fn <- function(plant){plant$height - size}
		yvar <- "height_growth_rate"
		main <- paste0("h=", size, "m")
	}
	if(focus =="p"){
		distance.from.target.fn <- function(plant){plant$height - size}
		yvar <- "net_production"
		main <- paste0("h=", size, "m")
	}

	plants <- change_with_trait(lma, "lma", E=1, distance.from.target.fn, strategy=strategy)
	i <- !sapply(plants, is.null)  	# check for null values
	x <- lma[i]
	y <- do.call(rbind,plants[i])[[yvar]]

	ylim <- c(0, max(y)*1.2)

	if(focus=="h") ylim <- c(0, 1.4)

	new_plot(0,2, log="x", xlab=NULL, ylab=NULL, ylim=ylim, ytick=pretty(ylim))
	points(x, y, type='l')
	title(main)
}

plot.panel <- function(focus, sizes, strategy, xlab, ylab){
	op <- par(oma=c(4,4,1,1), mfrow = c(1,length(sizes)))
	for(size in sizes)
		plot.trait.effect.at.given.size(focus, size,strategy)
	mtext(xlab, line =1, side = 1, cex=1, outer = TRUE)
	mtext(ylab, line =1, side = 2, cex=1, outer = TRUE)
	par(op)
}

to.pdf(
	plot.panel("h", sizes=c(0.5, 2,8,16), strategy,
		xlab=get.axis.info(0,"lab"), ylab=get.axis.info(2,"lab")),
	paste0("output/figs/growth-height.pdf"), height=4, width=8)


to.pdf(
	plot.panel("b", sizes=pi/4*(0.001*c(10, 20, 50, 100))^2, strategy,
		xlab=get.axis.info(0,"lab"), ylab=expression(paste("Basal area growth (", m^2, " ", yr^-1,")"))),
	paste0("output/figs/growth-stem.pdf"), height=4, width=8)

to.pdf(
	plot.panel("p", sizes=c(0.5, 2,8,16), strategy,
		xlab=get.axis.info(0,"lab"), ylab=expression(paste("Net production (kg ", yr^-1,")"))),
	paste0("output/figs/growth-production.pdf"), height=4, width =8)
