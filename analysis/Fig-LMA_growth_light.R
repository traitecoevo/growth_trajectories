#!/usr/bin/env Rscript

source("R/tree-fun.R")
source("R/axis-fun.R")

strategy = new(Strategy)

# growth decomp
figure <- function(h){
	op <- par(oma=c(4,4,1,1))

	new_plot(0,2, log="x", ylim = c(0, 0.5),ytick=seq(0, 0.5, by=0.1), ytick.lab=seq(0, 0.5, by=0.1))

	lma <- 10^seq(-2, 1, length.out=50)
	for(E in seq(0.2, 1, by = 0.2)){
		plants <- change_with_trait(lma, "lma", E=E, distance.from.target.fn=function(plant){plant$height - h}, strategy=strategy)
		i <- !sapply(plants, is.null)  	# check for null values
		x <- lma[i]
		y <- do.call(rbind,plants[i])[["height_growth_rate"]]
		points(x, y, type='l')

		i <- 20
		text(x[i], y[i], pos=3, labels=paste0( format(-log(E)/0.5,digits=1),"m2"), col= "grey")
	}
	par(op)
}

to.pdf(figure(h=0.25), paste0("output/figs/growth-light.pdf"), height=8, width=8)
