#!/usr/bin/env Rscript

source("R/tree-fun.R")


plot_mass_fraction <- function(h=1:50, E=1, strategy = new(Strategy),
			xvar="height",
			ylab="fraction of mass",xlab="height (m)",
			fill=TRUE, include_labels=TRUE, include_heartwood=FALSE,
			cols=c("forestgreen", "tan", "orange", "firebrick2", "brown"),...){

	vars <- c("mass_leaf", "mass_root", "mass_bark", "mass_sapwood")
	labels <- c("leaves", "roots", "bark", "sapwood")

	if(include_heartwood){
		vars <- c(vars, "mass_heartwood")
		labels <- c(labels, "heartwood")
	}

	# plants across a range of sizes
	X <- change_with_size(h=h, E=E, strategy=strategy)[["vars"]]

	mass_fraction <- X[,vars]/ rowSums(X[,vars])

	# cumulative sum of columns as fraction of total
	y <- t(apply( mass_fraction, 1, cumsum))
	x <- X[[xvar]]

	par(oma=c(2,2,2,4))

	plot(x,x, type='n', ylim=c(0,1),xaxs="i", yaxs="i", ylab=ylab,xlab=xlab,las=1,...)
	for(i in rev(seq_len(ncol(y)))){
	if(!fill)
		points(x,y[,i], type='l', col=cols[i])
	else
		polygon(c(x, x[length(x)], x[1], x[1]), c(y[,i], 0, 0, y[1,i]), col=cols[i], border =cols[i])
	}

	#add labels
	if(include_labels){
		at = 0.5*(c(0,y[nrow(y),-c(ncol(y))]) + y[nrow(y),])
		axis(4, at = at, labels = labels, las=1, lwd.ticks=par("lwd"), lwd=0)
	}
}

to.pdf(plot_mass_fraction(h=1:50), paste0("figs/mass_fraction.pdf"), height=6, width=6)
