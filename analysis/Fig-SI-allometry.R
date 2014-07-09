#names of all studies
library(smatr)
library(plyr)

source("R/plot-utils.R")

# TODO: Baad data not yet public, so need path on local machine. Eventually replace this with URL for public data
filename <- "~/Dropbox/_research/active/Falster-Allometry/baad/output/data/all.csv"
data <- read.csv(filename, stringsAsFactors=FALSE)


## Fill sapwood area. Let basal=breast height where only data at breast height exists.
# For small plants treat total stem cross section as sapwood
data[["sap"]] <- data[["a.ssba"]]
i <- is.na(data[["sap"]])
data[["sap"]][i] <- data[["a.ssbh"]][i]
i <- is.na(data[["sap"]]) & !is.na(data[["a.stba"]]) & !is.na(data[["h.t"]]) & data[["h.t"]] < 2
data[["sap"]][i] <- data[["a.stba"]][i]


addIsoclines <- function(A= seq(-10,10),b=1,...){
  for(a in A) abline(a,b,lty="dotted",...)
}


add.sma.by.group <-function (data, xvar, yvar, smagroup, colorBygroup, col.table,nmin=10,...){
	if(sum(!is.na(data[[yvar]]*data[[xvar]]))>nmin){
		col=colour.by.category(data[[colorBygroup]][1],col.table)
		fit <- sma(data[[yvar]]~data[[xvar]]*data[[smagroup]], log="xy")
		plot(fit, type='l',col=col,p.lines.transparent=0.1,add=TRUE,...)
		invisible(fit)
	}
}

make.color.table <- function(names){
	col.table <- niceColors(length(names))
	names(col.table) <- names
	col.table
}

# make a plot, fits lines to each within each group, applyiong same colour within that group
plot_with_sma_by_var_within_group <- function(data, xvar,yvar,group="vegetation",location=NULL, isoclines=NULL,...){

	plot(data[[xvar]], data[[yvar]],  type= 'n', log="xy", las=1, yaxt="n", xaxt="n",...)
	axis.log10(1)
	axis.log10(2)

	if(!is.null(isoclines))
		addIsoclines(b=isoclines)

	points(data[[xvar]], data[[yvar]],  type= 'p', col = make.transparent("grey", 0.4), pch=16)

	col.table <- make.color.table(unique(data[[group]]))

	fits <- dlply(data, group, add.sma.by.group, xvar=xvar, yvar=yvar, col.table=col.table,
		smagroup="species",colorBygroup=group)


	get.significant.groups <- function(fit, p=0.1){
		if(is.null(fit)) return(FALSE)
		any(fit$groupsummary$pval < p)
	}

	tmp <- laply(fits, get.significant.groups)
	lab <- sort(names(tmp)[tmp])

	if(!is.null(location))
		legend(location, legend = tolower(lab), bty = "n", pch=16, col=colour.by.category(lab,col.table), cex=0.5)
	fits
}


myfigure <- function(){
	par(mfrow = c(1,3))
	fits<-plot_with_sma_by_var_within_group(data,"a.lf", "h.t", "vegetation",ylim=c(0.005, 120), xlim=c(1E-5, 1E3), ylab="Height (m)", xlab="",location="topleft", isoclines=0.5, main="a) Architecture")
	legend("bottomright", legend = "Slope = 0.5", bty = "n", cex=1)

	plot_with_sma_by_var_within_group(data,"a.lf", "sap", "vegetation",xlim=c(1E-5, 1E3), ylim=c(1E-8, 1E0), ylab=expression(paste("Sapwood area (",m^2,")")), xlab=expression(paste("Leaf area (",m^2,")")),location="topleft", isoclines=1, "b) Pipe Model")
	legend("bottomright", legend = "Slope = 1", bty = "n", cex=1)

	plot_with_sma_by_var_within_group(data,"a.lf", "m.rt", "vegetation",xlim=c(1E-5, 1E3), ylim=c(1E-8, 400), ylab="mass root (kg)", xlab="",location="topleft", isoclines=1, main="c) Roots")
	legend("bottomright", legend = "Slope = 1", bty = "n", cex=1)

}


to.pdf(myfigure(),"output/figs/allometry.pdf", height= 4, width =12)
