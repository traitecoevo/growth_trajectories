#!/usr/bin/env Rscript

source("R/plot-utils.R")
source("R/axis-fun.R")

library(smatr)

LMA_plot <- function(){

  op <- par(oma=c(4,4,1,1))

  dat <- read.csv("data/wright-2004.csv", header = TRUE, stringsAsFactors=FALSE)

  G = dat$Dataset
  LL = dat$LeafLifespan
  LMA= dat$LMA /1000

  # only use sites with n > 5
  site.n <- data.frame(table(G))
  i <- !is.na(LMA) &!is.na(LL) & site.n[match(G, as.character(site.n[,1])),2] >3

  Y <- 1/LL[i]
  X <- LMA[i]

  sm1 <- sma(Y~X*G[i],log='xy')

  new_plot(0,3, log="xy")

  points(X,Y, type ='p', col=make.transparent("grey", 0.6),pch= 16, cex=0.9)

  plot(sm1, add= T, col="darkgreen", type='l', lwd=1.0, p.lines.transparent=0.15)

  cat(paste0("Significant relationships: ",sum(sm1$groupsummary$pval <0.05), " of ", length(sm1$groupsummary[,1]),"\n"))

  cat(paste0(length(unique(G[i])), " sites, ", sum(!is.na(LMA) &!is.na(LL)), " species") )

  x <- 10^seq(log10(0.001), log10(3), by =0.01)
  points(x, 0.0286*x^-1.71, type='l', col='black', lwd=2)

  lab = paste0(length(unique(G[i])), " sites, ", sum(!is.na(Y) &!is.na(X) ), " species")
  legend("topright", legend = lab, bty = "n")

  par(op)
}

to.pdf(LMA_plot(), paste0("figs/SI-leaf.pdf"), height=6, width=6)
