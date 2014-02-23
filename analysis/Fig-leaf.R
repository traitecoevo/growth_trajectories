#!/usr/bin/env Rscript

source("R/plot-utils.R")
library(smatr)

LMA_plot <- function(){

  dat <- read.csv("data/wright-2004.csv", header = TRUE, stringsAsFactors=FALSE)

  G = dat$Dataset
  LL = dat$LeafLifespan
  LMA= dat$LMA

  # only use sites with n > 5
  site.n <- data.frame(table(G))
  i <- !is.na(LMA) &!is.na(LL) & site.n[match(G, as.character(site.n[,1])),2] >3

  Y <- 1/LL[i]
  X <- LMA[i]

  sm1 <- sma(Y~X*G[i],log='xy')

  plot(X,Y, type ='p', log="xy", ann=FALSE, las=1,
    col=make.transparent("grey", 0.6),pch= 16, cex=0.9)
  plot(sm1, add= T, col="darkgreen", type='l', lwd=1.0, p.lines.transparent=0.15)

  cat(paste0("Significant relationships: ",sum(sm1$groupsummary$pval <0.05), " of ", length(sm1$groupsummary[,1]),"\n"))

  cat(paste0(length(unique(G[i])), " sites, ", sum(!is.na(LMA) &!is.na(LL)), " species") )

  x <- 10^seq(log10(0.001), log10(3), by =0.01)
  points(x, 0.0286*x^-1.71, type='l', col='black', lwd=2)

  mtext(expression(paste("Leaf construction cost (g ", m^-2,")")), line =3, side = 1, cex=1.0)
  mtext(expression(paste("Leaf turnover rate (",yr^-1,")")), line =3, side = 2, cex=1.0)


  lab = paste0(length(unique(G[i])), " sites, ", sum(!is.na(Y) &!is.na(X) ), " species")
  legend("topright", legend = lab, bty = "n")
}

to.pdf(LMA_plot(), paste0("figs/SI-leaf.pdf"), height=6, width=6)
