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

  biomes <- sort(unique(dat$Biome))
  nbiomes <- length(biomes)

  col.table <- niceColors(nbiomes)
  names(col.table) <- biomes

  colour.by.category <- function(x, table) unname(table[x])

  # only use sites with n > 5
  site.n <- data.frame(table(G))
  i <- !is.na(LMA) &!is.na(LL) & site.n[match(G, as.character(site.n[,1])),2] >3

  Y <- 1/LL[i]
  X <- LMA[i]

  sm1 <- sma(Y~X*G[i],log='xy')
  new_plot(0,3, log="xy")


  points(X,Y, type ='p', col=make.transparent("grey", 0.3),pch= 16, cex=0.9)

  plot(sm1, add= T, col=colour.by.category(dat$Biome[match(sm1$groups, dat$Dataset)], col.table), type='l', lwd=1.0, p.lines.transparent=0.15)

  cat(paste0("Significant relationships: ",sum(sm1$groupsummary$pval <0.05), " of ", length(sm1$groupsummary[,1]),"\n"))

  cat(paste0(length(unique(G[i])), " sites, ", sum(!is.na(LMA) &!is.na(LL)), " species") )

  x <- 10^seq(log10(0.001), log10(3), by =0.01)
  points(x, 0.0286*x^-1.71, type='l', col='black', lwd=2)
  points(x, 0.01*x^-1.71, type='l', col='black', lwd=2, lty="dashed")


  lab = paste0(length(unique(G[i])), " sites, ", sum(!is.na(Y) &!is.na(X) ), " species")
  legend("topright", legend = lab, bty = "n", cex=0.5)

  lab = c("",tolower(names(col.table)))
  legend("topright", legend = lab, bty = "n", pch=c(NA, rep(16, nbiomes)), col= c(NA, col.table), cex=0.5)

  par(op)
}

to.pdf(LMA_plot(), paste0("figs/SI-leaf.pdf"), height=6, width=6)
