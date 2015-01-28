figure_lma_tradeoff <- function(data) {
  sites <- data[["dataset"]]
  lma <- data[["lma"]]
  leaf_turnover <- data[["leaf_turnover"]]

  biomes <- sort(unique(data[["biome"]]))
  nbiomes <- length(biomes)

  col_table <- nice_colors(nbiomes)
  names(col_table) <- biomes

  i <- !is.na(lma) & !is.na(leaf_turnover) & table(sites)[sites] > 3
  sm1 <- sma(leaf_turnover[i] ~ lma[i] * sites[i], log="xy")
  col_sm1 <- col_table[data[["biome"]][match(sm1[["groups"]], sites)]]

  strategy <- default_strategy()
  B4 <- strategy$B4
  k_l0 <- strategy$k_l0
  lma_0 <- strategy$lma_0
  x <- seq_log(10^par("usr")[1], 10^par("usr")[2], length.out=10)
  y <- k_l0 * (x / lma_0)^(-B4)

  par(mar=c(4.6, 4.6, .5, .5))
  plot(NA, type="n", log="xy", xlim=c(0.01, 1.28), ylim=c(0.03, 32),
       xlab="", ylab="", las=1)
  mtext(name_pretty("lma"), 1, line=3)
  mtext(name_pretty("leaf_turnover"), 2, line=3)

  points(lma[i], leaf_turnover[i], pch=16, cex=0.9,
         col=make_transparent("grey", 0.3))
  plot(sm1, add=TRUE, col=col_sm1, type="l", lwd=1, p.lines.transparent=0.15)
  lines(x, y, lwd=2)

  title <- sprintf("%d sites, %d species",
                   length(unique(sites[i])),
                   sum(!is.na(leaf_turnover[i]) & !is.na(lma[i])))
  legend("topright", legend=tolower(names(col_table)), bty="n",
         pch=16, col=col_table, cex=1, title=title)
}
