figure_lma_tradeoff <- function(data) {

  op <- par(oma = c(4, 4, 1, 1))

  sites <- data[["dataset"]]
  lma <- data[["lma"]]
  leaf_turnover <- data[["leaf_turnover"]]

  biomes <- sort(unique(data[["biome"]]))
  nbiomes <- length(biomes)

  col.table <- nice_colors(nbiomes)
  names(col.table) <- biomes

  # only use sites with n > 5
  site.n <- data.frame(table(sites))
  i <- !is.na(lma) & !is.na(leaf_turnover) & site.n[match(sites, as.character(site.n[, 1])), 2] > 3

  new_plot("lma", "leaf_turnover", log = "xy", line = 5, xlim = c(0.01, 1.28))
  points(lma[i], leaf_turnover[i], type = "p", col = make_transparent("grey", 0.3), pch = 16, cex = 0.9)

  sm1 <- sma(leaf_turnover[i] ~ lma[i] * sites[i], log = "xy")
  plot(sm1, add = T, col = colour_by_category(data[["biome"]][match(sm1[["groups"]], sites)], col.table), type = "l", lwd = 1, p.lines.transparent = 0.15)

  x <- seq_log(0.001, 3, length.out = 100)

  strategy <- default_strategy()
  B4 <- strategy$parameters$B4
  k_l0 <- strategy$parameters$k_l0
  lma_0 <- strategy$parameters$lma_0

  points(x, k_l0 * (x/lma_0)^-B4, type = "l", col = "black", lwd = 2)

  lab <- paste0(length(unique(sites[i])), " sites, ", sum(!is.na(leaf_turnover[i]) & !is.na(lma[i])), " species")
  legend("topright", legend = lab, bty = "n", cex = 0.5)

  lab <- c("", tolower(names(col.table)))
  legend("topright", legend = lab, bty = "n", pch = c(NA, rep(16, nbiomes)), col = c(NA, col.table), cex = 0.5)

  par(op)
}
