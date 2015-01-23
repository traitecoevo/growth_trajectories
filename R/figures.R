
fig_size_dpdt <- function() {
  figure_size(ylim = c(0, 500), yvars = c("net_production", "assimilation", "respiration", "turnover"))
}

fig_size_dhdt <- function() {
  figure_size(yvars = c("height_growth_rate", "dheight_dleaf_area", "leaf_fraction", "growth_fraction", "net_production"))
}

fig_size_daldt <- function() {
  figure_size(yvars = c("dleaf_area_dt", "leaf_fraction", "growth_fraction", "net_production"))
}

# growth decomp
figure_size <- function(yvars = c("height_growth_rate", "dheight_dleaf_area", "leaf_fraction", "growth_fraction", "net_production"), E = 1, strategy = default_strategy(),
  ...) {

  h <- seq(0.1, 50, length.out = 50)
  plants <- change_with_height(h = h, E = E, strategy = strategy)

  i <- !sapply(plants, is.null)  # check for null values
  x <- h[i]
  y <- do.call(rbind, plants[i])[, yvars]

  i <- y[["net_production"]] > 0

  par(mfrow = c(1, length(yvars)))
  for (v in yvars) {
    plot(h[i], y[[v]][i], type = "l", xlab = "height (m)", ylab = v, ...)
  }
}

figure_growth_light <- function(strategy = default_strategy()) {
  op <- par(oma = c(3, 6, 3, 1), mar = c(1, 1, 2, 1))

  at <- c(0.005, 0.01, 0.1)
  nrows <- 2
  ncols <- length(at)

  m <- matrix(rep(c(1:ncols, rep(ncols + 1, ncols)), nrows) + sort(rep(0:(nrows - 1), ncols * 2)) * (ncols + 1), ncol = ncols, byrow = TRUE)
  layout(m, widths = rep(1/ncols, ncols), heights = rep(c(0.8, 0.2)/nrows, nrows))
  E <- exp(-0.5*c(0, 0.5, 1, 2,3))
  for(trait in c("lma", "rho")) {
    figure_trait_growth_model_panel(E,trait, at = at, strategy=strategy, title = (trait=="lma"))
    header_plot(get_axis_info(trait, "lab"))
  }
  mtext(get_axis_info("dbasal_diam_dt", "lab"), line = 4, side = 2, cex = 1, outer = TRUE)
  par(op)
}


figure_trait_growth_model_panel <- function(E, trait, at, title = FALSE, ...) {

  for (a in at) {
    if (a == at[1])
      ytick <- TRUE else ytick <- FALSE

  figure_trait_growth_model(E,  a, trait, ytick.lab = ytick, ...)
  if (title)
    mtext(paste0("dbh=", a, "m"), cex=1, line =1 )
  }
}

figure_trait_growth_model <- function(E,  at, trait, ytick.lab = TRUE, strategy = default_strategy,
  ylim =c(0.0, 0.08), ...) {

  axes <- c(trait, "dbasal_diam_dt")

  ytick <- seq(0, 0.1, by=0.02)

  if (ytick.lab){
    new_plot(axes[1], axes[2], log = "x", xlab = NULL, ylab = NULL, ylim = ylim,
      ytick=ytick, ...)
  } else{
    new_plot(axes[1], axes[2], log = "x", xlab = NULL, ylab = NULL, ylim = ylim,
      ytick=ytick, ytick.lab = NA, ...)
  }

  x <- seq_log_range(get_axis_info(trait, "lim"), 20)

  for (env in E) {
    plants <- change_with_trait(x, trait, env,
      distance_from_target_fn = function(plant) { plant$vars_size[["diameter"]] - at},
      strategy = strategy)
    keep <- !sapply(plants, is.null)  # check for null values
    xx <- x[keep]
    y <- do.call(rbind, plants[keep])[["dbasal_diam_dt"]]
    points(xx, y, type = "l")
    keep <- 2
    text(xx[keep], y[keep], pos = 3, labels = paste0(format(-log(env)/0.5, digits = 1), " m2"), col = "grey")
  }
}

figure_optimal_lma_light <- function() {

  op <- par(oma = c(4, 4, 1, 1))
  new_plot("height", "lma", log = "xy", ylim = c(0.01, 2.6))

  h <- 10^seq(log10(0.2), log10(30), length.out = 50)
  for (E in exp(-0.5 * c(3, 2, 1, 0.1))) {
    x <- trait_maximimum_with_size(h = h, "lma", c(1e-05, 20), E = E)
    points(h, x, type = "l")
    i <- 20
    text(h[i], x[i], pos = 3, labels = paste0(format(-log(E)/0.5, digits = 1), "m2"), col = "grey")
  }
  par(op)
}


# growth decomp
figure_lma_wplcp <- function() {

  op <- par(oma = c(4, 4, 1, 1))

  new_plot("lma", "shading", log = "xy")

  lma <- 10^seq(-2, 0.5, length.out = 50)
  for (h in c(0.1, 2, 10, 20)) {
    x <- log(wplcp_with_trait(lma, "lma", h = h))/-0.5
    points(lma, x, type = "l")
    i <- 25 - 4 * floor(log2(22 - h))
    text(lma[i], x[i], pos = 3, labels = paste0("h=", h), col = "grey")
  }
  par(op)
}

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


figure_mass_fraction <- function(h = 1:50, E = 1, strategy = default_strategy(), xvar = "height", ylab = "fraction of mass", xlab = "height (m)", fill = TRUE, include_labels = TRUE,
  include_heartwood = FALSE, cols = c("forestgreen", "tan", "orange", "firebrick2", "brown"), ...) {

  vars <- c("mass_leaf", "mass_root", "mass_bark", "mass_sapwood")
  labels <- c("leaves", "roots", "bark", "sapwood")

  if (include_heartwood) {
    vars <- c(vars, "mass_heartwood")
    labels <- c(labels, "heartwood")
  }

  # plants across a range of sizes
  X <- change_with_height(h = h, E = E, strategy = strategy)
  i <- !sapply(X, is.null)  # check for null values
  X <- do.call(rbind, X[i])


  mass_fraction <- X[, vars]/rowSums(X[, vars])

  # cumulative sum of columns as fraction of total
  y <- t(apply(mass_fraction, 1, cumsum))
  x <- X[[xvar]]

  par(oma = c(2, 2, 2, 4))

  plot(x, x, type = "n", ylim = c(0, 1), xaxs = "i", yaxs = "i", ylab = ylab, xlab = xlab, las = 1, ...)
  for (i in rev(seq_len(ncol(y)))) {
    if (!fill)
      points(x, y[, i], type = "l", col = cols[i]) else polygon(c(x, x[length(x)], x[1], x[1]), c(y[, i], 0, 0, y[1, i]), col = cols[i], border = cols[i])
  }

  # add labels
  if (include_labels) {
    at <- 0.5 * (c(0, y[nrow(y), -c(ncol(y))]) + y[nrow(y), ])
    axis(4, at = at, labels = labels, las = 1, lwd.ticks = par("lwd"), lwd = 0)
  }
}

figure_trait_effect_at_size <- function(trait, growth_measure, size_measure, size, strategy = default_strategy(), E = 1, ylim = NULL, title = TRUE, ylab = NULL,
  xlim = NULL,   ...) {

  if(is.null(xlim))
    x.range <- get_axis_info(trait, "lim")
  else
    x.range <- xlim

  xx <- seq_log_range(x.range, 50)

  distance_from_target_fn <- function(plant) {
    plant$vars_size[[size_measure]] - size
  }

  plants <- change_with_trait(xx, trait, E = E, distance_from_target_fn, strategy = strategy)

  i <- sapply(plants, function(x) !is.null(x) && !is.infinite(x[[growth_measure]]))  # check for null values
  x <- xx[i]
  y <- do.call(rbind, plants[i])[[growth_measure]]

  if (is.null(ylim))
    ylim <- c(0, max(y) * 1.2)

  new_plot(trait, growth_measure, log = "x", xlab = NULL, ylab = ylab, ylim = ylim, ytick = pretty(ylim), ...)

  points(x, y, type = "l")
  if (title) {
    # if(size< 0.01) TRUE else FALSE main <- format(size, digits=2, scientific=TRUE) else
    main <- size
    title(main)
  }
}


figure_trait_effect_at_sizes <- function(trait, growth_measure, size_measure, sizes, ...) {
  op <- par(oma = c(4, 4, 1, 1), mfrow = c(1, length(sizes)))
  for (size in sizes) figure_trait_effect_at_size(trait, growth_measure, size_measure, size, ...)
  mtext(get_axis_info(trait, "lab"), line = 1, side = 1, cex = 1, outer = TRUE)
  mtext(get_axis_info(growth_measure, "lab"), line = 1, side = 2, cex = 1, outer = TRUE)
  par(op)
}


figure_trait_effects_at_sizes <- function(trait, size_measure, sizes, growth_measures = c("height_growth_rate", "dbasal_area_dt", "dbasal_diam_dt", "dmass_above_ground_dt"),
  ...) {


  nrows <- length(growth_measures)
  ncols <- length(sizes)

  op <- par(oma = c(6, 6, 2, 1), mar = c(1, 1, 2, 1), mfrow = c(nrows, ncols))

  for (growth_measure in growth_measures) {
    if (growth_measure == growth_measures[1]) {
      title <- TRUE
    } else {
      title <- FALSE
    }
    if (growth_measure == last(growth_measures)) {
      xtick.lab <- TRUE
    } else {
      xtick.lab <- FALSE
    }

    for (size in sizes) {
      if (size == sizes[1]) {
        ylab <- get_axis_info(growth_measure, "lab")
        ytick.lab <- TRUE
      } else {
        ylab <- NULL
        ytick.lab <- FALSE
      }

      figure_trait_effect_at_size(trait, growth_measure, size_measure, size, title = title, ylab = ylab, ylim = get_axis_info(growth_measure, "lim"), ytick.lab = ytick.lab,
        xtick.lab = xtick.lab, line = 6, ...)
    }
  }
  mtext(get_axis_info(trait, "lab"), line = 4, side = 1, cex = 1, outer = TRUE)
  par(op)
}

figure_trait_relative_effects_at_sizes <- function(trait, size_measure, sizes, growth_measures = c("height_growth_rate_relative", "dbasal_area_dt_relative", "dbasal_diam_dt_relative",
  "dmass_above_ground_dt_relative"), ...) {
  figure_trait_effects_at_sizes(trait, size_measure, sizes, growth_measures, ...)
}

figure_trait_effects_at_heights <- function(trait, size_measure = "height", sizes = c(0.25, 2, 8, 16), ...) {
  figure_trait_effects_at_sizes(trait, size_measure, sizes, ...)
}

figure_trait_relative_effects_at_heights <- function(trait, size_measure = "height", sizes = c(0.25, 2, 8, 16), ...) {
  figure_trait_relative_effects_at_sizes(trait, size_measure, sizes, ...)
}

figure_trait_effects_at_diameters <- function(trait, size_measure = "diameter", sizes = c(0.0025, 0.01, 0.1, 0.5), ...) {
  figure_trait_effects_at_sizes(trait, size_measure, sizes, ...)
}

figure_trait_relative_effects_at_diameters <- function(trait, size_measure = "diameter", sizes = c(0.0025, 0.01, 0.1, 0.5), ...) {
  figure_trait_relative_effects_at_sizes(trait, size_measure, sizes, ...)
}

figure_lma_effects_at_heights <- function(...) {
  figure_trait_effects_at_heights("lma", ...)
}

figure_rho_effects_at_heights <- function(...) {
  figure_trait_effects_at_heights("rho", ...)
}

figure_hmat_effects_at_heights <- function(...) {
  figure_trait_effects_at_heights("hmat", ...)
}

figure_lma_relative_effects_at_heights <- function(...) {
  figure_trait_relative_effects_at_heights("lma", ...)
}

figure_rho_relative_effects_at_heights <- function(...) {
  figure_trait_relative_effects_at_heights("rho", ...)
}

figure_hmat_relative_effects_at_heights <- function(...) {
  figure_trait_relative_effects_at_heights("hmat", ...)
}

figure_lma_effects_at_diameters <- function(...) {
  figure_trait_effects_at_diameters("lma", ...)
}

figure_rho_effects_at_diameters <- function(...) {
  figure_trait_effects_at_diameters("rho", ...)
}

figure_hmat_effects_at_diameters <- function(...) {
  figure_trait_effects_at_diameters("hmat", ...)
}

figure_lma_relative_effects_at_diameters <- function(...) {
  figure_trait_relative_effects_at_diameters("lma", ...)
}

figure_rho_relative_effects_at_diameters <- function(...) {
  figure_trait_relative_effects_at_diameters("rho", ...)
}

figure_hmat_relative_effects_at_diameters <- function(...) {
  figure_trait_relative_effects_at_diameters("hmat", ...)
}
