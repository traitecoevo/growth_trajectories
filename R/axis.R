

get_axis_info <- function(name, parToGet) {

  # TRAITS
  if (name == "lma") {
    ticks <- 0.01 * 2^seq(0, 20)
    lab <- expression(paste("Leaf-construction cost (kg ", m^-2, " )"))
    lim <- c(0.04, 1.28)
  }
  if (name == "hmat") {
    ticks <- 2^seq(-4, 20)
    lab <- expression(paste("Maximum height (m)"))
    lim <- c(1, 40)
  }
  if (name == "rho") {
    ticks <- 50 * 2^seq(0, 10)
    lab <- expression(paste("Wood density (kg ", m^-3, " )"))
    lim <- c(200, 1200)
  }

  # SIZE
  if (name == "height") {
    ticks <- 0.25 * 2^seq(0, 12)
    lab <- expression(paste("Height (m)"))
    lim <- c(0.25, 32)
  }
  if (name == "diameter") {
    ticks <- 0.25 * 2^seq(-4, 12)
    lab <- expression(paste("Stem diameter (m)"))
    lim <- c(0.001, 1)
  }
  if (name == "area_basal") {
    ticks <- 0.25 * 2^seq(0, 12)
    lab <- expression(paste("Stem basal area (m)"))
    lim <- c(0.25, 32)
  }

  # GROWTH RATE
  if (name == "height_growth_rate") {
    ticks <- seq(0, 2, by = 0.25)
    lab <- expression(paste("Height (m ", yr^-1, " )"))
    lim <- c(0, 2)
  }

  if (name == "dbasal_diam_dt") {
    ticks <- 0.0125 * 2^seq(-5, 10)
    lab <- expression(paste("Diameter growth (m ", yr^-1, ")"))
    lim <- c(0, 0.05)
  }

  if (name == "dbasal_area_dt") {
    ticks <- 0.0125 * 2^seq(0, 20)
    lab <- expression(paste("Basal area growth (", m^2, " ", yr^-1, ")"))
    lim <- c(0, 0.003)
  }

  if (name == "dmass_above_ground_dt") {
    ticks <- 1 * 2^seq(0, 20)
    lab <- expression(paste("Mass growth (kg ", yr^-1, ")"))
    lim <- c(0, 25)
  }

  # RELATIVE GROWTH RATE
  if (name == "height_growth_rate_relative") {
    ticks <- seq(0, 5, by = 0.2)
    lab <- expression(paste("Relative height growth (", yr^-1, " )"))
    lim <- c(0, 5)
  }

  if (name == "dbasal_diam_dt_relative") {
    ticks <- seq(0, 5, by = 0.2)
    lab <- expression(paste("Relative diameter growth (", yr^-1, ")"))
    lim <- c(0, 5)
  }

  if (name == "dbasal_area_dt_relative") {
    ticks <- seq(0, 5, by = 0.2)
    lab <- expression(paste("Relative basal area growth (", yr^-1, ")"))
    lim <- c(0, 5)
  }

  if (name == "dmass_above_ground_dt_relative") {
    ticks <- seq(0, 5, by = 0.2)
    lab <- expression(paste("Relative mass growth (kg ", yr^-1, ")"))
    lim <- c(0, 5)
  }

  # OTHER
  if (name == "leaf_turnover") {
    ticks <- c(0.06, 0.12, 0.25, 0.5, 1, 2, 4, 8, 16, 32)
    lab <- expression(paste("Leaf turnover rate (", yr^-1, " )"))
    lim <- c(0.03, 32)
  }
  if (name == "shading") {
    ticks <- c(0.25, 0.5, 1, 2, 4)
    lab <- expression(paste("Maximum leaf area above (", m^2, " )"))
    lim <- c(0.2, 5)
  }

  return(get(parToGet))
}


new_plot <- function(xvar, yvar, xlim = get_axis_info(xvar, "lim"), xtick = get_axis_info(xvar, "ticks"), xlab = get_axis_info(xvar, "lab"), ylim = get_axis_info(yvar,
  "lim"), ytick = get_axis_info(yvar, "ticks"), ylab = get_axis_info(yvar, "lab"), xtick.lab = xtick, ytick.lab = ytick, ...) {

  blank_plot(xlim = xlim, xtick = xtick, xlab = xlab, ylim = ylim, ytick = ytick, ylab = ylab, xtick.lab = xtick.lab, ytick.lab = ytick.lab, ...)
}

blank_plot <- function(xlim, ylim, xlab = NULL, ylab = NULL, line = 4, xtick = NULL, ytick = NULL, xtick.lab = xtick, ytick.lab = ytick, ...) {

  plot(NA, xlim = xlim, ylim = ylim, xaxt = "n", yaxt = "n", ann = FALSE, ...)

  if (!is.null(xtick))
    axis(1, at = xtick, labels = xtick.lab)

  if (!is.null(ytick))
    axis(2, at = ytick, labels = ytick.lab, las = 1)

  # function handles multiple lines, if lab is a list
  add_axis_label <- function(lab, side, line) {
    for (i in seq_along(lab)) mtext(lab[[i]], side, line = line - (i + 1))
  }

  if (!is.null(xlab))
    add_axis_label(xlab, 1, line)

  if (!is.null(ylab))
    add_axis_label(ylab, 2, line)

}
