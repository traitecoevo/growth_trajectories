default_strategy <- function() {
  Strategy(B5=1.5,
           k_l0=0.4565855/3,
           hmat=30.0,
           c_r1=0.8,
           c_r2=20,
           k_s0=0.2,
           a1=2.17,
           B1=0.546)
}

run_plant_to_heights <- function(heights, strategy, env) {
  f <- function(h, p) {
    p$height <- h
    run_plant(p, env)
  }
  ## NOTE: previously we checked through these results and filtered
  ## out things that left NULL
  do.call("rbind", lapply(heights, f, Plant(strategy)))
}

run_plant_to_sizes <- function(sizes, size_variable, strategy, env,
                               time_max=Inf) {
  res <- grow_plant_to_size(Plant(strategy), sizes, size_variable,
                            env, time_max)
  do.call("rbind", lapply(res$plant, plant_info))
}

run_plant <- function(plant, env) {
  plant$compute_vars_phys(env)
  plant_info(plant)
}

plant_info <- function(p) {
  x <- as.data.frame(as.list(c(p$vars_size,
                               p$vars_phys,
                               p$vars_growth)))

  # add relative measures
  x$height_growth_rate_relative    <- x$height_growth_rate / x$height
  x$dbasal_area_dt_relative        <- x$dbasal_area_dt / x$basal_area
  x$dbasal_diam_dt_relative        <- x$dbasal_diam_dt / x$diameter
  x$dabove_ground_mass_dt_relative <- x$dabove_ground_mass_dt / x$above_ground_mass
  x
}

## Code for trait deriative figure
figure_trait_deriative <- function(type, trait_name="lma", canopy_openness=1,
                                strategy=default_strategy()) {

  strategy <- strategy
  strategy[[trait_name]] <- 0.05
  dat <- figure_rate_vs_size_data(canopy_openness, strategy)
  strategy2 <- strategy
  x <- strategy2[[trait_name]]
  dx = 0.01 *x
  strategy2[[trait_name]] <- x+dx
  dat2 <- figure_rate_vs_size_data(canopy_openness, strategy2)

  line1 <- (dat2$net_production - dat$net_production)/dx/dat$net_production
  line2  <- (1/(dat2$dleaf_area_dleaf_mass * dat2$leaf_fraction)
              - 1/(dat$dleaf_area_dleaf_mass * dat$leaf_fraction)) / dx /
            1/(dat2$dleaf_area_dleaf_mass * dat2$leaf_fraction)
  height <- dat$height

  plot(dat[["height"]], line1, type="l", xlab="Height (m)", ylab= "relative change", ylim=c(0,20))
  points(dat[["height"]], line2, type="l", col="red")
}

## Code for the "rates vs size" figure set:
figure_rate_vs_size <- function(data, type) {
  yvars <- figure_rate_vs_size_cols(type)
  par(mfrow=c(1, length(yvars)), oma=c(3,1,1,1))
  for (v in yvars) {
    plot(data[["height"]], data[[v]], type="l", xlab="", ylab= v, ylim=c(0, max(1,data[[v]])))
  }
  mtext("Height (m)", 1, cex=1, line=, outer=TRUE)

}

figure_rate_vs_size_panels <- function(data, type, path) {
  yvars <- figure_rate_vs_size_cols(type)
  for (v in yvars) {
    filename <- file.path(path, sprintf("F2_%s_%s.pdf", type, v))
    pdf(filename,width=5, height=5)
    on.exit(dev.off())
    par(oma=c(0,0,0,0), mar=rep(0.1,4))
    plot(data[["height"]], data[[v]], type="l", ann=FALSE, axes=FALSE,
      ylim=c(0, max(1,data[[v]])),
      col="green", lwd=3)
    box()

  }
}

figure_rate_vs_size_data <- function(canopy_openness=1, strategy=default_strategy()) {
  heights <- seq(Plant(strategy)$height, strategy$hmat, length.out=100)
  env <- fixed_environment(canopy_openness)
  res <- run_plant_to_heights(heights, strategy, env)
  i <- res[["net_production"]] > 0
  res[i, ]
}

figure_rate_vs_size_cols <- function(type) {
  if (type == "mass_production") {
    c("net_production", "assimilation", "respiration", "turnover")
  } else if (type == "height_growth") {
    c("height_growth_rate", "dheight_dleaf_area", "leaf_fraction", "growth_fraction", "net_production")
  } else if (type == "leaf_area_growth") {
    c("dleaf_area_dt", "leaf_fraction", "growth_fraction", "net_production")
  } else if (type == "diameter_growth") {
     c("dbasal_diam_dt", "leaf_fraction", "growth_fraction", "net_production")
  } else if (type == "basal_growth") {
     c("dbasal_area_dt", "leaf_fraction", "growth_fraction", "net_production")
  } else {
    stop("Unknown type ", dQuote(type))
  }
}

figure_diameter_growth <- function(dat=NULL) {
  if (is.null(dat)) {
    dat <- figure_diameter_growth_data()
  }
  diameters <- dat$diameters
  lai <- dat$lai
  traits <- dat$traits

  ymax <- max(sapply(dat[traits], function(x) max(x$dbasal_diam_dt)))
  ylim <- c(0, ymax * 1.1)
  cols <- rev(RColorBrewer::brewer.pal(length(lai) + 3, "Blues")[-(1:3)])
  par(mfcol=c(length(diameters), length(traits)),
      oma=c(4, 5, 0, 1.5), mar=c(1, 1, 1, 1))
  for (v in traits) {
    dat_v <- unname(split(dat[[v]], dat[[v]]$diameter_class))
    for (i in seq_along(diameters)) {
      dsub <- long_to_wide(dat_v[[i]], "canopy_openness",
                           c(v, "dbasal_diam_dt"))
      matplot(dsub[[1]], dsub[[2]], type="l", col=cols, lty=1, log="x",
              ylim=ylim, xaxt="n", yaxt="n", xlab="", ylab="")
      axis(1, labels=i == length(diameters), las=1)
      axis(2, labels=v == "lma", las=1)
      if (v == last(traits)) {
        mtext(sprintf("D=%sm", diameters[[i]]), 4, cex=1, line=1)
      }
    }
    mtext(name_pretty(v), 1, cex=1, line=3.5)
  }
  mtext(name_pretty("dbasal_diam_dt"), line=2.5, side=2, cex=1, outer=TRUE)
  legend("topright", paste(rev(lai), "m2"), lty=1, col=cols, bty="n")
}

figure_diameter_growth_data <- function() {
  diameters <- c(0.005, 0.01, 0.1, 0.2)
  vals <- list(lma=seq_log_range(trait_range("lma"),  20),
               rho=seq_log_range(trait_range("rho"), 20))
  lai <- c(0, 0.5, 1, 2, 3)

  canopy_openness <- exp(-Parameters()$c_ext * lai)
  dat_lma <- figure_diameter_growth_data1(canopy_openness,
                                         vals$lma, "lma", diameters)
  dat_rho <- figure_diameter_growth_data1(canopy_openness,
                                          vals$rho, "rho", diameters)
  list(traits=names(vals), lma=dat_lma, rho=dat_rho,
       diameters=diameters, lai=lai)
}

figure_diameter_growth_data1 <- function(canopy_openness,
                                        trait_values, trait_name,
                                        diameters) {
  ## The innermost function "run_trait_in_environment" runs a single
  ## trait in a single light environment.
  ##
  ## The middle function "run_traits_in_environment" runs a vector of
  ## traits (trait_values) in a single light environment.
  run_traits_in_environment <- function(canopy_openness) {
    run_trait_in_environment <- function(trait_value) {
      s <- default_strategy()
      s[[trait_name]] <- trait_value
      res <- run_plant_to_sizes(diameters, "diameter", s, env)
      tmp <- cbind(trait_value)
      colnames(tmp) <- trait_name
      cbind(tmp, res[c("diameter", "dbasal_diam_dt")],
            diameter_class=seq_along(diameters))
    }

    env <- fixed_environment(canopy_openness)
    res <- lapply(trait_values, run_trait_in_environment)
    ret <- do.call("rbind", res)
    cbind(canopy_openness=canopy_openness, ret)
  }

  ret <- lapply(canopy_openness, run_traits_in_environment)
  do.call("rbind", ret)
}

## Convert from long format to wide format -- not very general, used
## once.
long_to_wide <- function(dat, group, cols) {
  dsub <- unname(split(dat, dat[[group]]))
  ret <- lapply(cols, function(i) sapply(dsub, "[[", i))
  names(ret) <- cols
  ret
}

plant_list_set_height <- function(x, height) {
  for (i in x) {
    i$height <- height
  }
  invisible(x)
}

lcp_whole_plant_with_trait <- function(x, height,
                                       strategy=default_strategy()) {
  plants <- plant_list(x, strategy)
  plant_list_set_height(plants, height)
  sapply(plants, lcp_whole_plant)
}

figure_lcp_whole_plant <- function() {
  op <- par(oma=c(0, 4, 1, 1), mar=c(4, 1, 1, 1), mfcol=c(1, 2))
  for (trait in c("lma", "rho")) {
    xlim <- trait_range(trait)
    ylim <- c(0.2, 5)

    heights <- c(0.1, 2, 10, 20)
    x <- seq_log_range(xlim, 20)
    s <- default_strategy()
    cols <- RColorBrewer::brewer.pal(length(heights) + 3, "Blues")[-(1:3)]

    lcp <- sapply(heights, function(h)
                  lcp_whole_plant_with_trait(trait_matrix(x, trait), h, s))
    lai <- log(lcp) / (-Parameters()$c_ext)

    matplot(x, lai, type="l", lty=1, col=cols, log="xy", ylim=ylim,
            xlab=name_pretty(trait), ylab=name_pretty("shading"),
            yaxt="n")
    axis(2, labels=trait == "lma", las=1)
    if (trait == "lma") {
      mtext(name_pretty("shading"), 2, line=3, xpd=NA)
    }
  }
  legend("bottomright", paste(heights, "m"), lty=1, col=cols, bty="n")
}

trait_effects_data <- function(trait_name, size_name, relative=FALSE) {
  if (size_name == "diameter") {
    size_values <- c(0.0025, 0.01, 0.1, 0.5)
    titles <- sprintf("D=%0.2f (m)", size_values)
  } else {
    size_values <- c(0.25, 2, 8, 16)
    titles <- sprintf("H=%0.2f (m)", size_values)
  }
  trait_values <- seq_log_range(trait_range(trait_name), 50)
  s <- default_strategy()
  env <- fixed_environment(1.0)

  cols <- c("height_growth_rate", "dbasal_area_dt", "dbasal_diam_dt",
            "dabove_ground_mass_dt")
  if (relative) {
    cols <- paste0(cols, "_relative")
  }

  f <- function(s) {
    pp <- run_plant_to_sizes(size_values, size_name, s, env)
    tmp <- cbind(s[[trait_name]])
    colnames(tmp) <- trait_name
    cbind(tmp, size_class=seq_along(size_values), pp[c(size_name, cols)])
  }
  traits <- trait_matrix(trait_values, trait_name)
  ret <- do.call("rbind", lapply(strategy_list(traits, s), f))
  attr(ret, "info") <- list(size_name=size_name,
                            size_values=size_values,
                            trait_name=trait_name,
                            trait_values=trait_values,
                            growth_measures=cols)
  ret
}

trait_effects_plot <- function(d) {
  info <- attr(d, "info")
  growth_measures <- info$growth_measures
  dsplit <- split(d, d$size_class)
  trait <- info$trait_name
  ylim <- lapply(d[growth_measures], range)

  par(oma=c(6, 6, 2, 1), mar=c(1, 1, 2, 1),
      mfrow=c(length(growth_measures), length(info$size_values)))
  for (g in growth_measures) {
    for (i in seq_along(info$size_values)) {
      plot(dsplit[[i]][[trait]], dsplit[[i]][[g]], type="l",
           ylim=ylim[[g]], log="x", xaxt="n", yaxt="n")
      axis(1, labels=g == last(growth_measures))
      axis(2, labels=i == 1, las=1)
      if (i == 1L) {
        mtext(name_pretty(g), 2, line=3)
      }
      if (g == growth_measures[[1]]) {
        title <- sprintf("%s=%s (m)", toupper(substr(info$size_name,1,1)), prettyNum(info$size_values[[i]]))
        mtext(title, side=3, line=0.5, cex=1)
      }
    }
  }
  mtext(name_pretty(trait), side=1, line=4, cex=1, outer=TRUE)
}

figure_mass_fraction <- function() {
  heights <- seq(1, 50)
  strategy <- default_strategy()
  xlab <- "Height (m)"
  ylab <- "Fraction of live mass"

  vars <- c("leaf_mass", "root_mass", "bark_mass", "sapwood_mass",
            "heartwood_mass")
  cols <- c(leaf_mass="forestgreen", root_mass="tan", bark_mass="orange",
            sapwood_mass="firebrick2", heartwood_mass="brown")
  vars <- setdiff(vars, "heartwood_mass")

  p <- Plant(strategy)
  f <- function(h) {
    p$height <- h
    x <- p$vars_size[vars]
    cumsum(x) / sum(x)
  }
  y <- t(sapply(heights, f))

  par(mar=c(4.1, 4.1, 0.5, 6.1))
  plot(NA, type="n", xlim=range(heights), ylim=c(0, 1), las=1,
       xaxs="i", yaxs="i", xlab="Height (m)",
       ylab="Fraction of live mass")
  for (v in rev(vars)) {
    polygon(c(heights, last(heights), heights[1]), c(y[, v], 0, 0),
            col=cols[[v]], border=cols[[v]])
  }
  ylast <- c(0, y[nrow(y),])
  at <- (ylast[-1] + ylast[-length(ylast)]) / 2
  axis(4, at=at, labels=sub("_mass", "", vars), las=1)

  box()
}

empty_box <- function() {
  par(oma=c(0,0,0,0), mar=rep(0.1,4))
  plot(1,1, ann=FALSE, axes=FALSE, type='n')
  box()
}