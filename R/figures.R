default_strategy <- function() {
  FF16_Strategy(
    hmat = 30.0,
    a_f1 = 0.8,
    a_f2 = 20,
    a_l1 = 2.17,
    a_l2 = 0.546,
    k_l  = 0.4565855 / 3
      )
}

color_pallete <- function(n) {
  RColorBrewer::brewer.pal(n, "Blues")
}

run_plant_to_sizes <- function(sizes, size_variable, strategy, env,
                               time_max=300, filter=FALSE) {
  pl <- FF16_PlantPlus(strategy)
  res <- grow_plant_to_size(pl, sizes, size_variable, env, time_max,
                            warn=FALSE, filter=filter)
  data.frame(rbind_list(lapply(res$plant, extract_plant_info, env=env)))
}

extract_plant_info <- function(plant, env) {
  if (is.null(plant)) {
    x <- unlist(FF16_PlantPlus(FF16_Strategy())$internals)
    x[] <- NA_real_
  } else {
    plant$compute_vars_phys(env)
    plant$compute_vars_growth()
    x <- unlist(plant$internals)
  }
  ## Add relative measures:
  v <- c("height", "area_stem", "diameter_stem", "mass_above_ground")
  x[sprintf("%s_dt_relative", v)] <- x[sprintf("%s_dt", v)] / x[v]
  x
}

## Code for trait derivative figure
## TODO: Can grader help here?
figure_trait_derivative <- function(type, trait_name="lma", canopy_openness=1,
                                    strategy=default_strategy()) {
  p0 <- FF16_Parameters(strategy_default=strategy)
  ## strategy(trait_matrix( traits, p)

  ## TODO: This seems tragically broken because it apparently computes
  ## derivative with a value at 0.05 regardless of the trait?  I've
  ## disabled that for now.
  ## strategy[[trait_name]] <- 0.05
  dat <- figure_rate_vs_size_data(canopy_openness, strategy)
  strategy2 <- strategy
  x <- strategy2[[trait_name]]
  dx = 0.01 * x
  strategy2[[trait_name]] <- x + dx
  dat2 <- figure_rate_vs_size_data(canopy_openness, strategy2)

  line1 <- (dat2$net_mass_production_dt - dat$net_mass_production_dt)/dx/dat$net_mass_production_dt
  line2  <- (1/(dat2$darea_leaf_dmass_leaf * dat2$darea_leaf_dmass_live)
    - 1/(dat$darea_leaf_dmass_leaf * dat$darea_leaf_dmass_live)) / dx /
              1/(dat2$darea_leaf_dmass_leaf * dat2$darea_leaf_dmass_live)
  height <- dat$height

  plot(dat[["height"]], line1, type="l",
       xlab="Height (m)", ylab="relative change", ylim=c(0, 20))

  ## TODO: this does not work because darea_leaf_dmass_leaf is no
  ## longer calculated
  ##   lines(dat[["height"]], line2, col="red")
}

## Code for the "rates vs size" figure set:
figure_rate_vs_size <- function(data, type) {
  yvars <- figure_rate_vs_size_cols(type)
  par(mfrow=c(1, length(yvars)), oma=c(3,1,1,1))
  for (v in yvars) {
    plot(data[["height"]], data[[v]], type="l", xlab="", ylab= v, ylim=c(0, max(1,data[[v]], na.rm=TRUE)))
  }
  mtext("Height (m)", 1, cex=1, line=, outer=TRUE)
}

figure_rate_vs_size_panels <- function(data, type, path) {

  strategy=default_strategy()

  yvars <- figure_rate_vs_size_cols(type)

  data[["yield"]] <- strategy[["a_y"]]
  data[["dmass_leaf_darea_leaf"]] <- data[["mass_leaf"]] / data[["area_leaf"]]
  data[["darea_sapwood_darea_leaf"]] <- data[["area_sapwood"]] / data[["area_leaf"]]
  data[["darea_bark_darea_leaf"]] <- data[["area_bark"]] / data[["area_leaf"]]

  get_col <- function(yvar){

    cols <- RColorBrewer::brewer.pal(9, "Set1")
    switch(yvar,
      net_mass_production_dt=cols[1],
      fraction_allocation_growth=cols[2],
      darea_leaf_dmass_live =cols[3],
      area_stem_dt =cols[4],
      area_leaf_dt =cols[5],
      "black")
  }

  for (v in yvars) {
    filename <- file.path(path, sprintf("%s_%s.pdf", type, v))
    pdf(filename,width=5, height=6)
    par(oma=c(0,0,0,0), mar=c(6, 0.1,0.1,0.1))
    plot(data[["height"]], data[[v]], type="l", ann=FALSE, axes=FALSE,
       xlim=c(0, 25),
#       xaxs="i", yaxs = "i",
      col=get_col(v), lwd=8)
    if(type == "diameter_stem_dt" || (type == "area_stem_dt"  && v %in% c( "area_leaf_dt", "area_heartwood_dt"))){
      axis(1, at=seq(0,30, by=10), tck=-0.05, cex.axis=4, mgp=c(3, 4, 0))
    }
    box()
    dev.off()
  }
}

figure_rate_vs_size_data <- function(canopy_openness=1,
                                     strategy=default_strategy()) {
  heights <- seq(FF16_Plant(strategy)$height, strategy$hmat, length.out=100)
  env <- fixed_environment(canopy_openness)
  run_plant_to_sizes(heights, "height", strategy, env, time_max=300)
}

figure_rate_vs_size_cols <- function(type) {
  if (type == "net_mass_production_dt") {
    c("net_mass_production_dt", "yield", "assimilation", "respiration", "turnover")
  } else if (type == "mass_total_dt") {
    c("mass_total_dt", "fraction_allocation_growth", "net_mass_production_dt", "mass_heartwood_dt")
  } else if (type == "darea_leaf_dmass_live") {
    c("darea_leaf_dmass_live", "dmass_leaf_darea_leaf", "dmass_sapwood_darea_leaf", "dmass_bark_darea_leaf", "dmass_root_darea_leaf")
  } else if (type == "area_leaf_dt") {
    c("area_leaf_dt", "darea_leaf_dmass_live", "fraction_allocation_growth", "net_mass_production_dt")
  } else if (type == "height_dt") {
    c("height_dt", "dheight_darea_leaf", "area_leaf_dt")
  } else if (type == "area_stem_dt") {
    c("area_stem_dt", "darea_sapwood_darea_leaf", "darea_bark_darea_leaf", "area_leaf_dt", "area_heartwood_dt")
  } else if (type == "diameter_stem_dt") {
    c("diameter_stem_dt", "ddiameter_stem_darea_stem", "area_stem_dt")

  } else {
    stop("Unknown type ", dQuote(type))
  }
}

figure_dY_dt <- function(dat) {

  vars <- dat[["vars"]]
  sizes <- dat[["sizes"]]
  lai <- dat[["lai"]]
  traits <- dat[["traits"]]

  ymax <- max(sapply(dat[traits], function(x) max(x[[vars[2]]], na.rm=TRUE)))
  ylim <- c(0, ymax * 1.1)
  cols <- rev(color_pallete(length(lai) + 3)[-(1:3)])
  par(mfcol=c(length(sizes), length(traits)),
      oma=c(4, 5, 0, 1.5), mar=c(1, 3, 1, 0.9))
  for (v in traits) {
    dat_v <- unname(split(dat$data[[v]], dat$data[[v]]$class))
    for (i in seq_along(sizes)) {
      dsub <- long_to_wide(dat_v[[i]], "canopy_openness",
                           c(v, vars[2]))
      matplot(dsub[[1]], dsub[[2]], type="l", col=cols, lty=1, log="x",
              #ylim = ylim,
              xaxt="n", yaxt="n", xlab="", ylab="")
      axis(1, labels=i == length(sizes), las=1)
      #axis(2, labels=v == "narea", las=1)
      axis(2, labels = TRUE, las=1)
      if (v == last(traits)) {
        mtext(dat[["label"]](sizes[[i]]), 4, cex=1, line=1)
      }
    }
    mtext(name_pretty(v), 1, cex=1, line=3.5)
  }
  mtext(name_pretty(vars[2]), line=2.5, side=2, cex=1, outer=TRUE)
  legend("topright", paste(rev(lai), "m2"), lty=1, col=cols, bty="n")
}


figure_height_dt_data <- function() {
  ret <- figure_dY_dt_data(sizes =  c(0.5, 2, 10, 20),
        vars = c("height", "height_dt")
        )
  ret[["label"]] <- function(x) sprintf("H=%sm",x)
  ret
}

figure_diameter_stem_dt_data <- function() {
  ret <- figure_dY_dt_data(sizes =  c(0.005, 0.01, 0.1, 0.2),
        vars = c("diameter_stem", "diameter_stem_dt")
        )
  ret[["label"]] <- function(x) sprintf("D=%sm",x)
  ret
}

figure_mass_above_ground_dt_data <- function() {
  ret <- figure_dY_dt_data(sizes =  c(0.005, 0.01, 0.1, 1, 10),
        vars = c("mass_above_ground", "mass_above_ground_dt")
        )
  ret[["label"]] <- function(x) sprintf("M=%skg",x)
  ret
}

figure_dY_dt_data <- function(sizes, vars) {

  vals <- list(narea=seq_log_range(trait_range("narea"), 40),
               lma=seq_log_range(trait_range("lma"), 40),
               rho=seq_log_range(trait_range("rho"), 40)
               )
  lai <- c(0, 0.5, 1, 2, 3)

  canopy_openness <- exp(-default_parameters()$k_I * lai)
  data <- lapply(names(vals), function(x)
                    figure_dY_dt_data_worker(canopy_openness,
                                           vals[[x]], x, sizes, vars))
  names(data) <- names(vals)
  list(vars = vars,
      traits=names(vals),
      data=data,
      sizes=sizes,
      lai=lai,
      label = function(x) sprintf("X=%s",x))
}

figure_dY_dt_data_worker <- function(canopy_openness,
                                          trait_values, trait_name,
                                          sizes,
                                          vars) {
  p0 <- FF16_Parameters(strategy_default=default_strategy())
  ## The innermost function "run_trait_in_environment" runs a single
  ## trait in a single light environment.
  ##
  ## The middle function "run_traits_in_environment" runs a vector of
  ## traits (trait_values) in a single light environment.
  run_traits_in_environment <- function(canopy_openness) {
    run_trait_in_environment <- function(trait_value) {
      s <- strategy(trait_matrix(trait_value, trait_name), p0)
      res <- run_plant_to_sizes(sizes, vars[1], s, env)
      tmp <- cbind(trait_value)
      colnames(tmp) <- trait_name
      cbind(tmp, res[vars],
            class=seq_along(sizes))
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
  plants <- plant_list(x, FF16_Parameters(strategy_default=strategy))
  plant_list_set_height(plants, height)
  sapply(plants, lcp_whole_plant)
}

figure_lcp_whole_plant <- function() {
  op <- par(oma=c(0, 4, 1, 1), mar=c(4, 1, 1, 1), mfrow=c(3, 1))
  for (trait in c("narea", "lma", "rho")) {
    xlim <- trait_range(trait)
    ylim <- c(0.2, 5)

    heights <- c(0.5, 2, 10, 20)
    x <- seq_log_range(xlim, 20)
    s <- default_strategy()
    cols <- color_pallete(length(heights) + 3)[-(1:3)]

    lcp <- sapply(heights, function(h)
                  lcp_whole_plant_with_trait(trait_matrix(x, trait), h, s))
    lai <- log(lcp) / (-FF16_Parameters()$k_I)

    matplot(x, lai, type="l", lty=1, col=cols, log="xy", ylim=ylim,
            xlab=name_pretty(trait), ylab=name_pretty("shading"),
            yaxt="n")
    axis(2, labels=TRUE, las=1)
    if (trait == "lma") {
      mtext(name_pretty("shading"), 2, line=3, xpd=NA)
    }
  }
  legend("bottomright", paste(heights, "m"), lty=1, col=cols, bty="n")
}

trait_effects_data <- function(trait_name, size_name, relative=FALSE) {
  if (size_name == "diameter_stem") {
    size_values <- c(0.0025, 0.01, 0.1, 0.5)
    titles <- sprintf("D=%0.2f (m)", size_values)
  } else {
    size_values <- c(0.25, 2, 8, 16)
    titles <- sprintf("H=%0.2f (m)", size_values)
  }
  env <- fixed_environment(1.0)

  cols <- c("height_dt", "area_stem_dt", "diameter_stem_dt",
            "mass_above_ground_dt")
  if (relative) {
    cols <- paste0(cols, "_relative")
  }

  f <- function(s) {
    pp <- run_plant_to_sizes(size_values, size_name, s, env)
    tmp <- cbind(s[[trait_name]])
    colnames(tmp) <- trait_name
    cbind(tmp, size_class=seq_along(size_values), pp[c(size_name, cols)])
  }

  trait_values <- seq_log_range(trait_range(trait_name), 50)
  traits <- trait_matrix(trait_values, trait_name)

  ## OK, strategy_list has totally changed; we need a Parameters
  ## object apparently.
  p <- FF16_Parameters(strategy_default=default_strategy())
  ss <- strategy_list(traits, p)
  dat <- lapply(ss, f)

  dat <- vector("list", length(ss))
  for (i in seq_along(ss)) {
    dat[[i]] <- f(ss[[i]])
  }

  ret <- do.call("rbind", dat)

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
  ylim <- lapply(d[growth_measures], range, na.rm=TRUE)

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

figure_mass_fraction <- function(data) {

  vars <- c("mass_leaf", "mass_root", "mass_bark", "mass_sapwood",
            "mass_heartwood")
  cols <- c(mass_leaf="forestgreen", mass_root="orange", mass_bark="firebrick2",
            mass_sapwood="brown", mass_heartwood="tan")

  data <- data[!is.na(data[["height"]]), ]
  heights <- data[["height"]]
  y <- apply( data[, vars], 1, function(x) { cumsum(x) / sum(x)})
  y <- t(y)

  par(mar=c(2.1, 4.1, 0.5, 2))
  plot(NA, type="n", xlim=c(0, 24), ylim=c(0, 1), las=1,
       xaxs="i", yaxs="i", xlab="", ylab="", axes=FALSE)
  axis(1, at = c(0,5,10,15,20), labels = c(0,NA,10,NA,20))
  axis(2, at = c(0,0.25,0.5,0.75, 1), labels = c(0,NA,0.5,NA,1), las=1)

  #mtext("Height (m)",1, cex=1.5, line=2.5)
  mtext("Fraction of mass",2, cex=1.25, line=2.5)

  for (v in rev(vars)) {
    polygon(c(heights, last(heights), heights[1]), c(y[, v], 0, 0),
            col=cols[[v]], border=cols[[v]])
  }

  x <- c(2, 6, 10, 15, 22)
  y <- c(0.15, 0.195, 0.2, 0.45, 0.8)
  text(x,y, sub("mass_", "", vars), col="white")
  box()
}

empty_box <- function() {
  par(oma=c(0,0,0,0), mar=rep(0.1,4))
  plot(1,1, ann=FALSE, axes=FALSE, type='n')
  box()
}
