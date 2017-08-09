##' Hyperparameters for FF16 physiological model
##' only difference to default hyperpar function is that
##' kb=ks
##' @title Hyperparameters for FF16 physiological model
##' @param lma_0 Central (mean) value for leaf mass per area [kg /m2]
##' @param B_kl1 Rate of leaf turnover at lma_0 [/yr]
##' @param B_kl2 Scaling slope for phi in leaf turnover [dimensionless]
##' @param rho_0 Central (mean) value for wood density [kg /m3]
##' @param B_dI1 Rate of instantaneous mortality at rho_0 [/yr]
##' @param B_dI2 Scaling slope for wood density in intrinsic mortality [dimensionless]
##' @param B_ks1 Rate of sapwood turnover at rho_0 [/yr]
##' @param B_ks2 Scaling slope for rho in sapwood turnover [dimensionless]
##' @param B_rs1 CO_2 respiration per unit sapwood volume [mol / yr / m3 ]
##' @param B_rb1 CO_2 respiration per unit sapwood volume [mol / yr / m3 ]
##' @param B_f1 Cost of seed accessories per unit seed mass [dimensionless]
##' @param narea nitrogen per leaf area [kg / m2]
##' @param narea_0 central (mean) value for nitrogen per leaf area [kg / m2]
##' @param B_lf1 Potential CO_2 photosynthesis at average leaf nitrogen [mol / d / m2]
##' @param B_lf2 Curvature of leaf photosynthetic light response curve [dimensionless]
##' @param B_lf3 Quantum yield of leaf photosynthetic light response curve [dimensionless]
##' @param B_lf4 CO_2 respiration per unit leaf nitrogen [mol / yr / kg]
##' @param B_lf5 Scaling exponent for leaf nitrogen in maximum leaf photosynthesis [dimensionless]
##' @param k_I light extinction coefficient [dimensionless]
##' @param latitude degrees from equator (0-90), used in solar model [deg]
##' @export
make_hyperpar2 <- function(
                                lma_0=0.1978791,
                                B_kl1=0.4565855,
                                B_kl2=1.71,
                                rho_0=608.0,
                                B_dI1=0.01,
                                B_dI2=0.0,
                                B_ks1=0.2,
                                B_ks2=1.25, #0.0,
                                B_rs1=4012.0,
                                B_rb1=2.0*4012.0,
                                B_f1 =3.0,
                                narea=1.87e-3,
                                narea_0=1.87e-3,
                                B_lf1=5120.738 * 1.87e-3 * 24 * 3600 / 1e+06,
                                B_lf2=0.75,
                                B_lf3=0.04,
                                B_lf4=21000*0.75,
                                B_lf5=0.5,
                                k_I=0.5,
                                latitude=0) {
  assert_scalar <- function(x, name=deparse(substitute(x))) {
    if (length(x) != 1L) {
      stop(sprintf("%s must be a scalar", name), call. = FALSE)
    }
  }
  assert_scalar(lma_0)
  assert_scalar(B_kl1)
  assert_scalar(B_kl2)
  assert_scalar(rho_0)
  assert_scalar(B_dI1)
  assert_scalar(B_dI2)
  assert_scalar(B_ks1)
  assert_scalar(B_ks2)
  assert_scalar(B_rs1)
  assert_scalar(B_rb1)
  assert_scalar(B_f1)
  assert_scalar(narea)
  assert_scalar(narea_0)
  assert_scalar(B_lf1)
  assert_scalar(B_lf2)
  assert_scalar(B_lf3)
  assert_scalar(B_lf4)
  assert_scalar(B_lf5)
  assert_scalar(k_I)
  assert_scalar(latitude)

  function(m, s, filter=TRUE) {
    with_default <- function(name, default_value=s[[name]]) {
      rep_len(if (name %in% colnames(m)) m[, name] else default_value,
              nrow(m))
    }
    lma       <- with_default("lma")
    rho       <- with_default("rho")
    omega     <- with_default("omega")
    narea     <- with_default("narea", narea)

    ## lma / leaf turnover relationship:
    k_l   <- B_kl1 * (lma / lma_0) ^ (-B_kl2)

    ## rho / mortality relationship:
    d_I  <- B_dI1 * (rho / rho_0) ^ (-B_dI2)

    ## rho / wood turnover relationship:
    k_s  <- B_ks1 *  (rho / rho_0) ^ (-B_ks2)
    k_b <- k_s

    ## rho / sapwood respiration relationship:

    ## Respiration rates are per unit mass, so this next line has the
    ## effect of holding constant the respiration rate per unit volume.
    ## So respiration rates per unit mass vary with rho, respiration
    ## rates per unit volume don't.
    r_s <- B_rs1 / rho
    # bark respiration follows from sapwood
    r_b <- B_rb1 / rho

    ## omega / accessory cost relationship
    a_f3 <- B_f1 * omega

    ## Narea, photosynthesis, respiration

    assimilation_rectangular_hyperbolae <- function(I, Amax, theta, QY) {
      x <- QY * I + Amax
      (x - sqrt(x^2 - 4 * theta * QY * I * Amax)) / (2 * theta)
    }

    ## Photosynthesis  [mol CO2 / m2 / yr]
    approximate_annual_assimilation <- function(narea, latitude) {
      E <- seq(0, 1, by=0.02)
      ## Only integrate over half year, as solar path is symmetrical
      D <- seq(0, 365/2, length.out = 10000)
      I <- plant:::PAR_given_solar_angle(plant:::solar_angle(D, latitude = abs(latitude)))

      Amax <- B_lf1 * (narea/narea_0) ^  B_lf5
      theta <- B_lf2
      QY <- B_lf3

      AA <- NA * E

      for (i in seq_len(length(E))) {
        AA[i] <- 2 * plant:::trapezium(D, assimilation_rectangular_hyperbolae(
                                    k_I * I * E[i], Amax, theta, QY))
      }
      if(all(diff(AA) < 1E-8)) {
        # line fitting will fail if all have are zero, or potentially same value
        ret <- c(last(AA), 0)
        names(ret) <- c("p1","p2")
      } else {
        fit <- nls(AA ~ p1 * E/(p2 + E),
                  data.frame(E = E, AA = AA),
                  start = list(p1 = 100, p2 = 0.2))
        ret <- coef(fit)
      }
      ret
    }

    # This needed in case narea has length zero, in which case trapezium fails
    a_p1 <- a_p2 <- 0 * narea
    ## TODO: Remove the 0.5 hardcoded default for k_I here, and deal
    ## with this more nicely.
    if (length(narea) > 0 || k_I != 0.5) {
      i <- match(narea, unique(narea))
      y <- vapply(unique(narea), approximate_annual_assimilation,
                  numeric(2), latitude)
      a_p1  <- y["p1", i]
      a_p2  <- y["p2", i]
    }

    ## Respiration rates are per unit mass, so convert to mass-based
    ## rate by dividing with lma
    ## So respiration rates per unit mass vary with lma, while
    ## respiration rates per unit area don't.
    r_l  <- B_lf4 * narea / lma

    extra <- cbind(k_l,                # lma
                   d_I, k_s, k_b, r_s, r_b, # rho
                   a_f3,               # omega
                   a_p1, a_p2,         # narea
                   r_l)                # lma, narea

    overlap <- intersect(colnames(m), colnames(extra))
    if (length(overlap) > 0L) {
      stop("Attempt to overwrite generated parameters: ",
           paste(overlap, collapse=", "))
    }

    ## Filter extra so that any column where all numbers are with eps
    ## of the default strategy are not replaced:
    if (filter) {
      if (nrow(extra) == 0L) {
        extra <- NULL
      } else {
        pos <- diff(apply(extra, 2, range)) == 0
        if (any(pos)) {
          eps <- sqrt(.Machine$double.eps)
          x1 <- extra[1, pos]
          x2 <- unlist(s[names(x1)])
          drop <- abs(x1 - x2) < eps & abs(1 - x1/x2) < eps
          if (any(drop)) {
            keep <- setdiff(colnames(extra), names(drop)[drop])
            extra <- extra[, keep, drop=FALSE]
          }
        }
      }
    }

    if (!is.null(extra)) {
      m <- cbind(m, extra)
    }
    m
  }
}

default_parameters <- function() {

  s <- FF16_Strategy(
    hmat = 15.0,
    a_f1 = 0.8,
    a_f2 = 10,
    a_l1 = 2.17,
    a_l2 = 0.5,
    k_l  = 0.4565855 / 3)

  FF16_Parameters(
    hyperpar=make_hyperpar2(),
    strategy_default = s
    )
}

default_strategy <- function() {

  p <- default_parameters()
  p$strategy_default
}

color_pallete1 <- function(n) {
  RColorBrewer::brewer.pal(n+1, "Blues")[-(1)]
}

color_pallete2 <- function(n=9) {
  if(n <= 9)
    RColorBrewer::brewer.pal(n=n, "Blues")
  else
    colorRampPalette(brewer.pal(9,"Blues")[-(1)])(n)
}

color_pallete3 <- function() {

  cols <- RColorBrewer::brewer.pal(9, "YlOrBr")
  c(mass_leaf="forestgreen", mass_root=cols[4], mass_bark=cols[6],
            mass_sapwood=cols[7], mass_heartwood=cols[9])
}

run_plant_to_sizes <- function(sizes, size_variable, strategy, env,
                               time_max=300, filter=FALSE) {
  pl <- FF16_PlantPlus(strategy)
  res <- grow_plant_to_size(pl, sizes, size_variable, env, time_max,
                            warn=FALSE, filter=filter)
  bind_rows(lapply(res$plant, extract_plant_info, env=env))
}

extract_plant_info <- function(plant, env) {
  if (is.null(plant)) {
    x <- unlist(FF16_PlantPlus(default_strategy())$internals)
    x[] <- NA_real_
  } else {
    plant$compute_vars_phys(env)
    plant$compute_vars_growth()
    x <- unlist(plant$internals)
  }
  ## Add relative measures:
  v <- c("height", "area_stem", "diameter_stem", "mass_above_ground")
  x[sprintf("%s_dt_relative", v)] <- x[sprintf("%s_dt", v)] / x[v]

  data.frame(t(x))
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
      darea_leaf_dmass_live =cols[8],
      area_stem_dt =cols[4],
      area_leaf_dt ="lightgreen",
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
  heights <- seq(FF16_Plant(strategy)$height, 30, length.out=200)
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
  E <- dat[["E"]]
  traits <- dat[["traits"]]

  cols <- color_pallete1(length(E))
  par(mfrow=c(length(sizes), length(traits)),
      oma=c(4, 5, 0, 1.5), mar=c(1, 1, 1, 0.9))

  extract_f <- function(v, i, dat) {
      dat_v <- unname(split(dat$data[[v]], dat$data[[v]]$class))
      dsub <- long_to_wide(dat_v[[i]], "canopy_openness",
                           c(v, vars[2]))
  }
  for (i in seq_along(sizes)) {

    dat2 <- lapply(traits, extract_f, i, dat)
    names(dat2) <- traits
    ymax <- max(sapply(dat2, function(x) max(x[[2]],  na.rm=TRUE)))
    for (v in traits) {
      dsub <- dat2[[v]]
      matplot(dsub[[1]], dsub[[2]], type="n", log="x",
              xaxt="n", yaxt="n", xlab="", ylab="",
              ylim = c(0, ymax*1.05))
      usr <- par("usr")
      rect(1E-5, -1, 1E4, 2*ymax, col = make_transparent("grey", 0.3), border=NA)
      obs <- trait_range_obs(v)
      rect(obs[1], -1, obs[2], 2*ymax, col = "white", border=NA)
      box()
      matplot(dsub[[1]], dsub[[2]], type="l", col=cols, lty=1, add=TRUE)

      # axes
      axis.log10(1, labels = (i == length(sizes)), las=1)
      axis(2, labels = (v == traits[1] ), las=1, at = seq(0, 20, by=get_by(ymax)))
      if (v == last(traits)) {
        mtext(dat[["label"]](sizes[[i]]), 4, cex=0.9, line=1)
      }
      if (i == length(sizes)) {
        mtext(name_pretty(v), 1, cex=0.9, line=3)
      }

    }
  }
  mtext(name_pretty(vars[2]), line=3, side=2, cex=0.9 , outer=TRUE)

  legend("topright", legend = E, lty=1, col=rev(cols), bty="n", cex=0.9)
}

figure_height_dt_data <- function(sizes =  c(0.5, 2, 10, 15, 20)) {
  ret <- figure_dY_dt_data(sizes =  sizes,
        vars = c("height", "height_dt")
        )
  ret[["label"]] <- function(x) sprintf("H=%sm",x)
  ret
}


figure_dY_dt_grid <- function(dat) {

  vars <- dat[["vars"]]
  sizes <- dat[["sizes"]]
  E <- dat[["E"]]
  traits <- dat[["traits"]]

  par(oma=c(4, 5, 0, 3), mar=c(1, 1, 1, 0.9))

  layout(matrix(1:5, 1, 5, byrow = TRUE), c(1, 1, 1, 1, 0.3), c(1))

  nz <- 100
  cols <- rev(colorRampPalette(brewer.pal(9,"RdYlBu"))(nz))
  zmax <- 3
  levels <- seq_range(c(0, zmax), nz)

  for (v in traits) {

    out <- collapse_grid(dat$data[[v]], "class", v, "height_dt")
    x <- log10(out[[v]])
    y <- dat[["sizes"]]
    z <- out[["height_dt"]]

    # cap z data
    z[z > zmax] <- zmax

    # make plot boundaries
    xlim <- range(x)
    ylim <- range(y)
    plot.new()
    plot.window(xlim = xlim, ylim = ylim, "", xaxs = "i", yaxs = "i", las = 1)
    box()
    rect(xlim[1], ylim[1], xlim[2], ylim[2], col="grey")

    # add contour
    .filled.contour(x,y,z, levels, cols)

    # Calculate maximum of x for each value of y
    # index_max <- function(y) {
    #   if(all(is.na(y))) return(NA)
    #   which(y==max(y, na.rm=TRUE))
    # }
    # x_max <-  x[unlist(apply(out[["height_dt"]], 2, index_max))]
    # lines(x_max, y, lwd=2)

    # axes
    axis.log10(1, labels = TRUE, las=1, baseAxis=FALSE)
    axis(2, labels = (v == traits[1] ), las=1, at = seq(0, 20, by=5))
    mtext(name_pretty(v), 1, cex=0.9, line=3)
  }

  mtext("Height (m)", line=3, side=2, cex=0.9 , outer=TRUE)
  plot_colorbar(cols, levels, 6, "Height growth rate (m/yr)")
}


plot_colorbar <- function(cols, levels, nticks = 5, lab="") {

  min <- min(levels)
  max <- max(levels)
  ticks <- seq(min, max, len=nticks)
  scale <- (length(cols)-1)/(max-min)
  plot(c(0,10), c(min,max), type='n', bty='n', xaxt='n',
      xaxs='i', yaxs='i', xlab='', yaxt='n', ylab='')
  axis(4, ticks, las=1)
  mtext(lab, side=3, line=1)

  for (i in 1:(length(cols)-1)) {
    y = (i-1)/scale + min
    rect(0,y,10,y+1/scale, col=cols[i], border=NA)
  }
  box()
}

figure_height_dt_data_grid <- function(E=c(1)) {
  n <- 100
  ret <- figure_dY_dt_data(
        sizes =  seq_range(c(0.25, 20), n),
        vars = c("height", "height_dt"),
        vals = list(
               hmat=seq_log_range(trait_range("hmat"), n),
               narea=seq_log_range(trait_range("narea"), n),
               lma=seq_log_range(trait_range("lma"), n),
               rho=seq_log_range(trait_range("rho"), n)
              ),
        E = E
    )
  ret
}

figure_diameter_stem_dt_data <- function() {
  ret <- figure_dY_dt_data(sizes =  c(0.005, 0.01, 0.1, 0.2),
        vars = c("diameter_stem", "diameter_stem_dt")
        )
  ret[["label"]] <- function(x) sprintf("D=%sm",x)
  ret
}

figure_area_stem_dt_data <- function() {
  ret <- figure_dY_dt_data(sizes =  c(1E-5, 1E-4, 1E-3, 5E-2),
        vars = c("area_stem", "area_stem_dt")
        )
  ret[["label"]] <- function(x) sprintf("A=%sm",x)
  ret
}

figure_mass_above_ground_dt_data <- function() {
  ret <- figure_dY_dt_data(sizes =  c(0.005, 0.01, 0.1, 1, 10),
        vars = c("mass_above_ground", "mass_above_ground_dt")
        )
  ret[["label"]] <- function(x) sprintf("M=%skg",x)
  ret
}

figure_dY_dt_data <- function(sizes, vars,
    vals = list(
               hmat=seq_log_range(trait_range("hmat"), 40),
               narea=seq_log_range(trait_range("narea"), 40),
               lma=seq_log_range(trait_range("lma"), 40),
               rho=seq_log_range(trait_range("rho"), 40)
               ),
    E = c(1, 0.75, 0.5, 0.25)
    ) {

  data <- lapply(names(vals), function(x)
                    figure_dY_dt_data_worker(E,
                                           vals[[x]], x, sizes, vars))
  names(data) <- names(vals)
  list(vars = vars,
      traits=names(vals),
      data=data,
      sizes=sizes,
      E=E,
      label = function(x) sprintf("X=%s",x))
}

figure_dY_dt_data_worker <- function(canopy_openness,
                                          trait_values, trait_name,
                                          sizes,
                                          vars) {
  ## The innermost function "run_trait_in_environment" runs a single
  ## trait in a single light environment.
  ##
  ## The middle function "run_traits_in_environment" runs a vector of
  ## traits (trait_values) in a single light environment.
  run_traits_in_environment <- function(canopy_openness) {
    run_trait_in_environment <- function(trait_value) {
      s <- strategy(trait_matrix(trait_value, trait_name),
              default_parameters())
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

figure_lcp_trait_data <- function() {

  f <- function(x, height) {
    plants <- plant_list(x, default_parameters())
    plant_list_set_height(plants, height)
    sapply(plants, lcp_whole_plant)
  }

  heights <- c(0.5, 5, 10, 20)
  traits <- c("narea", "lma", "rho")
  lcp <- list()
  x <- list()

  for (trait in traits) {
    x[[trait]] <- seq_log_range(trait_range(trait), 250)
    lcp[[trait]] <- sapply(heights, function(h)
                                      f(trait_matrix(x[[trait]], trait), h))
  }
  list(traits=traits, heights=heights, x=x, lcp=lcp)
}

figure_lcp_trait <- function(data) {

  mylabel <- function(txt) label(txt, -0.2, 1.2, xpd=NA, cex=1.5)

  cols <- rev(color_pallete1(length(data[["heights"]])))

  par(oma=c(0, 2, 3, 0), mar=c(5, 3, 1, 1), mfrow=c(1,3))
  lab = list(narea="a", lma= "b", rho = "c")

  for (trait in data[["traits"]]) {
    matplot(data[["x"]][[trait]], data[["lcp"]][[trait]],
            type="n", log="x", ylim=c(0,1),
            xlab="", ylab="", yaxt="n", xaxt="n")
    rect(1E-5, -1, 1E4, 10, col = make_transparent("grey", 0.3), border=NA)
    obs <- trait_range_obs(trait)
    rect(obs[1], -1, obs[2], 10, col = "white", border=NA)
    box()

    matplot(data[["x"]][[trait]], data[["lcp"]][[trait]],
              type="l", lty=1, col=cols, add=TRUE)

    axis.log10(1, labels=TRUE, las=1)
    axis(2, labels=TRUE, las=1)

    mylabel(sprintf("%s)", lab[[trait]]))

    mtext(name_pretty(trait), 1, line=3.5, cex=0.8)
    if(trait=="narea"){
        mtext(name_pretty("shading"), 2, line=3.5, cex=0.8)
    }
  }
  mtext(name_pretty("shading"), 1, line=1, xpd=NA, outer=TRUE)

  legend("topright", paste(data[["heights"]], "m"), lty=1, col=cols, bty="n")
}

figure_lcp_schematic <- function() {

  # calculates respiration, turnover and lcp for plant
  get_data <- function(height, strategy) {
    pl <- FF16_PlantPlus(strategy)
    pl$height <- height
    env <- fixed_environment(1)
    pl$compute_vars_phys(env)

    ret <- list(
      E_star = lcp_whole_plant(pl),
      turnover = data.frame(
                leaf=pl$internals$mass_leaf*s$k_l,
                root=pl$internals$mass_root*s$k_r,
                bark=pl$internals$mass_bark*s$k_b,
                sapwood=pl$internals$mass_sapwood*s$k_s)/
                  pl$internals$area_leaf,
      respiration = data.frame(
                leaf=pl$internals$mass_leaf*s$r_l,
                root=pl$internals$mass_root*s$r_r,
                bark=pl$internals$mass_bark*s$r_b,
                sapwood=pl$internals$mass_sapwood*s$r_s)/
                  pl$internals$area_leaf * (s$a_bio * s$a_y),
      internals = pl$internals)
    ret$all <- ret$respiration + ret$turnover
    ret
  }

  s=default_strategy()

  cols <- color_pallete3()
  names(cols) <- sub("mass_", "", names(cols))
  E <- seq(0, 1, length.out=50)
  heights <- c(1, 5, 10, 20)
  data <- lapply(heights, get_data, s)

  par(oma=c(0, 0, 0, 0), mar=c(5, 5, 1, 4))
  plot(NA, xlab = "", ylab="", xaxs="i", yaxs="i",
      xlim=c(0,1), ylim=c(0, 2.5), las=1)
  mtext("Canopy openness, E (0-1)", 1, line=3)
  mtext(expression(paste("Mass production or loss (kg ", m^-2~yr^-1, ")")), 2, line=3)

  # colour background
  rect(0, 0, 1, 2.5, col = cols["bark"], border=NA)

  # leaf and root for smallest size
  x <- data[[1]]$all
  rect(0, 0, 1, x[,"leaf"], col = cols["leaf"], border=NA)
  rect(0, x[,"leaf"], 1, x[,"root"]+x[,"leaf"], col = cols["root"], border=NA)

  # add lines for each height
  for(i in seq_along(heights)){
    y <- sum(data[[i]]$all)
    abline(h=y)
    axis(4, at = y, labels = sprintf("H=%dm", heights[i]), las=1)
    abline(v=data[[i]]$E_star, lty="dotted")
  }

  # photosynthesis per leaf area
  A <- (s$a_bio * s$a_y) * (s$a_p1 * E / (E + s$a_p2))
  # white above line, acts as clipping mask for everything above
  polygon(c(E, 1, 0, 0), c(A, 10, 10, 0), col="white", border=NA)
  #now add the line
  points(E, A, type="l", lty="dashed", lwd=2)

  # add dots for E_star at each height
  for(i in seq_along(heights)){
    points(data[[i]]$E_star, sum(data[[i]]$all), pch=16)
  }

  # labels
  text(rep(0.8,3), c(0.4, 0.875, 1.7), c("leaf", "root", "sapwood+bark"), col="white")
}

figure_mass_fraction <- function(data) {

  cols <- color_pallete3()
  vars <- names(cols)

  data <- data[!is.na(data[["height"]]), ]
  heights <- data[["height"]]
  y <- apply( data[, vars], 1, function(x) { cumsum(x) / sum(x)})
  y <- t(y)

  par(mar=c(2.1, 4.1, 0.5, 2))
  plot(NA, type="n", xlim=c(0, 24), ylim=c(0, 1), las=1,
       xaxs="i", yaxs="i", xlab="", ylab="", axes=FALSE)
  axis(1, at = c(0,5,10,15,20), labels = c(0,NA,10,NA,20))
  axis(2, at = c(0,0.25,0.5,0.75, 1), labels = c(0,NA,0.5,NA,1), las=1)

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
