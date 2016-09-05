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
                                B_ks2=0.0,
                                B_rs1=4012.0,
                                B_rb1=2.0*4012.0,
                                B_f1 =3.0,
                                narea=1.87e-3,
                                narea_0=1.87e-3,
                                B_lf1=5120.738 * 1.87e-3 * 24 * 3600 / 1e+06,
                                B_lf2=0.5,
                                B_lf3=0.04,
                                B_lf4=21000,
                                B_lf5=1,
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
        fit <- nls(AA ~ p1 * E/(p2 + E), data.frame(E = E, AA = AA), start = list(p1 = 100, p2 = 0.2))
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
    hmat = 30.0,
    a_f1 = 0.8,
    a_f2 = 20,
    a_l1 = 2.17,
    a_l2 = 0.546,
    k_l  = 0.4565855 / 3)

  FF16_Parameters(
    hyperpar=make_hyperpar2(B_ks2=1.25),
    strategy_default = s
    )
}

default_strategy <- function() {

  p <- default_parameters()
  p$strategy_default
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
  x
}

## Code for trait derivative figure
## TODO: Can grader help here?
figure_trait_derivative <- function(type, trait_name="lma", canopy_openness=1) {
  strategy <- default_strategy()

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
  ret <- figure_dY_dt_data(sizes =  c(0.5, 2, 10, 15, 20),
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

lcp_whole_plant_with_trait <- function(x, height) {
  plants <- plant_list(x, default_parameters())
  plant_list_set_height(plants, height)
  sapply(plants, lcp_whole_plant)
}

figure_lcp_whole_plant <- function() {
  op <- par(oma=c(0, 4, 1, 1), mar=c(4, 1, 1, 1), mfrow=c(3, 1))
  for (trait in c("narea", "lma", "rho")) {
    xlim <- trait_range(trait)
    ylim <- c(0.2, 5)

    heights <- c(0.5, 2, 10, 15, 20)
    x <- seq_log_range(xlim, 60)
    cols <- color_pallete(length(heights) + 3)[-(1:3)]

    lcp <- sapply(heights, function(h)
                  lcp_whole_plant_with_trait(trait_matrix(x, trait), h))
    lai <- log(lcp) / (-default_parameters()$k_I)

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

  ## We need a Parameters object apparently.
  ss <- strategy_list(traits, default_parameters())
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
