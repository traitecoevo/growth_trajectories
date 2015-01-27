
default_strategy <- function() {
  strategy <- new(Strategy)
  strategy$set_parameters(structure(list(1.5, 0.4565855/3, 30, 0.8, 20, 0.2, 2.17, 0.546), names = c("B5", "k_l0", "hmat", "c_r1", "c_r2", "k_s0", "a1", "B1")))
  strategy
}

## This makes a fixed light environment over the plant height,
fixed.environment <- function(E, height, n = 101, light.env = NULL, n.strategies = 1, seed.rain = 0) {
  if (length(seed.rain) == 1)
    seed.rain <- rep(seed.rain, n.strategies)
  hh <- seq(0, height, length = n)
  if (is.null(light.env))
    light.env <- function(x) rep(E, length.out = length(x))
  ee <- light.env(hh)
  env <- new(Interpolator)
  env$init(hh, ee)

  parameters <- new(Parameters)
  for (i in seq_len(n.strategies)) parameters$add_strategy(default_strategy())
  parameters$seed_rain <- seed.rain

  ret <- new(Environment, parameters)
  ret$light_environment <- env
  attr(ret, "light.env") <- light.env
  ret
}

# runs a new plant in given environment
run_plant <- function(plant, E = 1) {

  env <- fixed.environment(E)
  plant$compute_vars_phys(env)

  x <- data.frame(t(plant$vars_size), t(plant$vars_phys), t(plant$vars_growth_decomp))

  # add relative measures
  x[["height_growth_rate_relative"]] <- x[["height_growth_rate"]]/x[["height"]]
  x[["dbasal_area_dt_relative"]] <- x[["dbasal_area_dt"]]/x[["area_basal"]]
  x[["dbasal_diam_dt_relative"]] <- x[["dbasal_diam_dt"]]/x[["diameter"]]
  x[["dmass_above_ground_dt_relative"]] <- x[["dmass_above_ground_dt"]]/x[["mass_above_ground"]]

  x
}

change_with_height <- function(h = seq(0.2, 1, length.out = 10), E = 1, strategy = default_strategy()) {

  myfun <- function(x) {
    plant <- new(Plant, strategy)
    plant$height <- x
    run_plant(plant, E)
  }

  lapply(h, myfun)
}

# e.g.  = function(plant){plant$height - 0.25}
change_with_light <- function(E = seq(0.1, 1, length.out = 20), distance_from_target_fn, strategy = default_strategy()) {

  myfun <- function(x) {
    plant1 <- new(Plant, strategy)
    plant1$height <- 0.1
    plant <- grow.plant.to.size(plant1, fixed.environment(x), distance.from.target = distance_from_target_fn)
    run_plant(plant, x)
  }

  lapply(E, myfun)
}


# given a vector of values x for a given parameter with name 'trait', runs plants across range of values for x e.g. distance_from_target_fn =
# function(plant){plant$height - 0.25}
change_with_trait <- function(x, trait, E, distance_from_target_fn, strategy = default_strategy()) {

  # Clones strategy, changes value of 'trait' to x then runs plant
  myfun <- function(x) {
    strategy <- strategy$copy()
    strategy$set_parameters(structure(list(x), names = trait))
    plant1 <- new(Plant, strategy)
    plant1$height <- 0.1
    plant <- try(grow.plant.to.size(plant1, fixed.environment(E), distance.from.target = distance_from_target_fn), silent = TRUE)
    if (inherits(plant, "try-error"))
      return(NULL)
    run_plant(plant, E)
  }

  lapply(x, myfun)
}

# estimates whole-plant-light-compensation-point for plant with given height and strategy
wplcp <- function(h = 0.2, strategy = default_strategy()) {

  # runs a plant with given height, environment and strategy, returns mass production used as wrapper for root solving, to pass to wplcp
  run_plant_production <- function(E) {
    plant <- new(Plant, strategy)
    plant$height <- h
    run_plant(plant, E)[["net_production"]]
  }

  out <- try(uniroot(run_plant_production, c(0, 1))$root, silent = TRUE)
  if (inherits(out, "try-error"))
    return(NA)
  out
}

wplcp_with_height <- function(h, strategy = default_strategy()) {
  sapply(h, function(x) wplcp(x, strategy))
}

# Clones strategy, changes value of 'trait' to x then runs plant
modify_strategy_then_find_wplcp <- function(x, trait, strategy, ...) {
  strategy <- strategy$copy()
  strategy$set_parameters(structure(list(x), names = trait))
  wplcp(strategy = strategy, ...)
}

wplcp_with_trait <- function(x, trait, strategy = default_strategy(), h = 0.2) {
  sapply(x, modify_strategy_then_find_wplcp, strategy = strategy, trait = trait, h = h)
}

# finds tarit value in range that maximises growth rate at given size and light
maximise_growth_rate_by_trait <- function(trait, range, h, E, strategy = default_strategy(), outcome ="height_growth_rate", tol=1e-6) {

  if (length(h) > 1) {
    cat("error, h must have length 1")
  }
  if (length(E) > 1) {
    cat("error, E must have length 1")
  }

  # wrapper function to pass to optimise
  f <- function(x) {

    strategy <- strategy$copy()
    strategy$set_parameters(structure(list(x), names = trait))
    plant <- new(Plant, strategy)
    plant$height <- h

    run_plant(plant, E)[[outcome]]
  }

  opt <- optimise(f, range, maximum = TRUE, tol = tol)

  if (opt$objective > 0)
    out <- opt$maximum
  else out <- NA

  out
}

# solve for trait value maximising growth over different light environments
trait_maximimum_with_light <- function(E, trait, range, h = 0.2, strategy = default_strategy()) {
  sapply(E, function(x) maximise_growth_rate_by_trait(trait, range, h, x, strategy))
}

# solve for trait value maximising growth over sizes
trait_maximimum_with_size <- function(h, trait, range, E = 1, strategy = default_strategy()) {
  sapply(h, function(x) maximise_growth_rate_by_trait(trait, range, x, E, strategy))
}
