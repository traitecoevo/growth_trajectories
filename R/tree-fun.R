
.libPaths(c("lib", .libPaths()))  # will cause us to use local version of tree

library(tree)

## This makes a fixed light environment over the plant height,
fixed.environment <- function(E, height, n=101, light.env=NULL,
                             n.strategies=1, seed.rain=0) {
  if (length(seed.rain) == 1)
    seed.rain <- rep(seed.rain, n.strategies)
  hh <- seq(0, height, length=n)
  if (is.null(light.env))
    light.env <- function(x) rep(E, length.out=length(x))
  ee <- light.env(hh)
  env <- new(Spline)
  env$init(hh, ee)

  parameters <- new(Parameters)
  for (i in seq_len(n.strategies))
    parameters$add_strategy(new(Strategy))
  parameters$seed_rain <- seed.rain

  ret <- new(Environment, parameters)
  ret$light_environment <- env
  attr(ret, "light.env") <- light.env
  ret
}

# runs a new plant with given height, environment and strategy
run_plant <- function(h=0.5, E=1,  strategy = new(Strategy)){

  p <- new(Plant, strategy)

  p$height <- h
  env <- fixed.environment(E)
  p$compute_vars_phys(env)

  list(h=h,
     E = E,
    vars_size = data.frame(t(p$vars_size)),
    vars_phys = data.frame(t(p$vars_phys)))
}

change_with_size <- function(h=seq(0.2, 1, length.out=10), E=1, strategy = new(Strategy)){

  env <- fixed.environment(E)
  sp <- new(Species, strategy)

  for (i in 1:length(h))
       sp$add_seeds(1)
  sp$height <- sort(h, decreasing=TRUE)  # set heights, make sure decreasing

  sp$compute_vars_phys(env)
  list(h=sp$height,
    E=E,
    vars_size = data.frame(t(sp$vars_size)),
    vars_phys = data.frame(t(sp$vars_phys)))

}


# transforms a list of lists into a single list by applying do.call(rbind, ..) to each component of a list
# assumes all elements of parent list have same structure

merge_list_components <- function(myLists){

  out <- list()

  if(length(myLists) > 0){
    vars <- names(myLists[[1]])
    f <- function(v) do.call(rbind, lapply(myLists, "[[", v))
      out <- structure(lapply(vars, f), names=vars)
  }
  out
}

change_with_light <- function( h=0.2, E=seq(0.1, 1, length.out=20), strategy = new(Strategy)){

  tmp <- lapply(E, function(x) run_plant(h=h, E=x, strategy=strategy))

  merge_list_components(tmp)
}

# Clones strategy, changes value of "trait" to x then runs plant
modify_strategy_then_run_plant <- function(x, trait, h, E, strategy){
  strategy <- strategy$clone()
  strategy$set_parameters(structure(list(x), names=trait))
    run_plant(h=h, E=E, strategy=strategy)
}

# given a vector of values x for a given parameter with name 'trait', runs plants across range of values for x
change_with_trait <- function(x, trait, h=0.2, E=1, strategy = new(Strategy)){
  tmp <- lapply(x,modify_strategy_then_run_plant, h=h, E=E, strategy=strategy, trait=trait)
  c(merge_list_components(tmp), structure(list(x), names=trait))
}

# step with ode stepper
require(deSolve, quietly=TRUE)

step_ode <- function(light.env, plant, timesteps = seq(0, 15, length=51)){

  pars.derivs <- list(plant=p, light.env=env)
  derivs.d <- function(...) list(derivs(...))

  plant.run <- rk(y, timesteps, derivs.d, pars.derivs,
                       method=rkMethod("rk45ck"), hini=1/1000, rtol=1,
                       atol=1)

}

## Grow the plant in a constant environment
derivs <- function(t, y, pars) {
  plant <- pars$plant
  light.env <- pars$light.env

  plant$set_ode_values(t, y)
  plant$compute_vars_phys(light.env)
  plant$ode_rates
}
