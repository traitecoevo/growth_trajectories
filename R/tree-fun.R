require(deSolve, quietly=TRUE)
require(dplyr, quietly=TRUE)
library(tree)

source("R/plot-utils.R")

## This makes a fixed light environment over the plant height,
fixed.environment <- function(E, height, n=101, light.env=NULL,
                             n.strategies=1, seed.rain=0) {
  if (length(seed.rain) == 1)
    seed.rain <- rep(seed.rain, n.strategies)
  hh <- seq(0, height, length=n)
  if (is.null(light.env))
    light.env <- function(x) rep(E, length.out=length(x))
  ee <- light.env(hh)
  env <- new(Interpolator)
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

# runs a new plant in given environment
run_plant <- function(plant, E=1){

  env <- fixed.environment(E)
  plant$compute_vars_phys(env)

  list(E = E,
     vars = data.frame(t(plant$vars_size), t(plant$vars_phys), t(plant$vars_growth_decomp))
    )
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

change_with_height <- function(h=seq(0.2, 1, length.out=10), E=1, strategy = new(Strategy)){

  myfun <- function(x){
    plant <- new(Plant, strategy)
    plant$height <- x
    run_plant(plant, E)
  }

  merge_list_components(lapply(h, myfun))
}

change_with_light <- function( h=0.2, E=seq(0.1, 1, length.out=20), strategy = new(Strategy)){

  myfun <- function(x){
    plant <- new(Plant, strategy)
    plant$height <- h
    run_plant(plant, x)
  }

  merge_list_components(lapply(E, myfun))
}


# given a vector of values x for a given parameter with name 'trait', runs plants across range of values for x
change_with_trait <- function(x, trait, h=0.2, E=1, strategy = new(Strategy)){

  # Clones strategy, changes value of "trait" to x then runs plant
  myfun <- function(x){
    strategy <- strategy$copy()
    strategy$set_parameters(structure(list(x), names=trait))
    plant <- new(Plant, strategy)
    plant$height <- h
    run_plant(plant, E)
  }

  c(merge_list_components(lapply(x,myfun)))
}


# estimates whole-plant-light-compensation-point for plant with given height and strategy
wplcp <- function(h=0.5, strategy = new(Strategy)){

  # runs a plant with given height, environment and strategy, returns mass production
  # used as wrapper for root solving, to pass to wplcp
  run_plant_production <- function(E){
    plant <- new(Plant, strategy)
    plant$height <- h

    run_plant(plant, E)$vars[["net_production"]]
  }

   uniroot(run_plant_production, c(0, 1))$root
}

wplcp_with_size <- function(h,strategy = new(Strategy)){
  sapply(h, function(x) wplcp(h=x, strategy=strategy))
}

# Clones strategy, changes value of "trait" to x then runs plant
modify_strategy_then_find_wplcp <- function(x, trait, h, strategy){
  strategy <- strategy$copy()
  strategy$set_parameters(structure(list(x), names=trait))
  wplcp(h=h, strategy=strategy)
}

wplcp_with_trait <- function(x, trait, h=0.2, strategy = new(Strategy)){
  sapply(x,modify_strategy_then_find_wplcp, h=h, strategy=strategy, trait=trait)
}

# finds tarit value in range that maximises growth rate at given size and light
maximise_growth_rate_by_trait <- function(trait, range, h, E, strategy = new(Strategy)){

  if(length(h)>1){cat("error, h must have length 1");}
  if(length(E)>1){cat("error, E must have length 1");}

  #wrapper function to pass to optimise
  dHdt.wrap <- function(x){

    strategy <- strategy$copy()
    strategy$set_parameters(structure(list(x), names=trait))
    plant <- new(Plant, strategy)
    plant$height <- h

    run_plant(plant, E)$vars[["height_growth_rate"]]
  }

  opt <- optimise(dHdt.wrap, range, maximum= TRUE, tol = 0.000001)

 if(opt$objective >0)
  out <- opt$maximum
 else
  out <- NA
 out
}

#solve for trait value maximising growth over different light environments
trait_maximimum_with_light <- function(E, trait, range, h=0.2, strategy = new(Strategy)){
  sapply(E, function(x) maximise_growth_rate_by_trait(trait, range, h, x, strategy))
}

#solve for trait value maximising growth over sizes
trait_maximimum_with_size <- function(h, trait, range, E=1, strategy = new(Strategy)){
  sapply(h, function(x) maximise_growth_rate_by_trait(trait, range, x, E, strategy))
}
