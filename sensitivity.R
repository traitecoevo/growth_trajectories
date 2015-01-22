# Fitting TREE to observed data

sourceDir <- function(path, ...) {
     for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
        source(file.path(path, nm), ...)
     }
}
sourceDir("R")

library(tree)
library(maker)
library(plyr)
library(dplyr)

# First, look at plot: challenge is to move peak to left and pull down elevation of curve.

m <- maker::maker()
data <- m$get("BCI_species_data") %>%
  mutate(dbh=at) %>%
  filter(!is.na(lma*rho*hmat*dbh*dbasal_diam_dt), dbh %in% c(0.01), hmat > 10) %>%
  select(dbh, lma, rho, dbasal_diam_dt)

figure_trait_effect_at_size("lma", "dbasal_diam_dt", "diameter", 0.01, xlim = c(0.01, 0.1), strategy = default_strategy())
points(data$lma, data$dbasal_diam_dt)

# given a dbh x, traits and parameters, grows plant to right size and estimates growth rate
growth <- function(trait, x, D, E=1, strategy = default_strategy()) {

  distance_from_target_fn <- function(plant) {
    plant$vars_size[["diameter"]] - D
  }

  strategy <- strategy$copy()
  strategy$set_parameters(structure(list(x), names = trait))
  plant <- new(Plant, strategy)
  plant$height <- 0.1

  plant <- try(grow.plant.to.size(plant, fixed.environment(E), distance.from.target = distance_from_target_fn), silent = FALSE)
  if (inherits(plant, "try-error"))
    return(NULL)

  run_plant(plant, E)
}

growth_outcome <- function(...,  outcome ="dbasal_diam_dt") {
  g <- growth(...)
  if(is.null(g))
    return(0)
  g[[outcome]]
}

# finds tarit value in range that maximises growth rate at given size and light
maximise_growth_rate_by_trait <- function(trait, range, D, E=1, strategy = default_strategy(), outcome ="dbasal_diam_dt", tol=1e-6) {

 # wrapper function to pass to optimise
  f <- function(x) {
      growth_outcome(trait, x, D, E, strategy, outcome=outcome)
    }

 optimise(f, range, maximum = TRUE, tol = tol)
}

# finds tarit value in range that maximises growth rate at given size and light
maximise_growth_rate_by_trait("lma", range = c(0.001, 1), D=0.01, E=1, strategy =strategy, outcome ="dbasal_diam_dt", tol=1e-4)

# Sensitivity to pars
exclude <- c("c_acc","c_bio","c_d0","c_d2","c_d3","c_p2","c_r1","c_r2","c_s0","hmat","hmat_0","lma","lma_0","s","s_0","B5","B6","c_d1", "B7", "n_area_0", "rho_0")
par_names <- names(strategy$parameters)[!names(strategy$parameters) %in% exclude]

pars <- expand.grid(par=par_names, adj=c(1.01), stringsAsFactors=FALSE)

run_variant <- function(par, adj, strategy = default_strategy()) {

  strategy <- strategy$copy()
  x <- strategy$parameters[[par]]
  strategy$set_parameters(structure(list(x*adj), names = par))
  max_lma <- maximise_growth_rate_by_trait("lma", range = c(0.001, 1), D=0.01, E=1, strategy =strategy, outcome ="dbasal_diam_dt", tol=1e-4)
  data.frame(theta_0=x, theta=x*adj, maximum = max_lma$maximum, objective=max_lma$objective)
}

base <- run_variant("c_acc", 1.0)

elasticity <- ddply(pars, c("par", "adj"), function(x) run_variant(x$par, x$adj))
elasticity$maximum_0 <- base$maximum
elasticity$objective_0 <- base$objective

CV <- function(x1,x2) {(x2-x1)/x1}

elasticity$el_max <- CV(elasticity[["maximum"]], elasticity[["maximum_0"]]) / CV(elasticity[["theta"]], elasticity[["theta_0"]])
elasticity$el_obj <- CV(elasticity[["objective"]], elasticity[["objective_0"]]) / CV(elasticity[["theta"]], elasticity[["theta_0"]])

elasticity[, c("par", "theta_0", "el_max", "el_obj")]
