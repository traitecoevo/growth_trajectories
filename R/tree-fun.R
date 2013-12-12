
library(tree)

## This makes a fixed light environment over the plant height,
test.environment <- function(E, height, n=101, light.env=NULL,
                             n.strategies=1, seed.rain=0) {
  if (length(seed.rain) == 1)
    seed.rain <- rep(seed.rain, n.strategies)
  hh <- seq(0, height, length=n)
  if (is.null(light.env))
    light.env <- function(x) E  ee <- light.env(hh)
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


## Grow the plant in a constant environment
derivs <- function(t, y, pars) {
  plant <- pars$plant
  light.env <- pars$light.env

  plant$set_ode_values(t, y)
  plant$compute_vars_phys(light.env)
  plant$ode_rates
}
