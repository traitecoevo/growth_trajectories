
source("R/tree-fun.R")

s <- new(Strategy)
p <- new(Plant, s)

# set height
h0 <- 10
p$height <- h0

size.p <- p$vars_size

# setup light env
env <- fixed.environment(1)
light.env <- attr(env, "light.env")

## Compute the physiological variables and retreive them.
p$compute_vars_phys(env)
p.phys <- p$vars_phys
p$vars_phys[["height_growth_rate"]]  #dh/dt
p$vars_phys[["leaf_fraction"]]  #dml/dmt
p$vars_phys[["reproduction_fraction"]]  #1-r
# To do: write functions to access other required elements of growth decomposition, see https://github.com/dfalster/TraitGrowthTrajectories/issues/1

# step with ode stepper
pars.derivs <- list(plant=p, light.env=env)
tt <- seq(0, 15, length=51)
library(deSolve)
derivs.d <- function(...) list(derivs(...))

plant.run <- rk(y, tt, derivs.d, pars.derivs,
                     method=rkMethod("rk45ck"), hini=1/1000, rtol=1,
                     atol=1)
