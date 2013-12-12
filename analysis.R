
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

## Compute physiological variables across range of heights for single species

sp <- new(Species, s)

## Add four plants with random sizes.
for (i in 1:4)
       sp$add_seeds(1)
sp$height <- sort(runif(sp$size), decreasing=TRUE)  # assign some random heights
sp$compute_vars_phys(env)

sp$vars_size  # extract size
sp$vars_phys  # phys

xs <- data.frame(t(sp$vars_size))
xp <- data.frame(t(sp$vars_phys))

plot(x[["height"]], x[["mass_total"]], type='b')

plot(xs[["height"]], xp[["height_growth_rate"]], type='b')


# step with ode stepper
pars.derivs <- list(plant=p, light.env=env)
tt <- seq(0, 15, length=51)
library(deSolve)
derivs.d <- function(...) list(derivs(...))

plant.run <- rk(y, tt, derivs.d, pars.derivs,
                     method=rkMethod("rk45ck"), hini=1/1000, rtol=1,
                     atol=1)
