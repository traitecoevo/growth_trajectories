# Fitting TREE to observed data

source_dir <- function(path, ...) {
  for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
    source(file.path(path, nm), ...)
  }
}
source_dir("R")

library(tree2)
suppressPackageStartupMessages({
  library(plyr)
  library(dplyr)
})

# First, look at plot: challenge is to move peak to left and pull down elevation of curve.
data <- remake::make("BCI_species_data") %>%
  mutate(dbh=at) %>%
  filter(!is.na(lma*rho*hmat*dbh*dbasal_diam_dt), dbh %in% c(0.01), hmat > 10) %>%
  select(dbh, lma, rho, dbasal_diam_dt)

d <- trait_effects_data("lma", "diameter")
d <- d[d$size_class == 2L,]

plot(data$lma, data$dbasal_diam_dt, las=1, log="x",
     xlab=name_pretty("lma"), ylab=name_pretty("dbasal_diam_dt"),
     xlim=range(data$lma, d$lma),
     ylim=range(data$dbasal_diam_dt, d$dbasal_diam_dt))
lines(d$lma, d$dbasal_diam_dt, col="red")

# given a dbh x, traits and parameters, grows plant to right size and estimates growth rate
growth <- function(x, trait, dbh, E=1,
                   strategy=default_strategy()) {
  strategy[trait] <- x
  pp <- grow_plant_to_size(Plant(strategy), dbh, "diameter",
                           fixed_environment(E))
  sapply(pp$plant, function(p) p$vars_growth[["dbasal_diam_dt"]])
}

# finds trait value in range that maximises growth rate at given size and light
maximise_growth_rate_by_trait <- function(strategy,
                                          trait="lma",
                                          range=c(0.001, 1),
                                          dbh=0.01, E=1) {
  f <- function(x) {
    growth(x, trait, dbh, E, strategy)
  }
  ret <- optimise(f, range, maximum=TRUE, tol=1e-4)
  list(optimal_trait=ret$maximum,
       growth_rate=ret$objective)
}

# finds trait value in range that maximises growth rate at given size
# and light
maximise_growth_rate_by_trait(default_strategy())

# Sensitivity to pars
exclude <- c("c_acc", "c_bio", "c_d0", "c_d2", "c_d3", "c_p2", "c_r1",
             "c_r2", "c_s0", "hmat", "hmat_0", "lma", "lma_0", "s",
             "s_0", "B5", "B6", "c_d1", "B7", "n_area_0", "rho_0",
             "control")
par_names <- setdiff(names(Strategy()), exclude)

run_variant <- function(par, adj, strategy=default_strategy()) {
  x0 <- strategy[[par]]
  x1 <- x0 * adj
  strategy[[par]] <- x1
  max_lma <- maximise_growth_rate_by_trait(strategy)
  data.frame(x0=x0, x1=x1,
             optimal_trait=max_lma$optimal_trait,
             growth_rate=max_lma$growth_rate)
}

CV <- function(x1, x2) {
  (x2 - x1) / x1
}

strategy <- default_strategy()
base <- run_variant("c_acc", 1.0, strategy)

elasticity <- ldply(par_names, function(x) run_variant(x, 1.01))
rownames(elasticity) <- par_names
elasticity$optimal_trait0 <- base$optimal_trait
elasticity$growth_rate0 <- base$growth_rate
elasticity$scale <- CV(elasticity$x1, elasticity$x0)
elasticity$el_opt <- CV(elasticity$optimal_trait, base$optimal_trait) /
  elasticity$scale
elasticity$el_gro <- CV(elasticity$growth_rate, base$growth_rate) /
  elasticity$scale
