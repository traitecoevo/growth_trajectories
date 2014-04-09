#names of all studies

source("R/tree-fun.R")
strategy = new(Strategy)

y0 <- c(0.25, 0, 0, 0,0)
x <- step_ode(y0, fixed.environment(1), new(Plant, strategy))

plot(x)
