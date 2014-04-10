#names of all studies
source("R/tree-fun.R")
strategy = new(Strategy)
plant <- new(Plant, strategy)


y0 <- c(0.25, 0, 0, 0,0)
x <- data.frame(step.ode(y0, fixed.environment(1), plant, timesteps = seq(0, 100, length=51))) %.%
	group_by(time) %.%
	mutate(basal_area=get.size.metric("area_basal", height, heartwood_area, heartwood_mass, plant))
plot(basal_area~time, data=x)


## grow to given size for target var

target.var <- "area_basal"
target.val <- 0.10
h.ini =0.25
timesteps = seq(0, 100, length=51)

y0 <- c(h.ini, 0, 0, 0,0)
x <- step.ode.for.target(target.var, y0, fixed.environment(1), plant, timesteps)

n =nrow(x)
check <- x[[target.var]][2:n] >  target.val &  x[[target.var]][1:(n-1)] <  target.val

if(!any(check)) stop("not in interval")
i <- which(check)

value.at.time <- function(time){
	y0 <- as.numeric(x[i,2:6])
	timesteps <-c(x$time[i], time)
	step.ode.for.target(target.var, y0, fixed.environment(1), plant, timesteps)[[target.var]][2] - target.val
}

value.at.times <- function(times){
	sapply(times,value.at.time)
}

value.at.times(x$time[i]*c(1.001, 1.1))

#uniroot(value.at.time, interval=x$time[i:(i+1)], tol = 1e-6)
