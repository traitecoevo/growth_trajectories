#names of all studies
source("R/tree-fun.R")
strategy = new(Strategy)
plant <- new(Plant, strategy)

# function to give distance from target
height.10 <- function(plant) {
	plant$height - 10
}

plant <- grow.plant.to.size(plant, fixed.environment(1), distance.from.target=height.10)
