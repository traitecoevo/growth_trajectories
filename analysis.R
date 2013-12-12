
source("R/tree-fun.R")

#
s <- new(Strategy)
run_plant(h=0.5, E=1,  strategy =s)

x <- change_with_size(h=seq(0.1, 30, by=0.1))

plot(x[["h"]], x$vars_size[["mass_total"]], type='b')
plot(x[["h"]], x$vars_phys[["height_growth_rate"]], type='b')

x <- change_with_light(E=seq(0.1, 1, length.out=50))

plot(x[["E"]], x$vars_size[["mass_total"]], type='b')
plot(x[["E"]], x$vars_phys[["height_growth_rate"]], type='b')

#x <- change_with_trait....
