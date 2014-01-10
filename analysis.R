
source("R/tree-fun.R")
source("R/plots.R")

s <- new(Strategy)
x <- run_plant(h=0.5, E=1,  strategy =s)

h <- seq(0.1, 30, length.out=50)
x <- change_with_size(h=h)
plot(x[["h"]], x$vars_size[["mass_total"]], type='b')
plot(x[["h"]], x$vars_phys[["height_growth_rate"]], type='b')
plot(h, wplcp_with_size(h))

x <- change_with_light(E=seq(0.1, 1, length.out=50))
plot(x[["E"]], x$vars_size[["mass_total"]], type='b')
plot(x[["E"]], x$vars_phys[["height_growth_rate"]], type='b')


lma <- 10^seq(-1.5, 0.5, length.out=20)
x <- change_with_trait(lma, "lma", h=18)
plot(x[["lma"]], x$vars_phys[["height_growth_rate"]], type='b')

x <- wplcp_with_trait(lma, "lma", h=18)
plot(lma, x, type='b')

plot_mass_fraction(h=1:50)

# growth decomp
x <- change_with_size(h=seq(0.1, 30,length.out=50))
par(mfrow = c(1,5))

for(v in c("height_growth_rate","dheight_dleaf_area","leaf_fraction", "growth_fraction","net_production")){
	plot(x[["h"]], x$vars_growth_decomp[[v]], type='l', xlab = "height", ylab=v)
}

# Decomposition of height growth rate

# dhdt = height growth rate  	# 								= height_growth_rate
#    dHdA   					# architecture             		= dheight_dleaf_area
#  * dAdMl  					# leaf area per leaf mass  		= strategy->lma
#  * dMldMt 					# total cost of leaf deployment = leaf_fraction
#  * dMtdMp 					# fraction new mass in growth 	= (1 - reproduction_fraction)
#  * dMpdt  					# total mass production 		= net_production
