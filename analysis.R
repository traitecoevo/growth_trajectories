
source("R/tree-fun.R")
source("R/plots.R")

s <- new(Strategy)
x <- run_plant(h=0.5, E=1,  strategy =s)

h <- seq(0.1, 30, length.out=50)
x <- change_with_size(h=h)
plot(h, x$vars_size[["mass_total"]], type='b')
plot(h, x$vars_phys[["height_growth_rate"]], type='b')
plot(h, wplcp_with_size(h))

E <- seq(0.1, 1, length.out=50)
x <- change_with_light(E=E)
plot(E, x$vars_size[["mass_total"]], type='b')
plot(E, x$vars_phys[["height_growth_rate"]], type='b')

E <- seq(0.1, 1, length.out=50)
x <- trait_maximimum_with_light(E, "lma", c(1E-5, 10), h=0.1)
plot(E, x, log="y")

h <- seq(0.1, 30, length.out=50)
x <- trait_maximimum_with_size(h, "lma", c(1E-5, 20), E=1)
plot(h, x, log="y")


lma <- 10^seq(-1.5, 0.5, length.out=20)
x <- change_with_trait(lma, "lma", h=18)
plot(lma, x$vars_phys[["height_growth_rate"]], type='b')

x <- wplcp_with_trait(lma, "lma", h=18)
plot(lma, x, type='b')

plot_mass_fraction(h=1:50)

# growth decomp
h <- seq(0.1, 30,length.out=50)
x <- change_with_size(h=h)
par(mfrow = c(1,5))

for(v in c("height_growth_rate","dheight_dleaf_area","leaf_fraction", "growth_fraction","net_production")){
	plot(h, x$vars_growth_decomp[[v]], type='l', xlab = "height", ylab=v)
}

# Decomposition of height growth rate

# dhdt = height growth rate  	# 								= height_growth_rate
#    dHdA   					# architecture             		= dheight_dleaf_area
#  * dAdMl  					# leaf area per leaf mass  		= strategy->lma
#  * dMldMt 					# total cost of leaf deployment = leaf_fraction
#  * dMtdMp 					# fraction new mass in growth 	= (1 - reproduction_fraction)
#  * dMpdt  					# total mass production 		= net_production
