name_pretty <- function(trait) {
  switch(trait,
         lma=expression(paste("Leaf-mass per area (kg ", m^-2, " )")),
         rho=expression(paste("Wood density (kg ", m^-3, " )")),
         narea=expression(paste("Nitrogen per area (kg ", m^-2, " )")),
         hmat=expression(paste("Height at maturation (m)")),
         diameter_stem_dt=expression(paste("Diameter growth (m ", yr^-1, ")")),
         shading=expression(paste("WPLCP, ",E^"*"," (0-1)")),
         leaf_turnover=expression(paste("Leaf turnover rate (", yr^-1, " )")),

         height_dt=expression(paste("Height growth rate (m ", yr^-1, " )")),
         diameter_stem_dt=expression(paste("Diameter growth (m ", yr^-1, ")")),
         area_stem_dt=expression(paste("Basal area growth (", m^2, " ", yr^-1, ")")),
         mass_above_ground_dt=expression(paste("Mass growth (kg ", yr^-1, ")")),

         height_dt_relative=expression(paste("Relative height growth (m ", yr^-1, " )")),
         diameter_stem_dt_relative=expression(paste("(Relative diameter growth (m ", yr^-1, ")")),
         area_stem_dt_relative=expression(paste("(Relative basal area growth (", m^2, " ", yr^-1, ")")),
         mass_above_ground_dt_relative=expression(paste("(Relative mass growth (kg ", yr^-1, ")")),

         stop("Unknown trait ", trait))
}

trait_range <- function(trait) {
  switch(trait,
         lma=c(5E-3, 3),
         rho=c(90, 3000),
         narea =c(1E-4, 5E-2),
         hmat=c(1, 100),
         stop("No standard range for trait ", trait))
}

trait_range_obs <- function(trait) {
  switch(trait,
         lma=c(0.05, 0.3),
         rho=c(300,  970),
         narea =c(5E-4, 1E-2),
         hmat=c(4, 30),
         stop("No standard range for trait ", trait))
}

get_by <- function(ymax) {
    by <- diff(pretty(c(0, ymax)))[1]
    if(ymax > 0.6)
        by <- 0.2
    if(ymax > 1.25)
        by <- 0.5
    if(ymax > 2.5)
        by <- 1
    if(ymax > 5)
        by <- 2
    if(ymax > 10)
        by <- 5
    by
}