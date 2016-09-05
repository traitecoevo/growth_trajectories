name_pretty <- function(trait) {
  switch(trait,
         lma=expression(paste("Leaf-mass per unit leaf area (kg ", m^-2, " )")),
         rho=expression(paste("Wood density (kg ", m^-3, " )")),
         narea=expression(paste("Nitrogen per leaf area (kg ", m^-2, " )")),
         hmat="Height at maturation (m)",
         diameter_stem_dt=expression(paste("Diameter growth (m ", yr^-1, ")")),
         shading=expression(paste("Maximum leaf area above (", m^2, " )")),
         leaf_turnover=expression(paste("Leaf turnover rate (", yr^-1, " )")),

         height_dt=expression(paste("Height growth (m ", yr^-1, " )")),
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
         lma=c(0.02, 1.0),
         rho=c(200,  1200),
         narea =c(5E-4, 2E-2),
         hmat=c(3, 60),
         stop("No standard range for trait ", trait))
}
