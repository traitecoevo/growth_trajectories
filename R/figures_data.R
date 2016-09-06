load_baad_data <- function(){
  data <- baad.data::baad_data("1.0.1")[["data"]]

  ## Fill sapwood area. Let basal=breast height where only data at breast height exists.
  data[["sap"]] <- data[["a.ssba"]]
  i <- is.na(data[["sap"]])
  data[["sap"]][i] <- data[["a.ssbh"]][i]
  # For small plants treat total stem cross section as sapwood
  i <- is.na(data[["sap"]]) &
       !is.na(data[["a.stba"]]) &
       !is.na(data[["h.t"]]) &
       data[["h.t"]] < 1
  data[["sap"]][i] <- data[["a.stba"]][i]/(1.2)

  # lets let everything less than 1m tall have zero heartwood
  i <-  is.na(data[["m.sh"]]) &
        !is.na(data[["h.t"]]) &
        data[["h.t"]] < 1
  data[["m.sh"]][i] <- 0

  # Now calculate support costs of live biomass being whole stem - heartwood
  # Records without heartwood recorded will return NA
  data[["msal"]] <- (data[["m.st"]] - data[["m.sh"]])/data[["a.lf"]]

  data
}

fit_SMA_baad <- function(xvar, yvar) {
  baad <- load_baad_data()
  data <- data_for_stan(baad, I("species"), xvar, yvar)
  fit_group_stan(data)
}

fit_SMA_lma <- function(data) {
  data <- data_for_stan(data, "dataset", "lma", "leaf_turnover")
  fit_group_stan(data, positive=FALSE)
}

figure_lma_tradeoff <- function(data, fit_lma_tradeoff) {

  data <- data[!is.na(data[["lma"]]*data[["leaf_turnover"]]), ]
  data[["sites"]] <- as.character(as.integer(
            as.factor(data[["dataset"]])))

  par(mar=c(4.6, 4.6, .5, .5))
  plot(NA, type="n", log="xy", xlim=c(0.01, 1.28), ylim=c(0.03, 32),
       xlab="", ylab="", las=1)
  mtext(name_pretty("lma"), 1, line=3)
  mtext(name_pretty("leaf_turnover"), 2, line=3)

  points(data[["lma"]], data[["leaf_turnover"]], pch=16, cex=0.9,
         col=make_transparent("grey", 0.4))

  # add lines
  dd <- fit_by_group_stan_summary(fit_lma_tradeoff)
  dd[["biome"]] <- data[["biome"]][match(dd$group, data[["sites"]])]

  biomes <- sort(unique(dd[["biome"]]))
  col_table <- nice_colors(length(biomes))
  names(col_table) <- biomes

  for(i in seq_len(nrow(dd)))
    curve(exp(dd$b_0[i])*x^dd$b_1[i], dd$from[i], dd$to[i], add=TRUE,
      col=col_table[dd$biome[i]])

  # legend
  title <- sprintf("%d sites, %d species",
                   length(unique(data[["sites"]])), nrow(data))
  legend("topright", legend=tolower(names(col_table)), bty="n",
         pch=16, col=col_table, cex=1, title=title)

  # TODO: These values no longer accessible via plant strategy object.
  # See traitecoevo/plant#166
  B4 <- 1.71
  lma_0 <- 0.1978791
  k_l0 <- 0.4565855
  x <- seq_log(10^par("usr")[1], 10^par("usr")[2], length.out=10)
  y <- k_l0 * (x / lma_0)^(-B4)
  lines(x, y, lwd=2)
}


figure_assumptions <- function(fit_a.lf_h.t,  fit_a.lf_sap, fit_a.lf_m.rf, fit_a.lf_msal){

  xlim <- c(1E-5, 1E4)

  mylabel <- function(txt) label(txt, -0.25, 1.1, xpd=NA, cex=1.5)
  myaxislabel <- function(txt, side) mtext(txt, side, line =3.5)
  mylegend <- function(txt) legend("bottomright", legend = txt, bty = "n", cex=1)

  cols <- color_pallete()
  s <- default_strategy()
  x <- seq_log_range(c(1E-6, 2E6), 100)

  par(mfrow = c(2,2), mar=c(5, 7, 3, 0.5), oma=c(0,0,0,0))

  figure_fit_by_group_stan(fit_a.lf_h.t, isoclines=0.5, col.l = cols[8])
  myaxislabel("Height (m)",2)
  myaxislabel(expression(paste("Leaf area (",m^2,")")),1)
  mylegend("Slope = 0.5")
  mylabel("a) Architectural layout")
  # Now plot expected relationship from model
  lines(x, s$a_l1*x^s$a_l2, lwd=2)

  figure_fit_by_group_stan(fit_a.lf_sap, isoclines=1,
    A= seq(-15,10, by=2), col.l = cols[8])
  myaxislabel(expression(paste("Sapwood area (",m^2,")")),2)
  myaxislabel(expression(paste("Leaf area (",m^2,")")),1)
  mylegend("Slope = 1")
  mylabel("b) Pipe model")
  # Now plot expected relationship from model
  lines(x, s$theta*x, lwd=2)

  figure_fit_by_group_stan(fit_a.lf_m.rf, isoclines=1,
    A=seq(-10,10,by=2), col.l = cols[8])
  myaxislabel("Mass of fine roots (kg)",2)
  myaxislabel(expression(paste("Leaf area (",m^2,")")),1)
  mylegend("Slope = 1")
  mylabel("c) Roots")
  # Now plot expected relationship from model
  lines(x, s$a_r1*x, lwd=2)

  figure_fit_by_group_stan(fit_a.lf_msal, isoclines=1,
    A=seq(-10,10,by=2), col.l = cols[8])
  myaxislabel(expression(paste("Mass sapwood + bark / leaf area (kg ",m^-2,")")),2)
  myaxislabel("Height (m)", 1)
  mylegend("Slope = 1")
  mylabel("d) Live stem mass per leaf area")
  # Now plot expected relationship from model
  # Ms = (1+a_b1)*theta*rho*eta_c*Al*H --> ms/al = (1+a_b1)*theta*rho*eta_c*H
  eta_c <- 1 - 2/(1 + s$eta) + 1/(1 + 2*s$eta)
  lines(x, (1+s$a_b1)*s$theta*s$rho*eta_c*x, lwd=2)
}

