figure_lma_tradeoff <- function(data) {
  sites <- data[["dataset"]]
  lma <- data[["lma"]]
  leaf_turnover <- data[["leaf_turnover"]]

  biomes <- sort(unique(data[["biome"]]))
  nbiomes <- length(biomes)

  col_table <- nice_colors(nbiomes)
  names(col_table) <- biomes

  i <- !is.na(lma) & !is.na(leaf_turnover) & table(sites)[sites] > 3
  sm1 <- sma(leaf_turnover[i] ~ lma[i] * sites[i], log="xy")
  col_sm1 <- col_table[data[["biome"]][match(sm1[["groups"]], sites)]]

  strategy <- default_strategy()

  B4 <- 1.71
  lma_0 <- 0.1978791
  k_l0 <- 0.4565855

  # TODO: These values no longer accessible via strategy. See traitecoevo/plant#166
  # B4 <- strategy$B4
  # k_l0 <- strategy$k_l0
  # lma_0 <- strategy$lma_0
  x <- seq_log(10^par("usr")[1], 10^par("usr")[2], length.out=10)
  y <- k_l0 * (x / lma_0)^(-B4)

  par(mar=c(4.6, 4.6, .5, .5))
  plot(NA, type="n", log="xy", xlim=c(0.01, 1.28), ylim=c(0.03, 32),
       xlab="", ylab="", las=1)
  mtext(name_pretty("lma"), 1, line=3)
  mtext(name_pretty("leaf_turnover"), 2, line=3)

  points(lma[i], leaf_turnover[i], pch=16, cex=0.9,
         col=make_transparent("grey", 0.3))
  plot(sm1, add=TRUE, col=col_sm1, type="l", lwd=1, p.lines.transparent=0.15)
  lines(x, y, lwd=2)

  title <- sprintf("%d sites, %d species",
                   length(unique(sites[i])),
                   sum(!is.na(leaf_turnover[i]) & !is.na(lma[i])))
  legend("topright", legend=tolower(names(col_table)), bty="n",
         pch=16, col=col_table, cex=1, title=title)
}

addIsoclines <- function(A=NULL,b=NULL,...){
  if(is.null(A)) {
    A <- seq(-10,10)
  }
  if(is.null(b)) {
    b <- 1
  }
  for(a in A) abline(a,b,lty="dotted",...)
}

add.sma.by.group <-function (data, xvar, yvar, smagroup, colorBygroup, col.table,nmin=10,...){
  if(sum(!is.na(data[[yvar]]*data[[xvar]]))>nmin){
    col=colour.by.category(data[[colorBygroup]][1],col.table)
    fit <- sma(data[[yvar]]~data[[xvar]]*data[[smagroup]], log="xy")
    plot(fit, type='l',col=col,p.lines.transparent=0.1,add=TRUE,...)
    invisible(fit)
  }
}

make.color.table <- function(names){
  col.table <- nice_colors(length(names))
  names(col.table) <- names
  col.table
}

# make a plot, fits lines to each within each group, applyiong same colour within that group
plot_with_sma_by_var_within_group <- function(data, xvar,yvar,group="vegetation",location=NULL, isoclines=NULL, A = NULL,...){
  plot(data[[xvar]], data[[yvar]],  type= 'n', log="xy", las=1, yaxt="n", xaxt="n",...)
  axis.log10(1)
  axis.log10(2)

  if(!is.null(isoclines))
    addIsoclines(b=isoclines, A=A)

  points(data[[xvar]], data[[yvar]],  type= 'p', col = make.transparent("grey", 0.4), pch=16)

  col.table <- make.color.table(unique(data[[group]]))

  fits <- dlply(data, group, add.sma.by.group, xvar=xvar, yvar=yvar, col.table=col.table,
    smagroup="species",colorBygroup=group)

  get.significant.groups <- function(fit, p=0.1){
    if(is.null(fit)) return(FALSE)
    any(fit$groupsummary$pval < p)
  }

  tmp <- laply(fits, get.significant.groups)
  lab <- sort(names(tmp)[tmp])

  if(!is.null(location))
    legend(location, legend = tolower(lab), bty = "n", pch=16, col=colour.by.category(lab,col.table), cex=0.5)
  fits
}

# make a plot, fits lines to each spp
plot_with_sma_by_spp <- function(data, xvar, yvar, location=NULL, isoclines=NULL, A = NULL,
  xlab=NULL, ylab=NULL, ...){
  plot(data[[xvar]], data[[yvar]],  type= 'n', log="xy", las=1, yaxt="n", xaxt="n",
    xlab="", ylab="",...)
  axis.log10(1)
  axis.log10(2)

  if(!is.null(xlab))
    mtext(xlab, 1, line=3)
  if(!is.null(ylab))
    mtext(ylab, 2, line=3)

  if(!is.null(isoclines))
    addIsoclines(b=isoclines, A=A)

  cols <- color_pallete(9)

  points(data[[xvar]], data[[yvar]],  type= 'p', col = cols[3], pch=16)

  fit <- sma(data[[yvar]]~data[[xvar]]*data[["species"]], log="xy")

  plot(fit, type='l', col=cols[8], p.lines.transparent=0.1, add=TRUE,..., lwd=1)

  invisible(fit)
}


figure_assumptions <- function(baad){


  data <- baad[["data"]]

  ## Fill sapwood area. Let basal=breast height where only data at breast height exists.
  # For small plants treat total stem cross section as sapwood
  data[["sap"]] <- data[["a.ssba"]]
  i <- is.na(data[["sap"]])
  data[["sap"]][i] <- data[["a.ssbh"]][i]
  i <- is.na(data[["sap"]]) & !is.na(data[["a.stba"]]) & !is.na(data[["h.t"]]) & data[["h.t"]] < 1
  data[["sap"]][i] <- data[["a.stba"]][i]/(1.2)

  xlim <- c(1E-5, 1E4)

  mylabel <- function(txt) label(txt, -0.25, 1.1, xpd=NA, cex=1.5)
  s <- default_strategy()
  x <- seq_log_range(c(1E-6, 2E6), 100)

  par(mfrow = c(2,2), mar=c(5, 7, 3, 0.5), oma=c(0,0,0,0))
  plot_with_sma_by_spp(data,"a.lf", "h.t",
    ylim=c(1.5E-3, 1.5E2), xlim=xlim,
    ylab="Height (m)", xlab=expression(paste("Leaf area (",m^2,")")), location="topleft",
    isoclines=0.5, A= seq(-10,10))

  lines(x, s$a_l1*x^s$a_l2, lwd=2)
  mylabel("a) Architectural layout")
  legend("bottomright", legend = "Slope = 0.5", bty = "n", cex=1)

  plot_with_sma_by_spp(data,"a.lf", "sap",
    xlim=xlim, ylim=c(1E-9, 1E1),
    ylab=expression(paste("Sapwood area (",m^2,")")), xlab=expression(paste("Leaf area (",m^2,")")),location="topleft",
    isoclines=1, A= seq(-15,10, by=2))
  lines(x, s$theta*x, lwd=2)
  mylabel("b) Pipe model")
  legend("bottomright", legend = "Slope = 1", bty = "n", cex=1)

  plot_with_sma_by_spp(data,"a.lf", "m.rt",
    xlim=xlim, ylim=c(1E-8, 1E2),
    ylab="Mass of fine roots (kg)", xlab=expression(paste("Leaf area (",m^2,")")),location="topleft",
    isoclines=1, A= seq(-10,10, by=2))
  lines(x, s$a_r1*x, lwd=2)
  mylabel("c) Roots")
  legend("bottomright", legend = "Slope = 1", bty = "n", cex=1)

  data <- baad[["data"]]

  # lets let everything less than 1m tall have zero heartwood
  i <- !is.na(data[["h.t"]]) & data[["h.t"]] < 1 & is.na(data[["m.sh"]])
  data$m.sh[i] <- 0

  # Now calculate support costs of live biomass being whole stem - heartwood
  # Records without heartwood recorded will return NA
  data$msal <- (data$m.st - data$m.sh)/data$a.lf

  plot_with_sma_by_spp(data,"h.t", "msal",
    xlim=c(1E-3, 1E2), ylim=c(1E-4, 1E1),
    ylab=expression(paste("Mass sapwood + bark / leaf area (kg ",m^-2,")")), xlab="Height (m)",location="topleft",
    isoclines=1, A= seq(-10,10, by=1))

  # Now plot expected relationship from model
  # Ms = (1+a_b1)*theta*rho*eta_c*Al*H --> ms/al = (1+a_b1)*theta*rho*eta_c*H
  eta_c <- 1 - 2/(1 + s$eta) + 1/(1 + 2*s$eta)
  lines(x, (1+s$a_b1)*s$theta*s$rho*eta_c*x, lwd=2)
  legend("bottomright", legend = "Slope = 1", bty = "n", cex=1)
  mylabel("d) Live stem mass per leaf area")

}

