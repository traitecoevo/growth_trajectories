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

addIsoclines <- function(A= seq(-10,10),b=1,...){
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
plot_with_sma_by_var_within_group <- function(data, xvar,yvar,group="vegetation",location=NULL, isoclines=NULL,...){

  plot(data[[xvar]], data[[yvar]],  type= 'n', log="xy", las=1, yaxt="n", xaxt="n",...)
  axis.log10(1)
  axis.log10(2)

  if(!is.null(isoclines))
    addIsoclines(b=isoclines)

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

figure_assumptions <- function(baad){

  data <- baad[["data"]]

  ## Fill sapwood area. Let basal=breast height where only data at breast height exists.
  # For small plants treat total stem cross section as sapwood
  data[["sap"]] <- data[["a.ssba"]]
  i <- is.na(data[["sap"]])
  data[["sap"]][i] <- data[["a.ssbh"]][i]
  i <- is.na(data[["sap"]]) & !is.na(data[["a.stba"]]) & !is.na(data[["h.t"]]) & data[["h.t"]] < 2
  data[["sap"]][i] <- data[["a.stba"]][i]

  par(mfrow = c(1,3))
  fits<-plot_with_sma_by_var_within_group(data,"a.lf", "h.t", "vegetation",ylim=c(0.005, 120), xlim=c(1E-5, 1E3), ylab="Height (m)", xlab="",location="topleft", isoclines=0.5, main="a) Architecture")
  legend("bottomright", legend = "Slope = 0.5", bty = "n", cex=1)

  plot_with_sma_by_var_within_group(data,"a.lf", "sap", "vegetation",xlim=c(1E-5, 1E3), ylim=c(1E-8, 1E0), ylab=expression(paste("Sapwood area (",m^2,")")), xlab=expression(paste("Leaf area (",m^2,")")),location="topleft", isoclines=1, "b) Pipe Model")
  legend("bottomright", legend = "Slope = 1", bty = "n", cex=1)

  plot_with_sma_by_var_within_group(data,"a.lf", "m.rt", "vegetation",xlim=c(1E-5, 1E3), ylim=c(1E-8, 400), ylab="mass root (kg)", xlab="",location="topleft", isoclines=1, main="c) Roots")
  legend("bottomright", legend = "Slope = 1", bty = "n", cex=1)

}

axis.log10 <- function(side=1, horiz=FALSE, labels=TRUE, baseAxis = TRUE, wholenumbers=T, labelEnds=T,las=1, at=NULL) {

  fg <- par("fg")

  if(is.null(at)){

    #get range on axis
    if(side ==1 | side ==3) {
      r <- par("usr")[1:2]   #upper and lower limits of x-axis
    } else {
      r <- par("usr")[3:4] #upper and lower limits of y-axis
    }

    #make pertty intervals
    at <- pretty(r)
    #drop ends if desirbale
    if(!labelEnds)
      at <- at[at > r[1] & at < r[2]]
  }
  #restrict to whole numbers if desriable
  if(wholenumbers)
    at<-at[is.wholenumber(at)]

  lab <- do.call(expression, lapply(at, function(i) bquote(10^.(i))))

  #convert at if
  if(baseAxis)
    at<-10^at

  #make labels
  if ( labels )
    axis(side, at=at, lab, col=if(horiz) fg else NA,
         col.ticks=fg, las=las)
  else
    axis(side, at=at, FALSE, col=if(horiz) fg else NA,
         col.ticks=fg, las=las)
}

is.wholenumber <-  function(x, tol = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tol
}

make.transparent <- function(col, opacity=0.5) {
  tmp <- col2rgb(col)/255
  rgb(tmp[1,], tmp[2,], tmp[3,], alpha=opacity)
}

colour.by.category <- function(x, table) {
 unname(table[x])
}
