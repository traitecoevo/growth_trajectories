

get.axis.info <- function(name, parToGet){

  # TRAITS
  if(name=="lma"){
    ticks <- 0.01*2^seq(0,20)
    lab <- expression(paste("Leaf-construction cost (kg ",m^-2," )"))
    lim <- c(0.01,1.28)
  }
  if(name=="hmat"){
    ticks <- 2^seq(-4,20)
    lab <- expression(paste("Maximum height (m)"))
    lim <- c(1,40)
  }
  if(name=="rho"){
    ticks <- 0.2*2^seq(-5,10)
    lab <- expression(paste("Wood density (kg ",m^3," )"))
    lim <- c(200,1200)
  }

  # SIZE
  if(name=="height"){
    ticks <- 0.25*2^seq(0,12)
    lab <- expression(paste("Height (m)"))
    lim <- c(0.25,32)
  }
  if(name=="dbh"){
    ticks <- 0.25*2^seq(0,12)
    lab <- expression(paste("Stem diameter (m)"))
    lim <- c(0.25,32)
  }
  if(name=="area_basal"){
    ticks <- 0.25*2^seq(0,12)
    lab <- expression(paste("Stem basal area (m)"))
    lim <- c(0.25,32)
  }

  # GROWTH RATE
  if(name=="height_growth_rate"){
    ticks <- seq(0, 2, by=0.25)
    lab <- expression(paste("Height growth rate (m ",yr^-1," )"))
    lim <- c(0,2)
  }

  if(name=="dbh_gr"){
    ticks <- 0.0125*2^seq(-5,10)
    lab <- expression(paste("Diameter growth rate (m ",yr^-1,")"))
    lim <- c(0.01,0.2)/1000
  }

  if(name=="dbasal_area_dt"){
    ticks <- 0.0125*2^seq(0,20)
    lab <- expression(paste("Basal area growth (", m^2," ", yr^-1,")"))
    lim <- c(0.01,0.2)^2
  }

  if(name=="mass_gr"){
    ticks <- 0.0125*2^seq(0,20)
    lab <- expression(paste("Mass growth (kg ", yr^-1,")"))
    lim <- c(0.01,0.2)^2
  }

  # OTHER
  if(name=="leaf_turnover"){
    ticks <- c(0.06, 0.12, 0.25, 0.5, 1.0, 2,4,8,16, 32)
    lab <- expression(paste("Leaf turnover rate (",yr^-1," )"))
    lim <- c(0.03,32)
  }
  if(name=="shading"){
    ticks <- c(0.25, 0.5, 1.0, 2,4)
    lab <- expression(paste("Maximum leaf area above (",m^2," )"))
    lim <- c(0.2,5)
  }

  return(get(parToGet))
}


new_plot <- function(xvar, yvar,
    xlim=get.axis.info(xvar,"lim"),
    xtick=get.axis.info(xvar,"ticks"),
    xlab=get.axis.info(xvar,"lab"),
    ylim=get.axis.info(yvar,"lim"),
    ytick=get.axis.info(yvar,"ticks"),
    ylab=get.axis.info(yvar,"lab"),
    xtick.lab=xtick,
    ytick.lab=ytick,
  ...){

  blank_plot(xlim=xlim, xtick=xtick, xlab=xlab,
    ylim=ylim, ytick=ytick, ylab=ylab, xtick.lab=xtick.lab,
    ytick.lab=ytick.lab, ...)
}
