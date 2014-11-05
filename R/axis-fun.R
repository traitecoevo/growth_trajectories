

get.axis.info <- function(dim, parToGet){

  ticks <- 0.01*2^seq(0,20)
  lab <- expression(paste("Leaf-construction cost (kg ",m^-2," )"))
  lim <- c(0.01,1.28)

  if(dim==1){
    ticks <- 0.25*2^seq(0,12)
    lab <- expression(paste("Height (m)"))
    lim <- c(0.25,32)
  }

  if(dim==2){
    ticks <- seq(0, 2, by=0.25)
    lab <- expression(paste("Height growth rate (m ",yr^-1," )"))
    lim <- c(0,2)
  }

  if(dim==3){
    ticks <- c(0.06, 0.12, 0.25, 0.5, 1.0, 2,4,8,16, 32)
    lab <- expression(paste("Leaf turnover rate (",yr^-1," )"))
    lim <- c(0.03,32)
  }

  if(dim==4){
    ticks <- c(0.25, 0.5, 1.0, 2,4)
    lab <- expression(paste("Maximum leaf area above (",m^2," )"))
    lim <- c(0.2,5)
  }

  if(dim==5){
    ticks <- 0.0125*2^seq(0,20)
    lab <- expression(paste("Growth rate (mm ",yr^-1,")"))
    lim <- c(0.01,0.2)
  }

  if(dim==6){
    ticks <- 2^seq(-4,20)
    lab <- expression(paste("Maximum height (m)"))
    lim <- c(1,40)
  }

  if(dim==7){
    ticks <- 0.2*2^seq(-5,10)
    lab <- expression(paste("Wood density (kg ",m^3," )"))
    lim <- c(0.2,1.2)
  }

  return(get(parToGet))
}


new_plot <- function(dim.x, dim.y,
    xlim=get.axis.info(dim.x,"lim"),
    xtick=get.axis.info(dim.x,"ticks"),
    xlab=get.axis.info(dim.x,"lab"),
    ylim=get.axis.info(dim.y,"lim"),
    ytick=get.axis.info(dim.y,"ticks"),
    ylab=get.axis.info(dim.y,"lab"),
    xtick.lab=xtick,
    ytick.lab=ytick,
  ...){

  blank_plot(xlim=xlim, xtick=xtick, xlab=xlab,
    ylim=ylim, ytick=ytick, ylab=ylab, xtick.lab=xtick.lab,
    ytick.lab=ytick.lab, ...)
}
