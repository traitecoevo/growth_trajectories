

get.axis.info <- function(dim, parToGet){

  ticks <- 0.01*2^seq(0,12)
  lab <- expression(paste("Leaf mass per area(kg ",m^-2," )"))
  lim <- c(0.02,1.28)

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
    ticks <- c(0.12, 0.25, 0.5, 1.0, 2,4,8,16)
    lab <- expression(paste("Leaf turnover rate (",yr^-1," )"))
    lim <- c(0.12,16)
  }

  if(dim==4){
    ticks <- seq(0, 0.5, by=0.1)
    lab <- expression(paste("Light compensation point (0-1)"))
    lim <- c(0,0.5)
  }

  if(dim==5){
    ticks <- 0.0125*2^seq(0,20)
    lab <- expression(paste("Relative growth rate (",yr^-1,")"))
    lim <- c(0.01,0.2)
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
