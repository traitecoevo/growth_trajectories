get.pic <- function(file) {
  base <- tools::file_path_sans_ext(file)
  file.xml <- sprintf("%s.xml", base)
  on.exit({
    file.remove(file.xml)
    file.remove(sprintf("capture%s", basename(file)))
  })
  PostScriptTrace(file, file.xml)
  readPicture(file.xml)
}

# establishes a new viewport to plot onto at RHS of plotting window
# bottom, right, top in inches
setup_viewport <- function(bottom, right, top){
  #gridBase  - works out coordinates of different plots
  vps <- baseViewports()
  pushViewport(vps$inner)  #put current inner onto global viewport stack
  #make new viewport
  vp <- viewport(x=unit(1, "npc") - unit(right, "inches"),
                 y=unit(bottom, "inches"),
                 width=unit(right, "inches"),
                 height=unit(1, "npc") - unit(bottom, "inches") - unit(top, "inches"),
                 just=c("left", "bottom"))
  #push onto stack
  pushViewport(vp)
}

figure_dY_dt_talk <- function(v, i, dat, tree) {

  vars <- dat[["vars"]]
  E <- dat[["E"]]
  cols <- "forestgreen"
  H <- dat[["sizes"]][[i]]
  Hmax <- max(dat[["sizes"]])
  n <- length(E)

  extract_f <- function(v, i, dat) {
      dat_v <- unname(split(dat$data[[v]], dat$data[[v]]$class))
      long_to_wide(dat_v[[i]], "canopy_openness",
                           c(v, vars[2]))
  }

  dsub <- extract_f(v, i, dat)

  ymax <-  3
  par(mai=c(1.2, 1.0, 0.6, 2))
  plot(NA, type="n", log="x",
          xaxt="n", yaxt="n", xlab="", ylab="",
          ylim = c(0, ymax*1.05),
          xlim = range(dsub[[1]]))

  usr <- par("usr")
  rect(1E-5, -1, 1E4, 2*ymax, col = make_transparent("grey", 0.3), border=NA)
  obs <- trait_range_obs(v)
  rect(obs[1], -1, obs[2], 2*ymax, col = "white", border=NA)

  if(i==1){
    text(exp(mean(log(obs[1:2]))), ymax, "<- Real range ->")
  }

  box()
  lines(dsub[[1]][,n], dsub[[2]][,n], col=cols, lwd=2)

  # axes
  axis.log10(1, las=1)
  axis(2, las=1, at = seq(0, 20, by=get_by(ymax)))
  mtext(name_pretty(v), 1, cex=1.2, line=3)
  mtext(name_pretty(dat[["vars"]][2]), line=3, side=2, cex=1.2)

  setup_viewport(1.2, 2, 0.6)
  grid.text(dat[["label"]](H), x=unit(.5, "npc"), y=unit(1.0, "npc"), just=c("center", "bottom"))
  grid.picture(tree, x=unit(.5, "npc"), y=unit(0, "npc"), just=c("center", "bottom"), height=unit(0.2+0.8*H/Hmax, "npc"))
 }

figure_dY_dt_talk2 <- function(dat) {

  vars <- dat[["vars"]]
  sizes <- dat[["sizes"]][1]
  E <- dat[["E"]]
  traits <- dat[["traits"]][2:4]

  cols <- color_pallete1(length(E))
  par(mfrow=c(1, length(traits)),
      oma=c(4, 5, 0, 1.5), mar=c(1, 1, 1, 0.9))

  extract_f <- function(v, i, dat) {
      dat_v <- unname(split(dat$data[[v]], dat$data[[v]]$class))
      dsub <- long_to_wide(dat_v[[i]], "canopy_openness",
                           c(v, vars[2]))
  }

  for (i in seq_along(sizes)) {

    dat2 <- lapply(traits, extract_f, i, dat)
    names(dat2) <- traits
    ymax <- max(sapply(dat2, function(x) max(x[[2]],  na.rm=TRUE)))
    for (v in traits) {
      dsub <- dat2[[v]]
      matplot(dsub[[1]], dsub[[2]], type="n", log="x",
              xaxt="n", yaxt="n", xlab="", ylab="",
              ylim = c(0, ymax*1.05))
      usr <- par("usr")
      obs <- trait_range_obs(v)
      rect(obs[1], -1, obs[2], 2*ymax, col = "white", border=NA)
      box()
      matplot(dsub[[1]], dsub[[2]], type="l", col=cols, lty=1, add=TRUE)

      if(v== traits[1])
        mtext(name_pretty(vars[2]), line=3, side=2, cex=0.9 , outer=FALSE)
      # axes
      axis.log10(1, labels = (i == 1), las=1)
      axis(2, labels = (v == traits[1] ), las=1, at = seq(0, 20, by=get_by(ymax)))
      if (v == last(traits)) {
        mtext(dat[["label"]](sizes[[i]]), 4, cex=0.9, line=1)
      }
      if (i == 1) {
        mtext(name_pretty(v), 1, cex=0.9, line=3)
      }

    }
  }

 legend("topright", legend = E, lty=1, col=rev(cols), bty="n", cex=0.9)
}
