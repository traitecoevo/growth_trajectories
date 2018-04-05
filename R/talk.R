

library(gridBase)
library(grImport)

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
