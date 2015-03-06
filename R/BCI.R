#List of functions used to manipulate and summarise data from BCI.

#Download BCI data
# download function from package downloader provides wrapper
# to download file so that works for https and across platforms
BCI_download_50ha_plot_full <- function(dest) {
  url <-"https://repository.si.edu/bitstream/handle/10088/20925/bci.full.Rdata31Aug2012.zip"
  download(url, dest, mode="wb")
}

BCI_download_species_table <- function(dest) {
  url <-"https://repository.si.edu/bitstream/handle/10088/20925/bci.spptable.rdata"
  download(url, dest, mode="wb")
}

#Load 50ha census data
BCI_load_50ha_plot <- function(path_to_zip) {

  tmp <- tempfile()
  unzip(path_to_zip, exdir=tmp)
  on.exit(unlink(tmp, recursive=TRUE))

  files <- list.files(tmp, pattern=".rdata", full.names=TRUE)
  tbl_df(ldply(list.files(tmp, pattern=".rdata", full.names=TRUE), function(x) load_rdata(x)))
}

BCI_calculate_individual_growth <- function(BCI_50haplot, BCI_nomenclature) {

  # Identifies individuals that return from the dead or are supposably refound
  # i.e. Individuals given dbh=NA and then later given numeric value
  # Note this function must be used prior to subsetting only observations with pom=1.3
  is_zombie <- function(dbh) {
    any(diff(is.na(dbh)) == -1)
  }

  names(BCI_50haplot) <- tolower(names(BCI_50haplot)) # lower case for all column names

  data <- BCI_50haplot %>%
    arrange(sp, treeid, date) %>%
    select(sp, treeid, nostems, censusid,exactdate, dfstatus, pom, dbh) %>%
    mutate(
      #census id for period 7 was entered incorrectly
      censusid = ifelse(censusid==171, 7,censusid),
      species = lookup_latin(sp, BCI_nomenclature),
      family = lookup_family(sp, BCI_nomenclature),
      dbh=dbh/1000) %>%
    # Remove stems from earlier census, measured with course resolution
    filter(censusid >= 3) %>%
    # Remove families that don't exhibit dbh growth e.g. palms
    filter(!family %in% c('Arecaceae', 'Cyatheaceae', 'Dicksoniaceae', 'Metaxyaceae',
                          'Cibotiaceae', 'Loxomataceae', 'Culcitaceae', 'Plagiogyriaceae',
                          'Thyrsopteridaceae')) %>%
    # Remove observations without a species code
    filter(!is.na(sp)) %>%
    # For each individual..
    group_by(treeid) %>%
    # Filter plants with multiple stems
    filter(max(nostems)==1) %>%
    # Remove zombies - individuals that are recorded as dead but reappear at later date
    filter(!is_zombie(dbh)) %>%
    # Remove plants where height of measurement is not close to 1.3
    filter(pom ==1.3) %>%
    # Only keep alive stems
    filter(dfstatus=="alive") %>%
    mutate(
      # First measurement in 1990 ='1990-02-06'
      julian = as.vector(julian(as.Date(exactdate,"%Y-%m-%d"), as.Date("1990-02-06", "%Y-%m-%d"))),
      dbh_increment = c(diff(dbh), NA),
      dbasal_diam_dt = calculate_growth_rate(dbh, julian),
      dbasal_diam_dt_relative = calculate_growth_rate(dbh, julian, log)
    ) %>%
    filter(
      !is.na(dbasal_diam_dt) &
        !is.na(dbh_increment) &
        CTFS_sanity_check(dbh, dbh_increment, dbasal_diam_dt)
    ) %>%
    select(censusid,species, family, dfstatus, dbh, dbasal_diam_dt)
}

BCI_calculate_species_traits <- function(individual_growth, wright_2010) {

  # bands in which to calculate growth rate In all but first band we estimate growth at centre of band.  The first interval is repeated so that we can estimate
  # growth at LHS also
  size_min <- c(10, 10, 25, 50, 100, 200)/1000
  size_range <- data.frame(at = 2 * size_min, min = size_min, max = 4 * size_min)
  size_range[1, "at"] <- 10 / 1000

  get_species_data <- function(data, min, max, at) {
    group_by(data, species) %>%
      filter(dbh >= min & dbh < max) %>%
      summarise(count = n(),
        dbasal_diam_dt = predict_via_quantile_regression(dbh, dbasal_diam_dt, predict_at = at))
  }

  out <- ddply(size_range, 1, function(x) get_species_data(individual_growth, x[["min"]], x[["max"]], x[["at"]]))

  # merge with data from Wright 2010
  wright_2010 <- mutate(wright_2010, species = paste(genus, species))

  species_data <- merge(select(wright_2010, species, lma, rho, hmat), out, by = "species") %>%
    filter(!is.na(dbasal_diam_dt)) %>%
    arrange(species, at)

  names(species_data) <- tolower(names(species_data))

  species_data
}

# function to calculate growth rate of tree, either absolute (default) or relative rgowth rate (set f=log)
calculate_growth_rate <- function(x, t, f = function(y) y) {
  dt <- diff(t)/365.25
  if (any(dt < 0, na.rm = TRUE)) {
    stop("time must be sorted")
  }
  c(diff(f(x))/dt, NA)
}

# Function to identify bad data. Adapted from function in CTFS R package
CTFS_sanity_check <- function(dbh, dbh_increment, dbasal_diam_dt) {

  slope <- 0.006214
  intercept <- 0.9036 /1000   #convert from mm to m
  error_limit <- 4
  max_growth <- 75 / 1000     #convert from mm to m

  accept <- rep(TRUE, length(dbh))
  # Remove records based on max growth rate
  accept[dbasal_diam_dt > max_growth] <- FALSE
  # Remove records based on min growth rate, estimated from allowbale error
  allowable.decrease <- -error_limit * (slope * dbh + intercept)
  accept[dbh_increment < allowable.decrease] <- FALSE
  accept
}

#Look up family
lookup_family <- function(tag, nomen){
  i <- match(tag, tolower(nomen[['sp6']]))
  nomen$family[i]
}

#Look up species code
lookup_latin <- function(tag, nomen){
  nomen$latin <- paste(nomen$genus, nomen$species)
  i <- match(tag, tolower(nomen[['sp6']]))
  nomen[['latin']][i]
}

# fits quantile regression to data
fit_quantile_regression <- function(y, x, tau) {
  rq(y ~ log10(x), tau = tau)
}

# Estimates value for y at given x (predict_at) by fitting quantile regression.  The cutoff value (p) for the gression is adjusted according to the number
# of points in dataset. If there are few points, use default value for p. Otherwise cutrade-off based on top 20 points Returns predicted value of y at given
# value of x (predict_at)
predict_via_quantile_regression <- function(x, y, p = 0.99, nmin = 50, predict_at = 10) {
  n <- length(y)
  if (n < nmin)
    return(as.numeric(NA))
  f <- fit_quantile_regression(y, x, p)
  predict(f, newdata = data.frame(x = predict_at))
}

figure_BCI_data <- function(data) {
  traits <- c("lma", "rho", "hmat")
  at <- c(10, 20, 50, 100, 200)/1000

  op <- par(oma=c(3, 6, 3, 1),
            mar=c(1, 1, 2, 1),
            mfcol=c(length(at), length(traits)))

  data <- data[data$count > 500,]
  dsplit <- lapply(at, function(a) data[data$at == a,])

  ylim <- c(0.5, 32) / 1000
  cex_lab <- 1

  xlim <- list(lma=c(0.02, 0.18), rho=c(250, 1000), hmat=c(5,50))

  for (trait in traits) {
    for (i in seq_along(at)) {
      dsub <- dsplit[[i]]
      cex <- linear_rescale(log10(dsub$count), c(0.6, 2.5), log10(c(800, 10000)))
      x <- dsub[[trait]]
      y <- dsub$dbasal_diam_dt

      sm <- sma(y ~ x, log="xy", method="OLS")

      plot(x, y, pch=1, xlim=xlim[[trait]], ylim=ylim, log="xy", cex=cex, xaxt="n", yaxt="n")
      plot(sm, add=TRUE, col="darkgreen", lwd=2, type="l", p.lines.transparent=0.1)

      lab <- sprintf("r2 = %2.2f, n=%d",
                     sm[["groupsummary"]][["r2"]],
                     sum(!is.na(y) & !is.na(x)))
      legend("topright", legend=lab, bty="n", cex=1.25, text.col="grey")

      axis(1, labels=i == length(at))
      axis(2, labels=trait == traits[[1]], las=1)
      if (i == length(at)) {
        mtext(name_pretty(trait), 1, line=3, xpd=NA, cex=cex_lab)
      }
      if (trait == last(traits)) {
        mtext(sprintf("D=%sm", dsub$at[1]), 4, cex=cex_lab, line=1)
      }
    }
  }
  mtext(name_pretty("dbasal_diam_dt"), 2, line=3, outer=TRUE, cex=cex_lab)
}

figure_BCI_data2 <- function(data) {
  traits <- c("lma", "rho", "hmat")
  at <- c(10, 20, 50, 100, 200)/1000

  data <- data[data$count > 500 & data$at %in% at,]
  data$class <- match(data$at, at)

  ## Three types if fits: independent (_i), weighted (_w) and joint
  ## (_j).
  prep <- function(trait) {
    data$x <- data[[trait]]
    dsplit <- split(data, data$class)
    fits_i <- lapply(dsplit, function(d)
                     lm(log10(dbasal_diam_dt) ~ log10(x), d))

    ## Next thing to try is weights.
    fits_w <- lapply(dsplit, function(d)
                     lm(log10(dbasal_diam_dt) ~ log10(x), d,
                        weights=log(count)))

    fits_j <- lm(log10(dbasal_diam_dt) ~ log10(x) * log(at), data,
                 weights=log(count))

    ## Generate a data set to do prediction with:
    xr <- sapply(dsplit, function(x) range(x[[trait]], na.rm=TRUE))

    yr_i <- sapply(seq_along(dsplit), function(i)
                   10^(predict(fits_i[[i]], list(x=xr[,i]))))
    yr_w <- sapply(seq_along(dsplit), function(i)
                   10^(predict(fits_w[[i]], list(x=xr[,i]))))
    yr_j <- sapply(seq_along(dsplit), function(i)
                   10^(predict(fits_j, data.frame(x=xr[,i], at=at[i]))))

    list(data=data, xr=xr, yr_i=yr_i, yr_w=yr_w, yr_j=yr_j)
  }

  dat <- setNames(lapply(traits, prep), traits)

  ylim <- c(0.5, 32) / 1000
  xlim <- list(lma=c(0.02, 0.18), rho=c(250, 1000), hmat=c(5,50))
  cols <- RColorBrewer::brewer.pal(length(at), "Set1")
  cex <- linear_rescale(log10(data$count), c(0.6, 2.5), log10(c(800, 10000)))
  cex_lab <- 0.75

  par(mfrow=c(1, 3), oma=c(0, 4.1, 0, 0), mar=c(4.1, 2.1, .5, .5))
  for (trait in traits) {
    dt <- dat[[trait]]
    plot(dbasal_diam_dt ~ x, dt$data, pch=21, xlim=xlim[[trait]], #ylim=ylim,
         log="xy", cex=cex, yaxt="n", xlab="", ylab="",
         col=make_transparent(cols[class], .6),
         bg=make_transparent(cols[class], .5))

    # segments(dt$xr[1,], dt$yr_i[1,], dt$xr[2,], dt$yr_i[2,],
    #          col=cols, lwd=3, lty=2)
    ## segments(dt$xr[1,], dt$yr_w[1,], dt$xr[2,], dt$yr_w[2,],
    ##          col=cols, lwd=3, lty=3)
    segments(dt$xr[1,], dt$yr_j[1,], dt$xr[2,], dt$yr_j[2,],
             col=cols, lwd=3)
    axis(2, las=1, labels=trait == traits[[1]])
    if (trait == traits[[1]]) {
      mtext(name_pretty("dbasal_diam_dt"), 2, xpd=NA, line=4, cex=cex_lab)
    }
    mtext(name_pretty(trait), 1, line=3, cex=cex_lab)
  }
}

figure_qunatile_examples <- function(BCI_individual_growth) {

 # bands in which to calculate growth rate In all but first band we estimate growth at centre of band.  The first interval is repeated so that we can estimate
 # growth at LHS also
 size_min <- c(10, 10, 25, 50, 100)/1000
 size_range <- data.frame(at = 2 * size_min, min = size_min, max = 4 * size_min)
 size_range[1, "at"] <- 10 / 1000

 cex_lab <- 1.2
 example_species <- c("Alseis blackiana", "Cordia bicolor", "Garcinia madruno","Tabernaemontana arborea")
 nspecies <- length(example_species)

 par(mfrow=c(4,5), mar=c(1,1,1,1), oma=c(5,5,3,3))

 for(j in seq_len(nspecies)) {
  data1 <- filter(BCI_individual_growth, species ==  example_species[j])
   for(i in seq_len(nrow(size_range))) {

      data <- filter(data1,
        dbh >= size_range[["min"]][i] & dbh < size_range[["max"]][i])

      x <- data[["dbh"]]
      y <- data[["dbasal_diam_dt"]]
      n <- length(y)
      plot(x, y, log="x", pch=16, col="grey", ann=FALSE, xaxt="n", yaxt="n", cex=1.2,
        ylim =  c(-1, 32) / 1000, xlim = as.numeric(size_range[i, c("min", "max")]))
      legend("topright", legend=sprintf("n = %d", n), bty="n", cex=1.2, text.col="grey")
      axis(1, labels=j==nspecies)
      axis(2, labels=i==1)

      at <- size_range[["at"]][i]
      if (j == 1) {
        mtext(sprintf("D=%sm", at), 3, cex=cex_lab, line=2, xpd=NA)
      }
      if (n > 500) {
        f <- fit_quantile_regression(y, x, 0.99)
        abline(f, lwd=1, col="red")
        points(at, predict(f, newdata = data.frame(x=at)), col="red", pch=16, cex=1.5)
      }



      if (i == last(nrow(size_range))) {
        mtext( example_species[j], 4, cex=cex_lab, line=2)
      }


    }
  }

  mtext("Diameter (m)", 1, cex=cex_lab, line=3, outer=TRUE)
  mtext(name_pretty("dbasal_diam_dt"), 2, line=3, outer=TRUE, cex=cex_lab)

}
