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

  # Identifies individuals that return from the dead
  is_zombie <- function(status) {
    i <- seq_len(length(status)-1)
    any(status[i] == 'D' & status[i+1] == 'A')
  }

  names(BCI_50haplot) <- tolower(names(BCI_50haplot)) # lower case for all column names

  data <- BCI_50haplot %>%
    arrange(sp, treeid, date) %>%
    select(sp, treeid, nostems, exactdate, date, status, hom, dbh) %>%
    mutate(species = lookup_latin(sp, BCI_nomenclature),
          family = lookup_family(sp, BCI_nomenclature),
          dbh=dbh/1000) %>%
    group_by(treeid) %>%
    # Remove zombies - plants recorded as dead that then reappear at later date
    filter(!is_zombie(status)) %>%
    # Remove stems from earlier census, measured with course resolution
    filter(exactdate > "1990-02-06") %>%
    # Only keep alive stems
    filter(status=="A") %>%
    #Remove plants where height of measurement is not close to 1.3
    filter( abs(hom - 1.3) < 0.05) %>%   #NB hom ==1.3 does not work due to precision errors
    # filter plants with multiple stems
    filter(max(nostems)==1) %>%
    # Remove species from families that don't exhibit dbh growth, eg. palms
    filter(!family %in% c('Arecaceae', 'Cyatheaceae', 'Dicksoniaceae', 'Metaxyaceae',
                        'Cibotiaceae', 'Loxomataceae', 'Culcitaceae', 'Plagiogyriaceae',
                        'Thyrsopteridaceae')) %>%
  mutate(
      dbh_increment = c(diff(dbh), NA),
      dbasal_diam_dt = calculate_growth_rate(dbh, date),
      dbasal_diam_dt_relative = calculate_growth_rate(dbh, date, log)
      ) %>%
    filter(
      !is.na(dbasal_diam_dt) &
      !is.na(dbh_increment) &
      CTFS_sanity_check(dbh, dbh_increment, dbasal_diam_dt)
      ) %>%
    select(species, family, status, dbh, dbasal_diam_dt)
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

# plot to demonstarte the qunatile gression method for estimating growth rates
quantreg_plot <- function(x, y, p = 0.99, nmin = 100, predict_at = 10, add.legend = TRUE, title, ...) {
  n <- length(y)
  plot(y ~ x, log = "x", main = paste0(title, ", n=", n), ...)
  if (n > nmin) {
    f <- fit_quantile_regression(y, x, p)
    abline(h = quantile(y, probs = 0.95), col = "red")
    abline(f, col = "blue")
    points(predict_at, predict(f, newdata = data.frame(x = predict_at)), col = "blue", pch = 16)
  }
  if (add.legend)
    legend("topright", legend = c("Wright2010", "QuantReg"), col = c("red", "blue"), lty = c("solid", "solid"), bty = "n")
}

figure_BCI_data <- function(data) {
  traits <- c("lma", "rho", "hmat")
  at <- c(10, 20, 50, 100, 200)/1000

  op <- par(oma=c(3, 6, 3, 1),
            mar=c(1, 1, 2, 1),
            mfcol=c(length(at), length(traits)))

  data <- data[data$count > 800,]
  dsplit <- lapply(at, function(a) data[data$at == a,])

  ylim <- c(1, 30) / 1000
  cex_lab <- .85

  for (trait in traits) {
    xlim <- range(data[[trait]], na.rm=TRUE)
    for (i in seq_along(at)) {
      dsub <- dsplit[[i]]
      cex <- linear_rescale(log10(dsub$count), c(0.6, 2.5), log10(c(800, 10000)))
      x <- dsub[[trait]]
      y <- dsub$dbasal_diam_dt

      sm <- sma(y ~ x, log="xy", method="OLS")

      plot(x, y, pch=1, xlim=xlim, ylim=ylim, log="xy", cex=cex, xaxt="n", yaxt="n")
      plot(sm, add=TRUE, col="darkgreen", lwd=2, type="l", p.lines.transparent=0.1)

      lab <- sprintf("r2 = %2.2f, n=%d",
                     sm[["groupsummary"]][["r2"]],
                     sum(!is.na(y) & !is.na(x)))
      legend("topright", legend=lab, bty="n", cex=1.25)

      axis(1, labels=i == length(at))
      axis(2, labels=trait == traits[[1]], las=1)
      if (i == length(at)) {
        mtext(name_pretty(trait), 1, line=3, xpd=NA, cex=cex_lab)
      }
    }
  }
  mtext(name_pretty("dbasal_diam_dt"), 2, line=3, outer=TRUE, cex=cex_lab)
}
