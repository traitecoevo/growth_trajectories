#List of functions used to manipulate and summarise data from BCI.

#Download BCI data
# download function from package downloader provides wrapper
# to download file so that works for https and across platforms
BCI_download_50ha_plot_full <- function(dest) {
  url <-"https://repository.si.edu/bitstream/handle/10088/20925/bci.full.Rdata31Aug2012.zip"
  download(url, dest, mode="wb")
}

BCI_download_50ha_plot_stem <- function(dest) {
  url <-"https://repository.si.edu/bitstream/handle/10088/20925/bci.stem.Rdata31Aug2012.zip"
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

  files<- data.frame(name=list.files(tmp, pattern=".rdata", full.names=TRUE),
                    year = c(1980, 1985, 1990, 1995, 2000, 2005, 2010), stringsAsFactors = FALSE)
  tbl_df(ldply(list.files(tmp, pattern=".rdata", full.names=TRUE), function(x) load_rdata(x)))
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

# Calculates growth rate, either absolute (default) or relative relative (set f=log)
calculate_growth_rate <- function(x, t, f=function(y) y){
  dt = diff(t)/365.25
  if(any(dt < 0, na.rm=TRUE)){
    stop("time must be sorted")
  }
  c(NA, diff(f(x))/dt)
}

# Determines if individual died in following census
mortality_in_next_census <- function(status){
  if(length(status) > 1){
    i <- 1:(length(status)-1)} # if more than 1 obs
  else{
    i <- 0
  }
  as.numeric(c(status[i] == 'A' & status[i+1] == 'D', NA))
}

#Flags individuals that return from the dead
is_zombie <- function(status){
  i <- seq_len(length(status)-1)
  any(status[i] == 'D' & status[i+1] == 'A')
}

# Flags bad data. Adapted from function in CTFS R package. Errorenous data = 0
flag_bad_data <- function(dbh, dbh_increment, dbasal_diam_dt) {

  slope <- 0.006214
  intercept <- 0.9036 /1000   #convert from mm to m
  error_limit <- 4
  max_growth <- 75 / 1000     #convert from mm to m

  accept <- rep(TRUE, length(dbh))
  # Remove records based on max growth rate
  accept[!is.na(dbasal_diam_dt) & dbasal_diam_dt > max_growth] <- FALSE
  # Remove records based on min growth rate, estimated from allowbale error
  allowable.decrease <- -error_limit * (slope * dbh + intercept)
  accept[!is.na(dbh_increment) & dbh_increment < allowable.decrease] <- FALSE
  # Remove trees where point of measurement changes substantially
  accept
}

#Screen data and calculate growth rates
BCI_calculate_individual_growth <- function(BCI_50haplot_data, BCI_nomenclature) {
# library(maker)
# m <- maker()
# e <- m$make_dependencies("BCI_individual_growth")
# maker_attach(e)
# BCI_50haplot_data <- BCI_50haplot_stem

  data <- BCI_50haplot_data
  names(data) <- tolower(names(data)) # lower case for all column names

  # sort bci data so that ordered by species
  data <- data %>%
    select(sp, treeid, stemid, hom, exactdate, status, dbh) %>%
    mutate(family = lookup_family(sp, BCI_nomenclature),  # adds family column
         latin = lookup_latin(sp, BCI_nomenclature)) %>%
    arrange(latin, treeid, stemid, exactdate)

  # From 1990 to 2010 there are 320 species, 374797 individuals and 1555593 observations
  individual_growth <- group_by(data, treeid) %>%
    # Selects observations that were recorded at 1.3 m
    # Also removes species from families that don't exhibit dbh growth or species that were not named.
    # resulitng in the removal of 14 species, 13586 individual plants removed or 171927 observations
   filter( abs(hom - 1.3) < 1e-4  #NB hom ==1.3 does not work due to precision errors
            & !is_zombie(status)
            &  status=='A'
            & !family %in% c('Arecaceae', 'Cyatheaceae', 'Dicksoniaceae', 'Metaxyaceae',
                            'Cibotiaceae', 'Loxomataceae', 'Culcitaceae', 'Plagiogyriaceae',
                            'Thyrsopteridaceae')
            & !is.na(sp) ) %>%

    # Removes multistemmed plants because stemid isn't consistently recorded for multistemmed plants
    # resulting in the removal of 6 additional species, 74085 multistemmed individuals or 451152 observations
    mutate(nostems = length(na.exclude(unique(stemid)))) %>% # na.exclude because dead individuals contain a NA stemid.
    filter(nostems == 1) %>%

    # Removes inidividuals that do not survive more than 1 census (required to estimate growth rates).
    # resulting in the removal of an additional 23 species, 97980 individuals or 134111 observations

    mutate(ncensus = length(unique(exactdate))) %>%
    filter(ncensus >1) %>%

    # Calculates growth rates, census interval and flags whether data is erroneous (via CTFS function) or plants return from dead.
    mutate(
      dbh = dbh/1000, #convert to m
      julian = as.vector(julian(as.Date(exactdate,"%Y-%m-%d"), as.Date("1970-01-01", "%Y-%m-%d"))),
      census_interval = c(NA, diff(julian/365.25)),
      dbh_increment = c(NA, diff(dbh)),
      dbasal_diam_dt = calculate_growth_rate(dbh, julian),
      dbasal_diam_dt_relative = calculate_growth_rate(dbh, julian, log),
      basal_area = 0.25*pi*dbh^2,
      basal_area_dt = calculate_growth_rate(basal_area, julian),
      basal_area_dt_relative = calculate_growth_rate(basal_area, julian, log)) %>% # CTFS datacheck (Dodgy data= 0).

    # Removes redundant rows for dead individuals, zombie plants and data flagged as erroneous.
    # datacheck reveals 2495 individuals contain erroneous data resulting in the removal of 11401 observations
    # 262 individuals are zombies resulting in the removal of 1202 observations
    # 69982 redundant dead rows.
    # 184950 observations are the first records of an individual & contain no measure of growth rate

    filter(!is.na(dbh_increment) &
            flag_bad_data(dbh, dbh_increment, dbasal_diam_dt) )# CTFS datacheck (Dodgy data= 0).

  # Rearranges columns
  tbl_df(individual_growth) %>%
    select(latin,sp,treeid,exactdate,julian,census_interval,
           dbh,dbh_increment,dbasal_diam_dt,dbasal_diam_dt_relative,basal_area,basal_area_dt,
           basal_area_dt_relative)
}

BCI_calculate_species_traits <- function(individual_growth, wright_2010) {
  # bands in which to calculate growth rate In all but first band we estimate growth at centre of band.  The first interval is repeated so that we can estimate
  # growth at LHS also
  size_min <- c(10, 10, 25, 50, 100, 200)/1000
  size_range <- data.frame(at = 2 * size_min, min = size_min, max = 4 * size_min)
  size_range[1, "at"] <- 10 / 1000


  get_species_data <- function(individual_growth, min, max, at) {
    group_by(individual_growth, latin) %>%
      filter(dbh >= min & dbh < max) %>%
      summarise(count = n(),
                dbasal_diam_dt = predict_via_quantile_regression(dbh, dbasal_diam_dt, predict_at = at),
                basal_area_dt = predict_via_quantile_regression(dbh, basal_area_dt, predict_at = at))
  }

  out <- ddply(size_range, 1, function(x) get_species_data(individual_growth, x[["min"]], x[["max"]], x[["at"]]))

  # merge with data from Wright 2010
  wright_2010 <- mutate(wright_2010, latin = paste(genus, species))

  species_data <- merge(select(wright_2010, latin, lma, rho, hmat), out, by = "latin") %>%
    filter(!is.na(dbasal_diam_dt)) %>%
    arrange(latin, at)

  names(species_data) <- tolower(names(species_data))

  species_data
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

figure_trait_growth_data <- function(X, Y, cex = 1, col = "darkgreen", pch = 1, ylim = range(Y, na.rm = TRUE), axes = c("lma", "dbasal_diam_dt"), ytick.lab = TRUE, ...) {
  if (ytick.lab){
    new_plot(axes[1], axes[2], log = "xy", ylim = ylim, xlab = NULL, ylab = NULL, ...)
  } else{
    new_plot(axes[1], axes[2], log = "xy", ylim = ylim, xlab = NULL, ylab = NULL, ytick.lab = NA, ...)
  }

  sm <- sma(Y ~ X, log = "xy", method = "OLS")
  points(X, Y, col = "black", pch = pch, type = "p", cex = cex)
  plot(sm, add = T, col = col, pch = pch, lwd = 2, type = "l", p.lines.transparent = 0.1)

  lab <- paste0("r2 = ", format(round(sm[["groupsummary"]][["r2"]],2), nsmall = 2), ", n=",sum(!is.na(Y) & !is.na(X)))
  legend("topright", legend = lab, bty = "n", cex=1.25)
}

figure_trait_growth_data_panel <- function(data, axes = c("lma", "dbasal_diam_dt"), ylim = c(1, 30)/1000, at = c(10, 20, 60, 120), title = FALSE, ...) {

  trait <- axes[[1]]
  for (a in at) {
    data_sub <- data[data[["at"]] == a & data[["count"]] > 800, ]
    if (a == at[1])
      ytick <- TRUE else ytick <- FALSE
    figure_trait_growth_data(X = data_sub[[trait]], Y = data_sub[[axes[2]]], ylim = ylim,
                             cex = linear_rescale(log10(data_sub[["count"]]), c(0.6, 2.5), log10(c(800, 10000))),
                             ytick.lab = ytick, axes=axes, ...)
    if (title)
      mtext(paste0("dbh=", a, "m"), cex=1, line =1 )
  }
}


fig_BCI_data <- function(data, at = c(10, 20, 50, 100, 200)/1000) {

# library(maker)
# m <- maker()
# e <- m$make_dependencies("ms/figures/BCI_data.pdf")
# maker_attach(e)

  op <- par(oma = c(3, 6, 3, 1), mar = c(1, 1, 2, 1))

  nrows <- 3
  ncols <- length(at)

  m <- matrix(rep(c(1:ncols, rep(ncols + 1, ncols)), nrows) + sort(rep(0:(nrows - 1), ncols * 2)) * (ncols + 1), ncol = ncols, byrow = TRUE)
  layout(m, widths = rep(1/ncols, ncols), heights = rep(c(0.8, 0.2)/nrows, nrows))

  figure_trait_growth_data_panel(data, axes = c("lma", "dbasal_diam_dt"), at = at, title=TRUE, xlim= c(0.02, 0.2))
  header_plot(get_axis_info("lma", "lab"))

  figure_trait_growth_data_panel(data, axes = c("rho", "dbasal_diam_dt"), at = at)
  header_plot(get_axis_info("rho", "lab"))

  figure_trait_growth_data_panel(data, axes = c("hmat", "dbasal_diam_dt"), at = at)
  header_plot(get_axis_info("hmat", "lab"))

  mtext(get_axis_info("dbasal_diam_dt", "lab"), line = 4, side = 2, cex = 1, outer = TRUE)
  par(op)
}
