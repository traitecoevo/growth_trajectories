
BCI_download_50ha_plot <- function(dest) {
  if (!file.exists(dest))
    stop(sprintf("Please visit %s and save file as %s. See Readme file for furtehr instructions.", "http://ctfs.arnarb.harvard.edu/webatlas/datasets/bci/", dest))
}

BCI_load_50ha_plot <- function(path) {
  path_to_BCI_data <- dirname(path)
  files <- data.frame(year = c(1990, 1995, 2000, 2005, 2010),
    name = paste0(path_to_BCI_data, "/bci.full", 3:7, ".csv"), stringsAsFactors = FALSE)
  tbl_df(ddply(files, "year", function(x) read.csv(x[["name"]], stringsAsFactors = FALSE)))
}

BCI_calculate_individual_growth <- function(data_in, nomenclature) {

  # sort data so that ordered by species
  data <- data_in %>%
    arrange(sp, treeID, year) %>%
    select(sp, treeID, stemID, year, date, status, pom, dbh, agb) %>%
    mutate(species = lookup_species_name(sp, nomenclature),
      dbh=dbh/1000)

  # set agb to NA when dbh = NA
  data[["agb"]][is.na(data[["dbh"]])] <- NA

  # calculate growth for each tree in each census
  individual_growth <- group_by(data, treeID) %>%
    mutate(dbh_increment = c(diff(dbh), NA),
      pom_change = c(diff(pom), NA)/pom,
      stemID_change = c(diff(stemID), NA),
      dbasal_diam_dt = calculate_growth_rate(dbh, date),
      dbasal_diam_dt_relative = calculate_growth_rate(dbh, date, log),
      basal_area = 0.25 * pi * dbh^2,
      dbasal_area_dt = calculate_growth_rate(basal_area, date))

  individual_growth
}

BCI_calculate_species_traits <- function(individual_growth, wright_2010) {

  data <- mutate(tbl_df(individual_growth),
      keep = flag_bad_data(dbh, dbh_increment, dbasal_diam_dt, pom_change, stemID_change)) %>%
    select(species, keep, status, dbh, dbasal_diam_dt, basal_area, dbasal_area_dt) %>%
    filter(keep & status == "A" & !is.na(dbasal_diam_dt))

  # bands in which to calculate growth rate In all but first band we estimate growth at centre of band.  The first interval is repeated so that we can estimate
  # growth at LHS also
  size_min <- c(10, 10, 25, 50, 100, 200)/1000
  size_range <- data.frame(at = 2 * size_min, min = size_min, max = 4 * size_min)
  size_range[1, "at"] <- 10 / 1000


  get_species_data <- function(data, min, max, at) {
    group_by(data, species) %>%
      filter(dbh >= min & dbh < max) %>%
      summarise(count = n(),
        dbasal_diam_dt = predict_via_quantile_regression(dbh, dbasal_diam_dt, predict_at = at),
        dbasal_area_dt = predict_via_quantile_regression(dbh, dbasal_area_dt, predict_at = at))
  }

  out <- ddply(size_range, 1, function(x) get_species_data(data, x[["min"]], x[["max"]], x[["at"]]))

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
flag_bad_data <- function(dbh, dbh_increment, dbasal_diam_dt, pom_change, stemID_change) {

  slope <- 0.006214
  intercept <- 0.9036 /1000   #convert from mm to m
  error_limit <- 4
  max_growth <- 75 / 1000     #convert from mm to m
  pom_cut <- 0.05

  accept <- rep(TRUE, length(dbh))
  # Remove records based on max growth rate
  accept[!is.na(dbasal_diam_dt) & dbasal_diam_dt > max_growth] <- FALSE
  # Remove records based on min growth rate, estimated from allowbale error
  allowable.decrease <- -error_limit * (slope * dbh + intercept)
  accept[!is.na(dbh_increment) & dbh_increment < allowable.decrease] <- FALSE
  # Remove trees where point of measurement changes substantially
  accept[!is.na(pom_change) & pom_change > pom_cut] <- FALSE
  # # remove records where stem change accept[stemID_change !=0] <- FALSE
  accept
}

# lookup full Latin name given 4 or 6 letter abbreviation
lookup_species_name <- function(tag, conversion_table, column = "sp6") {

  data <- conversion_table

  # sort table
  data <- data[!is.na(data[[column]]) & data[[column]] != "", ]
  data <- data[order(data[[column]]), ]

  i <- match(tolower(tag), tolower(data[[column]]))
  paste(data[["genus"]], data[["species"]])[i]
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
