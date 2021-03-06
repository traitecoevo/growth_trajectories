process_wright_2004 <- function(filename) {

  d <- read_excel(filename, sheet = 1, skip = 10)

  ## Do some name translations:
  tr <- c("Code"="Code",
          "Dataset"="Dataset",
          "BIOME"="Biome",
          "Species"="Species",
          "GF"="GrowthForm",
          "Decid/E'green"="Deciduous",
          "Needle/Broad lf"="Needle",
          "C3C4"="C3",
          "N2-fixer"="N2fixer",
          "log LL"="LogLeafLifespan",
          "log LMA"="LogLMA",
          "log Nmass"="Log.N.mass",
          "log Narea"="Log.N.area",
          "log Pmass"="Log.P.mass",
          "log Parea"="Log.P.area",
          "log Amass"="Log.A.mass",
          "log Aarea"="Log.A.area",
          "log Gs"="Log.Gs",
          "log Rdmass"="Log.Rd.mass",
          "log Rdarea"="Log.Rd.area",
          "Ca - Ci"="CaCi")
  names(d)[match(names(tr), names(d))] <- tr

  ## Drop blank columns
  d <- d[names(d) != " "]

  ## Data tweaking:
  d[["Code"]] <- as.integer(d[["Code"]])
  d[["CaCi"]] <- as.numeric(d[["CaCi"]])

  d[["Deciduous"]] <- category_to_logical(d[["Deciduous"]], "D")
  d[["Needle"]] <- category_to_logical(d[["Needle"]], "N")
  d[["C3"]] <- category_to_logical(d[["C3"]], "C3")
  d[["N2fixer"]] <- category_to_logical(d[["N2fixer"]], "Y")

  names(d) <- gsub("Log\\.", "Log", names(d))
  re <- "Log"
  i_log <- grep(re, names(d))
  d[i_log] <- lapply(d[i_log], as.numeric)
  d_unlogged <- as.data.frame(10^d[i_log])
  names(d_unlogged) <- sub(re, "", names(d_unlogged))

  data <- cbind(d[-c(i_log)], d_unlogged)

  # lowercase names
  names(data) <- tolower(names(data))

  ## Convert to Kg from g
  data[["lma"]] <- data[["lma"]]/1000
  ## Convert to Kg from g
  data[["n.area"]] <- data[["n.area"]]/1000
  ## Convert to mol/m2/d from micro-mol/m2/s
  data[["a.area"]] <- (data[["a.area"]] * 24 * 3600) * 10^-6

  ## Convert to mol/m2/d from micro-mol/m2/s
  data[["rd.area"]] <- (data[["rd.area"]] * 24 * 3600) * 10^-6

  ## Convert to years from month
  data[["leaflifespan"]] <- data[["leaflifespan"]]/12
  ## Convert to 1/year from year
  data[["leaf_turnover"]] <- 1/data[["leaflifespan"]]

  data
}
