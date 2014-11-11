download_wright_2004 <- function(destination_filename) {
  url <-
    "http://www.nature.com/nature/journal/v428/n6985/extref/nature02403-s2.xls"
  download.file(url, destination_filename)
}

process_wright_2004 <- function(filename, sitevars_file) {

  ## There are several strategies for reading in an excel file, but
  ## this one works quite well.
  library(methods) # Serious, WTF Rscript?
  library(xlsx, quietly=TRUE) # Yay, using xlsx::read

  d <-read.xlsx2(filename, sheetIndex=1, startRow=11,
                  stringsAsFactors=FALSE, check.names=FALSE)

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
  d[["Needle"]]    <- category_to_logical(d[["Needle"]],    "N")
  d[["C3"]]        <- category_to_logical(d[["C3"]],        "C3")
  d[["N2fixer"]]   <- category_to_logical(d[["N2fixer"]],   "Y")

  names(d) <- gsub("Log\\.", "Log", names(d))
  re <- "Log"
  i_log <- grep(re, names(d))
  d[i_log] <- lapply(d[i_log], as.numeric)
  d_unlogged <- as.data.frame(10^d[i_log])
  names(d_unlogged) <- sub(re, "", names(d_unlogged))

  d <- cbind(d[-c(i_log)], d_unlogged)

  # add location info
  sitevars <- read.csv(sitevars_file, stringsAsFactors=FALSE)
  data <- merge(d, sitevars, by.x = 'Dataset', by.y = 'dataset_location')

  #lowercase names
  names(data) <- tolower(names(data))


  # unit conversions
  data$lma <- data$lma/1000 # Converts to Kg
  data$n.area <- data$n.area/1000 # Converts to Kg
  data$a.area <- (data$a.area * 31557.6)*10^-6 # converts to mol/kg/yr from micro-mol/g/s
  data$rd.area <- (data$rd.area * 31557.6)*10^-6 # converts to mol/kg/yr from micro-mol/g/s

  data$leaflifespan <- data$leaflifespan/12 ## conevrt LL from months to years
  data$leaf_turnover <- 1/data$leaflifespan ## per year

  data
}
