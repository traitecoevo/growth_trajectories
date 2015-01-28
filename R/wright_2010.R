download_wright_2010 <- function(dest) {
  url <- "http://www.esapubs.org/archive/ecol/E091/257/Supplement_20100505.txt"
  download.file(url, dest)
}

process_wright_2010 <- function(filename) {
  # Correct names of species, anything with length > 12 was clipped
  corrections <- data.frame(rbind(c("costaricensi","costaricensis"),
                                  c("Beilschmiedi","Beilschmiedia"),
                                  c("megalophyllu","megalophyllum"),
                                  c("Chrysochlamy","Chrysochlamys"),
                                  c("Chrysophyllu","Chrysophyllum"),
                                  c("manzinellens","manzinellensis"),
                                  c("billbergianu","billbergianus"),
                                  c("costaricensi","costaricensis"),
                                  c("coloradoensi","coloradoensis"),
                                  c("alchorneoide","alchorneoides"),
                                  c("Tabernaemont","Tabernaemontana"),
                                  c("Trattinnicki","Trattinnickia"),
                                  c("$",""),
                                  c("-99.000000", "NA"),
                                  c("-99.00000", "NA"),
                                  c("-99.0000", "NA"),
                                  c("-99.000", "NA"),
                                  c("-99.00", "NA"),
                                  c("-99.0", "NA"),
                                  c("-99", "NA"),
                                  c("\t", ",")), stringsAsFactors = FALSE)
  names(corrections) <- c("OLD", "NEW")

  x <- readLines(filename)[26:160]

  for (i in 1:nrow(corrections)) {
    x <- gsub(corrections[["OLD"]][i], corrections[["NEW"]][i], x, fixed = TRUE)
  }

  ## This is a hack and should be looked at:
  f <- tempfile()
  on.exit(file.remove(f))
  cat(x, file = f, sep = "\n")
  data <- read.csv(f, stringsAsFactors = FALSE)

  names(data) <- tolower(names(data))
  names(data)[names(data) == "wsg"] <- "rho"
  names(data)[names(data) == "height"] <- "hmat"

  data[["lma"]] <- data[["lma"]] / 1000  #(convert from g/2 to kg/m2)
  data[["rho"]] <- data[["rho"]] * 1000

  data
}
