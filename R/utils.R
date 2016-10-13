last <- plant:::last

read_csv <- function(filename, ...){
  read.csv(filename, stringsAsFactors=FALSE, ...)
}

export_csv <- function(data, filename) {
  write.csv(data, filename, row.names = FALSE)
}

category_to_logical <- function(x, trueval) {
  x[x == ""] <- NA
  x == trueval
}

change_column_names <- function(dat, table) {
  i <- match(names(dat), table[["var_in"]])
  j <- !is.na(i) & !is.na(table[["var_out"]][i])
  names(dat)[j] <- table[["var_out"]][i[j]]
  dat[, j, drop = FALSE]
}

change_column_names_file <- function(dat, table_file) {
  change_column_names(dat, read.csv(table_file, stringsAsFactors = FALSE))
}

render_md_as_html <- function(filename) {
  rmarkdown::render(filename, "html_document", quiet = TRUE)
}

#loads an RData file, and returns it
load_rdata <- function(fileName) {
  load(fileName)
  get(ls()[ls() != "fileName"])
}

linear_rescale <- function(x, range, scale = range(x)) {
  p <- (x - scale[[1]])/(scale[[2]] - scale[[1]])
  range[[1]] + p * (range[[2]] - range[[1]])
}

# Does reverse of expand.grid function
# Takes a dataframe with up to 2 or 3 cols and converts into x,y,z vectors
# such that data[,c("x","y)] = expand.grid(x,y)
collapse_grid <- function(data, yvar, xvar, zvar){
  n <- length(data[[yvar]])

  #Find dimensions of X and Y vectors
  ncol <- match(FALSE, data[[yvar]][seq_len(n-1) +1] > data[[yvar]][seq_len(n-1)])
  nrow <- length(data[[xvar]])/ncol

  #Extract x and y vectors
  out <- list()
  out[[yvar]] <- data[[yvar]][1:ncol]
  out[[xvar]] <- data[[xvar]][ncol*seq(0,(nrow-1))+1]

  #Reshape Z data into matrix
  out[[zvar]] <- matrix(data[[zvar]], ncol =ncol, nrow=nrow, byrow=TRUE)
  out
}