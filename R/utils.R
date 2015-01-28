
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

na.clean <- function(x) {
  x[!is.na(x)]
}

seq_log_range <- function(r, length.out) {
  seq_log(r[[1]], r[[2]], length.out)
}

seq_log <- function(from, to, length.out) {
  exp(seq(log(from), log(to), length.out = length.out))
}
