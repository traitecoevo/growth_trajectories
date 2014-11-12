
pandoc_build <- function(file){
  args <- list('--template=include.tex', '--latex-engine=xelatex')
  rmarkdown::pandoc_convert(file, output= paste0(tools::file_path_sans_ext(file), ".pdf"), citeproc = TRUE, options = args, verbose = TRUE, wd = ".")

}
