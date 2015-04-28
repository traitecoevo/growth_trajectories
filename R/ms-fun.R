latex_build <- function(filename, latex_engine="xelatex", bibtex=TRUE){
  # filename <- tools::file_path_sans_ext(filename)
  build <- sprintf("%s -interaction=nonstopmode %s", latex_engine, filename)
  system(build)
  if(bibtex){
	  system(sprintf("bibtex %s",   tools::file_path_sans_ext(filename)))
  }
  system(build)
  system(build)
  remove_aux_files(filename)
}

latex_build2 <- function(filename, latex_engine="xelatex"){
  system(sprintf('latexmk -pdf -pdflatex="%s -interaction=nonstopmode" %s', latex_engine, filename))
  remove_aux_files(filename)
}

remove_aux_files <- function(filename) {
	aux.files <- paste0(tools::file_path_sans_ext(filename),
                     c(".log", ".aux", ".bbl", ".blg", ".fls", ".out",
                      ".fdb_latexmk"))
  file.remove(aux.files[file.exists(aux.files)])
}
