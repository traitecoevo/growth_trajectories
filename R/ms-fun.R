latex_build <- function(filename, latex_engine="xelatex", bibtex=TRUE, dest_dir = "."){
  build <- sprintf("%s -interaction=nonstopmode %s", latex_engine, filename)
  basename <- tools::file_path_sans_ext(filename)
  system(build)
  if(bibtex){
	  system(sprintf("bibtex %s",  basename))
    system(build)
  }
  system(build)
  remove_aux_files(filename)
  if(dest_dir!="."){
    dir.create(dest_dir, showWarnings = FALSE, recursive = TRUE)
    system(sprintf("mv %s.pdf %s", basename, dest_dir))
  }
}

latex_build2 <- function(filename, latex_engine="xelatex"){
  system(sprintf('latexmk -pdf -pdflatex="%s -interaction=nonstopmode" %s', latex_engine, filename))
  remove_aux_files(filename)
}

remove_aux_files <- function(filename) {
	aux.files <- paste0(tools::file_path_sans_ext(filename),
                     c(".log", ".aux", ".bbl", ".blg", ".fls", ".out", ".snm", ".nav", ".toc",
                      ".fdb_latexmk"))
  file.remove(aux.files[file.exists(aux.files)])
}

download_seedling_png <- function(destination_filename) {
  url <-
    "http://ian.umces.edu/imagelibrary/albums/userpics/10002/normal_ian-symbol-quercus-spp-oak-seedling.png"
  download(url, destination_filename, mode="wb")
}

download_tree_png <- function(destination_filename) {
  url <-
    "http://ian.umces.edu/imagelibrary/albums/userpics/12789/normal_ian-symbol-quercus-montana.png"
  download(url, destination_filename, mode="wb")
}
