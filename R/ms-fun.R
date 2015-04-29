# interaction must be one of: batchmode, nonstopmode, scrollmode, errorstopmode
# default combination causes latex to bail back to system on error

latex_build <- function(filename, latex_engine="xelatex", bibtex=TRUE, dest_dir = ".", interaction="nonstopmode") {
  build <- sprintf("%s -interaction=%s -halt-on-error -output-directory=%s %s", latex_engine, interaction, dest_dir, filename)
  basename <- tools::file_path_sans_ext(filename)
  system(build)
  if(bibtex){
	  system(sprintf("bibtex %s",  basename))
    system(build)
  }
  system(build)
  remove_aux_files(file.path(dest_dir, basename(filename)))
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
