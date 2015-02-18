tex_2_pdf <- function(texfile){

  system(sprintf('latexmk -pdf -pdflatex="xelatex -interaction=nonstopmode"
                 %s', texfile))
  aux.files <- paste0(tools::file_path_sans_ext(texfile),
                     c(".log", ".aux", ".bbl", ".blg"))
  file.remove(aux.files[file.exists(aux.files)])
}
