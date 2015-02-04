
pandoc_build <- function(file){
  args <- list('--template=include.tex', '--latex-engine=xelatex')
  pandoc_convert(basename(file), to="latex", output= paste0(tools::file_path_sans_ext(basename(file)), ".tex"), citeproc = FALSE, options = args, verbose = FALSE, wd = dirname(file))
}

get_nature_csl <- function(dest, url="https://raw.githubusercontent.com/citation-style-language/styles/master/nature.csl"){
	cat(getURL(url), file=dest)
}

get_amnat_csl <- function(dest, url="https://raw.githubusercontent.com/citation-style-language/styles/master/the-american-naturalist.csl"){
	cat(getURL(url), file=dest)
}

tex_2_pdf <- function(texfile){

  filename <- tools::file_path_sans_ext(texfile)
  system(sprintf("xelatex %s", texfile))
  system(sprintf("bibtex %s",   filename))
  system(sprintf("xelatex %s", texfile))
  system(sprintf("xelatex %s", texfile))
  aux.files <- paste0(filename, c(".log", ".aux", ".bbl", ".blg"))
  file.remove(aux.files[file.exists(aux.files)])
}
