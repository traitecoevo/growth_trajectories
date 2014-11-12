
pandoc_build <- function(file){
  args <- list('--template=include.tex', '--latex-engine=xelatex')
  pandoc_convert(basename(file), output= paste0(tools::file_path_sans_ext(basename(file)), ".pdf"), citeproc = TRUE, options = args, verbose = TRUE, wd = dirname(file))
}

get_nature_csl <- function(dest, url="https://raw.githubusercontent.com/citation-style-language/styles/master/nature.csl"){
	cat(getURL(url), file=dest)
}

get_amnat_csl <- function(dest, url="https://raw.githubusercontent.com/citation-style-language/styles/master/the-american-naturalist.csl"){
	cat(getURL(url), file=dest)
}
