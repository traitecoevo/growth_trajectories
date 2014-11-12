all: MS.pdf theory.pdf

MS.pdf: MS.md table-pars.tex

figs:
	make -C ..

the-american-naturalist.csl:
	curl -k https://raw.githubusercontent.com/citation-style-language/styles/master/the-american-naturalist.csl > $@

nature.csl:
	curl -k https://raw.githubusercontent.com/citation-style-language/styles/master/nature.csl > $@

%.pdf: %.md include.tex nature.csl the-american-naturalist.csl
	Rscript -e "source('ms-fun.R');pandoc_build('$<')"

table-pars.tex:
	Rscript -e "library(xtable); data <- read.csv('../tree/doc/Growth_model_pars.csv', stringsAsFactors=TRUE); x<-xtable(data[-c(1,2,4,7,10,21,22,25:31),1:4],hline.after=c(1), align='lp{5cm}lll'); y <- print(x,sanitize.text.function=function(str)as.character(str),include.rownames=FALSE,floating=FALSE,print.results = FALSE); cat(y, file ='table-pars.tex')"

clean:
	rm -f *.pdf
