all: figs/RGR.pdf

figs/RGR.pdf: R/fig-RGR.R data/wright-2004.csv data/Wright2010.txt
	Rscript $<

data/wright-2004.csv: data/wright-2004.xls R/make-data-wright-2004.csv.R
	Rscript R/make-data-wright-2004.csv.R

data/wright-2004.xls:
	Rscript R/make-data-wright-2004.xls.R

clean:
	rm -f $(all)

.PHONY: all clean
