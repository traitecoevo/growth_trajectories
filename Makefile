all: figs/RGR.pdf figs/SI-leaf.pdf figs/hump.pdf figs/LMA_growth_size.pdf figs/mass_fraction.pdf figs/LMA_growth_light.pdf figs/LMA_wplcp.pdf figs/LMA_optim.pdf

figs/mass_fraction.pdf: R/Fig-mass_fraction.R
	Rscript $<

figs/LMA_optim.pdf: R/Fig-LMA_optim.R
	Rscript $<

figs/LMA_wplcp.pdf: R/Fig-LMA_wplcp.R
	Rscript $<

figs/LMA_growth_light.pdf: R/Fig-LMA_growth_light.R
	Rscript $<

figs/LMA_growth_size.pdf: R/Fig-LMA_growth_size.R
	Rscript $<

figs/hump.pdf: R/Fig-hump.R
	Rscript $<

figs/SI-leaf.pdf: R/Fig-leaf.R data/wright-2004.csv
	Rscript $<

figs/RGR.pdf: R/Fig-RGR.R data/wright-2004.csv data/wright-2010.txt
	Rscript $<

data/wright-2010.txt: R/make-data-wright-2010.txt.R
	Rscript $<

data/wright-2004.csv: R/make-data-wright-2004.csv.R data/wright-2004.xls
	Rscript $<

data/wright-2004.xls: R/make-data-wright-2004.xls.R
	Rscript $<

clean:
	rm -f figs/*.pdf

.PHONY: all clean
