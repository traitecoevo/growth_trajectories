all: figs/RGR.pdf figs/SI-leaf.pdf figs/hump.pdf figs/LMA_growth_size.pdf figs/mass_fraction.pdf figs/LMA_growth_light.pdf figs/LMA_wplcp.pdf figs/LMA_optim.pdf

figs/mass_fraction.pdf: analysis/Fig-mass_fraction.R
	Rscript $<

figs/LMA_optim.pdf: analysis/Fig-LMA_optim.R
	Rscript $<

figs/LMA_wplcp.pdf: analysis/Fig-LMA_wplcp.R
	Rscript $<

figs/LMA_growth_light.pdf: analysis/Fig-LMA_growth_light.R
	Rscript $<

figs/LMA_growth_size.pdf: analysis/Fig-LMA_growth_size.R
	Rscript $<

figs/hump.pdf: analysis/Fig-hump.R
	Rscript $<

figs/SI-leaf.pdf: analysis/Fig-leaf.R data/wright-2004.csv
	Rscript $<

figs/RGR.pdf: analysis/Fig-RGR.R data/wright-2004.csv data/wright-2010.txt
	Rscript $<

data/wright-2010.txt: analysis/make-data-wright-2010.txt.R
	Rscript $<

data/wright-2004.csv: analysis/make-data-wright-2004.csv.R data/wright-2004.xls
	Rscript $<

data/wright-2004.xls: analysis/make-data-wright-2004.xls.R
	Rscript $<

clean:
	rm -f figs/*.pdf

.PHONY: all clean
