all: figs/GR.pdf figs/SI-leaf.pdf figs/size-dhdt.pdf figs/growth-height.pdf figs/mass_fraction.pdf figs/growth-light.pdf figs/LMA_wplcp.pdf figs/LMA_optim.pdf

figs/mass_fraction.pdf: analysis/Fig-mass_fraction.R
	Rscript $<

figs/LMA_optim.pdf: analysis/Fig-LMA_optim.R
	Rscript $<

figs/LMA_wplcp.pdf: analysis/Fig-LMA_wplcp.R
	Rscript $<

figs/growth-light.pdf: analysis/Fig-LMA_growth_light.R
	Rscript $<

figs/growth-height.pdf: analysis/Fig-LMA_growth_size.R
	Rscript $<

figs/size-dhdt.pdf: analysis/Fig-hump.R
	Rscript $<

figs/GR.pdf: analysis/Fig-GR.R data/BCI_species.csv
	Rscript $<

figs/SI-leaf.pdf: analysis/Fig-leaf.R data/wright-2004.csv
	Rscript $<

data/wright-2004.csv: analysis/make-data-wright-2004.csv.R data/wright-2004.xls
	Rscript $<

data/wright-2004.xls: analysis/make-data-wright-2004.xls.R
	Rscript $<

data/BCI_indiv.50ha.Rdata: analysis/make-data-BCI50ha.indiv.R
	Rscript $<

data/BCI_species.csv: analysis/make-data-BCI50ha.species.R data/BCI_indiv.50ha.Rdata data/wright-2010.txt
	Rscript $<

data/wright-2010.txt: analysis/make-data-wright-2010.txt.R
	Rscript $<

clean:
	rm -f figs/*.pdf

.PHONY: all clean
