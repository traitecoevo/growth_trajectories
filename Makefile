all: output/figs/GR-LCC.pdf output/figs/SI-leaf.pdf output/figs/SI-size-dhdt.pdf output/figs/growth-lma_abs_height.pdf output/figs/SI-mass_fraction.pdf output/figs/growth-light.pdf output/figs/LMA_wplcp.pdf output/figs/LMA_optim.pdf

output/figs/GR-LCC.pdf: analysis/Fig-GR.R
	Rscript $<

output/figs/growth-lma_abs_height.pdf: analysis/Fig-LMA_growth_size.R
	Rscript $<

output/figs/growth-light.pdf: analysis/Fig-LMA_growth_light.R
	Rscript $<

output/figs/LMA_wplcp.pdf: analysis/Fig-LMA_wplcp.R
	Rscript $<

output/figs/LMA_optim.pdf: analysis/Fig-LMA_optim.R
	Rscript $<

output/figs/SI-size-dhdt.pdf: analysis/Fig-SI-size.R
	Rscript $<

output/figs/SI-mass_fraction.pdf: analysis/Fig-SI-mass_fraction.R
	Rscript $<

output/figs/SI-leaf.pdf: analysis/Fig-SI-leaf.R data/wright-2004.csv
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
	rm -f output/figs/*.pdf

.PHONY: all clean
