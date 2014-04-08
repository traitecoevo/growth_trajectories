all: slides.html

%.html: %.md figs
	pandoc -s -S -t slidy --mathjax $< -o $@

figs: figs/GR.png figs/SI-leaf.png figs/size-dhdt.png figs/growth-light-1.png  figs/allometry.png figs/growth-height.png figs/growth-leaf.png figs/growth-light-0.25.png figs/growth-production.png figs/growth-stem.png figs/LMA_optim.png figs/LMA_wplcp.png figs/mass_fraction.png

%.pdf:
	mkdir -p figs
	cp $(subst figs,../figs,$@) $@

%.png: %.pdf
	convert -resize 1200 -density 300 $< $@

clean:
	rm figs/* slides.html
