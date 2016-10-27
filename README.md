# Trajectories: how functional traits influence plant growth and shade tolerance across the life-cycle

This repository contains code needed to reproduce the article:

Falster DS, Duursma R, FitzJohn R (2016) **Growth trajectories: how functional traits influence plant growth**. *bioRxiv*: 083451. [10.1101/083451](http://doi.org/10.1101/083451)

The paper is based on a presentation at Intecol 2013 in London:

Falster, Daniel (2013): **Growth trajectories: a new way of understanding the influence of traits on plant growth**. [10.6084/m9.figshare.775379](http://dx.doi.org/10.6084/m9.figshare.775379)

## Instructions

Recreating all the figures from the paper will take around 2 hrs. Mostly this is because the of the MCMC sampling done to fit lines for figure 3.

### Install relevant software

All analyses were done in `R`. You need to [download this repository](https://github.com/traitecoevo/growth_trajectories/archive/master.zip), and then open an R session with working directory set to the root of the project.

To compile the paper, including figures and supplementary material we use the [remake](https://github.com/richfitz/remake) package for R. You can install remake using the `devtools` package (run `install.packages("devtools")` to install devtools if needed):

```r
devtools::install_github("richfitz/storr", dependencies=TRUE)
devtools::install_github("richfitz/remake", dependencies=TRUE)
```

Our analysis uses data from the [Biomass and Allometry Database](https://github.com/dfalster/baad). Access to that dataset is obtained via installing the [baad.data](https://github.com/traitecoevo/baad.data) package:
```r
devtools::install_github("richfitz/datastorr")
devtools::install_github("traitecoevo/baad.data")
```

We use a number of other packages, which can be easily installed by remake:

```r
remake::install_missing_packages()
```

Finally, compiling the figure 1 and the paper requires a reasonably complete LaTeX installation (e.g. [MacTeX](https://tug.org/mactex/) for OSX or [MikTex](http://miktex.org/) for windows). The LaTeX compilation will depend on a few packages from CTAN, make sure to allow automatic package installation by your LaTeX distribution.

### Recreating the figures and paper

To generate all figures, analyses, and manuscript (PDF, using LaTeX), simply do:

```r
remake::make()
```

To reproduce only the figures run:

```r
remake::make("figures")
```

To make only one of the figures, run a command like

```r
remake::make("output/main/lcp_schematic.pdf")
```

The list of targets can be gleaned from the file `remake.yml`.


If you find remake confusing and prefer to run plain R, you can use remake to build a script `build.R`that produces a given output, e.g.

```r
remake::make_script(filename="build.R")
```
