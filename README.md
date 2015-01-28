# Analysis investigating how the influence of traits on growth changes with size

The paper will be based on presentation at Intecol2013: Falster, Daniel (2013): Growth trajectories: a new way of understanding the influence of traits on plant growth. http://dx.doi.org/10.6084/m9.figshare.775379

# Setup

We use the TREE growth model, which is managed using git's [submodules feature](http://git-scm.com/book/en/Git-Tools-Submodules).

To setup existing submodules after cloning run

	git submodule init
	git submodule update

To update submodule to correct version:

	git submodule update

The TREE package also needs to be installed.

    R CMD INSTALL tree

After installation, tests can be run by running

    make test

from within the tree directory.

Best to install `smatr` from github to avoid warnings in the plotting function in the CRAN version:

```
devtools::install_bitbucket("remkoduursma/smatr)
```

# Variables


TRAITS

- "lma"
- "hmat"
- "rho"

SIZE

- "height"
- "basal_area"
- "dbh"

GROWTH

- "height_growth_rate"
- "height_rgr"
- "dbasal_diam_dt"
- "dbh_rgr"
- "ba_gr"
- "ba_rgr"
- "mt_gr"
- "mt_rgr"

OTHER

- "leaf_turnover_rate"
- "leaf_area_above"

TREE_VARS

_["dleaf_area_dleaf_mass"]=1/strategy->lma,
_["leaf_fraction"]=vars.leaf_fraction,
_["growth_fraction"]=1-vars.reproduction_fraction,
_["net_production"]=vars.net_production,
_["dmass_sapwood_dmass_leaf"]=dmass_sapwood_dmass_leaf(),
_["dmass_bark_dmass_leaf"]=dmass_bark_dmass_leaf(),
_["dmass_root_dmass_leaf"]=dmass_root_dmass_leaf(),
_["dleaf_area_dt"]=dleaf_area_dt(),
_["dsapwood_area_dt"]=dsapwood_area_dt(),
_["dbark_area_dt"]=dbark_area_dt(),
_["dheartwood_area_dt"]=dheartwood_area_dt(),
_["dheartwood_mass_dt"]=sapwood_turnover(),
_["dbasal_area_dt"]=dbasal_area_dt(),
_["dbasal_diam_dbasal_area"]=dbasal_diam_dbasal_area(),
_["dbasal_diam_dt"]=dbasal_diam_dt()
