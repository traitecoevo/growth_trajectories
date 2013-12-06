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
