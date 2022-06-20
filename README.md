# Introduction to Systems Biology - Project

Replication project integrating ODE models in R.

For the original paper, see [Simulations of an impulsive model for the growth of fruit trees](https://iopscience.iop.org/article/10.1088/1742-6596/2153/1/012018/pdf).

## Table of contents
* [Installation](#installation)
* [Packages](#packages)
* [Links](#links)
* [Files](#files)
* [Support](#support)

## Installation
NOTE: Installation is based on an Ubuntu (Linux) environment.
For installations regarding a different OS, please look up the appropriate guide for your environment: [CRAN Mirrors](https://cran.r-project.org/mirrors.html)

Run the following lines (if you are on `root`, remove `sudo`):
```shell
sudo apt update -qq
sudo apt install --no-install-recommends software-properties-common dirmngr
wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | sudo tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
sudo add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"
sudo apt install --no-install-recommends r-base
```

## Packages
Installation of the packages can be done in an R console with the `install.packages()` function.
* [deSolve](https://cran.r-project.org/web/packages/deSolve/index.html)
* [ggpubr](https://cran.r-project.org/web/packages/ggpubr/index.html)
* [formatR](https://cran.r-project.org/web/packages/formatR/index.html)
* [scales](https://cloud.r-project.org/web/packages/scales/index.html)
* [ggplot2](https://ggplot2.tidyverse.org/)*

<sup>*</sup> This package is not available via CRAN. Follow the link for installation.<br>

## Files
The `Final` directory contains all the files for the final report.
The directories named with `Week` contain the assignments accordingly.

## Links
* [Project information](https://bioinf.nl/~fennaf/thema08/)
* [GitHub](https://github.com/cappuchinese/Systems-Biology)
* [Simulations of an impulsive model for the growth of fruit trees](https://iopscience.iop.org/article/10.1088/1742-6596/2153/1/012018/pdf)

## Support
To report problems, open an issue on [Github](https://github.com/cappuchinese/Systems-Biology/issues).