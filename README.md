
# Explore and Evaluate the Mechanisms for Missing Data in the Enhanced Tuberculosis Surveillance System

[![badge](https://img.shields.io/badge/Launch-Analysis-lightblue.svg)](https://mybinder.org/v2/gh/seabbs/ETSMissing/master?urlpath=rstudio)
[![Documentation](https://img.shields.io/badge/Documentation-click%20here!-lightgrey.svg?style=flat)](https://www.samabbott.co.uk/ETSMissing)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.2635687.svg)](https://doi.org/10.5281/zenodo.2635687)

[Sam Abbott](https://www.samabbott.co.uk), Hannah Christensen, Ellen
Brooks-Pollock

## Objectives

## Design

## Setting

England and Wales.

## Outcome measures

## Results

## Conclusions

## Reproducibility

### Repository structure

The repository is structured as an R package. It has the following
structure:

  - `data-raw`: Raw data processing.
  - `data`: Processed data.
  - `R`: Supporting R functions.
  - `docs:` Documentation for R code.
  - `vignettes`: Analysis paper, results, and analysis plan.
  - `peer-review`: Documentation required for peer review.

### Manual install

  - Install R (analysis run with `3.6.1`) and Rstudio (alternatively use
    Docker as outlined below).

  - Download the analysis folder from
    <https://github.com/seabbs/ETSMissing/archive/master.zip> or use
    `git clone`, as follows, in the command line (not the R terminal).

<!-- end list -->

``` bash
git clone https://github.com/seabbs/ETSMissing.git
```

  - Once this has been downloaded click on the project file
    (`ETSMissing.Rproj`).

  - Install the analysis dependencies and build the package using the
    following. To enable more robust reproducibility consider using the
    [`checkpoint`](https://cran.r-project.org/web/packages/checkpoint/index.html)
    package versioned locked to R `3.6.1`.

<!-- end list -->

``` r
#install.packages("devtools")
# To build locally
devtools::install_dev_deps(dependencies = TRUE)
devtools::install()
# Alternatively to remote install
devtools::install_github("seabbs/ETSMissing", dependencies = TRUE)
```

  - Load the analysis results by running `vignettes/paper.Rmd`.
    Alternatively the complete analysis (along with documentation) can
    be reconstructed using `make` in the project root directory.

  - See `data-raw` for data processing and the documentation for
    implementation details.

### Docker

This analysis was developed in a docker container based on the tidyverse
docker image. To run the docker image
run:

``` bash
docker run -d -p 8787:8787 --name etsmissing -e USER=etsmissing -e PASSWORD=etsmissing seabbs/etsmissing
```

The rstudio client can be found on port :8787 at your local machines ip.
The default username:password is
assessbcgpolicychange:assessbcgpolicychange, set the user with -e
USER=username, and the password with - e PASSWORD=newpasswordhere. The
default is to save the analysis files into the user directory.

If you have access to the required underlying raw data (see
[`tbinenglanddataclean`](https://www.samabbott.co.uk/tbinenglanddataclean/))
then the entire analysis can be reproduced from scratch by adding the
following to the `docker run` command, with the data saved into
`data/tb_data`. The data requirements, and structure, can be found
[here](https://www.samabbott.co.uk/tbinenglanddataclean/).

``` bash
--mount type=bind,source=$(pwd)/data/tb_data,target=/home/ETSMissing/data/tb_data
```

Alternatively the analysis environment can be accessed via
[binder](https://mybinder.org/v2/gh/seabbs/AssessBCGPolicyChange/master?urlpath=rstudio).
