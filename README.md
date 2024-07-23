# iSEEtree

[![issues](https://img.shields.io/github/issues/microbiome/iSEEtree)](https://github.com/microbiome/iSEEtree/issues)
[![pulls](https://img.shields.io/github/issues-pr/microbiome/iSEEtree)](https://github.com/microbiome/iSEEtree/pulls)
[![R-CMD-check](https://github.com/microbiome/iSEEtree/workflows/rworkflows/badge.svg)](https://github.com/microbiome/iSEEtree/actions)
[![codecov](https://codecov.io/gh/microbiome/iSEEtree/branch/devel/graph/badge.svg)](https://app.codecov.io/gh/microbiome/iSEEtree?branch=devel)
[![codefactor](https://www.codefactor.io/repository/github/microbiome/iseetree/badge)](https://www.codefactor.io/repository/github/microbiome/iseetree)

The goal of iSEEtree is to provide panels related to TreeSummarizedExperiment
objects which facilitate the interactive visualisation of microbiome data in
[_iSEE_](https://isee.github.io/).

## Installation instructions
The release version of iSEEtree can be installed from Bioconductor as follows:

```
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("iSEEtree")
```

Contributors or users interested in the latest functionality can install the
devel version of iSEEtree as follows:

```
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

# The following initializes usage of Bioc devel
BiocManager::install(version='devel')

BiocManager::install("iSEEtree")
```

## Example
The basic functionality of iSEEtree can be explored as follows:

```
library(iSEEtree)
library(mia)
library(scater)

# Import TreeSE
data("Tengeler2020", package = "mia")
tse <- Tengeler2020

# Add relabundance assay
tse <- transformAssay(tse, method = "relabundance")

# Add reduced dimensions
tse <- runMDS(tse, assay.type = "relabundance")

# Launch iSEE
if (interactive()) {
  iSEE(tse)
}
```

## Code of Conduct
Please note that the iSEEtree project is released with a
[Contributor Code of Conduct](https://bioconductor.org/about/code-of-conduct/).
By contributing to this project, you agree to abide by its terms. Contributions
are welcome in the form of feedback, issues and pull requests. You can find the
contributor guidelines of the miaverse
[here](https://github.com/microbiome/mia/blob/devel/CONTRIBUTING.md).

## Acknowledgements
Please note that iSEEtree was only made possible thanks to many other R and
bioinformatics software authors, which are cited either in the vignettes and/or
the paper(s) describing this package. In particular, iSEEtree implements the [_miaViz_](https://microbiome.github.io/miaViz/) package for microbiome data
visualisation to create panels that are specific for TreeSummarizedExperiment
objects. Not surprisingly, it also depends on the generic panels from iSEE.

This package was developed using the following resources:

- [_usethis_](https://cran.r-project.org/web/packages/usethis/) to generate an
  initial template.
- Continuous code testing is performed on
  [GitHub actions](https://github.com/features/actions) and include R CMD check,
  [_BiocCheck_](https://bioconductor.org/packages/3.16/bioc/html/BiocCheck.html)
  and testthat.
- Code coverage assessment is possible thanks to
  [codecov](https://app.codecov.io/gh/).
- The documentation website is automatically updated thanks to
  [_pkgdown_](https://cran.r-project.org/web/packages/pkgdown/).
- The documentation is formatted thanks to
  [_devtools_](https://cran.r-project.org/web/packages/devtools/) and
  [_roxygen2_](https://cran.r-project.org/web/packages/roxygen2/).
- All the actions above are made reproducible by
  [_rworkflows_](https://neurogenomics.github.io/rworkflows/)