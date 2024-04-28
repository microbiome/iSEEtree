# iSEEtree

[![GitHub issues](https://img.shields.io/github/issues/RiboRings/iSEEtree)](https://github.com/RiboRings/iSEEtree/issues)
[![GitHub pulls](https://img.shields.io/github/issues-pr/RiboRings/iSEEtree)](https://github.com/RiboRings/iSEEtree/pulls)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check-bioc](https://github.com/RiboRings/iSEEtree/workflows/R-CMD-check-bioc/badge.svg)](https://github.com/RiboRings/iSEEtree/actions)
[![Codecov test coverage](https://codecov.io/gh/RiboRings/iSEEtree/branch/devel/graph/badge.svg)](https://app.codecov.io/gh/RiboRings/iSEEtree?branch=devel)

The goal of iSEEtree is to provide panels related to TreeSummarizedExperiment
objects which facilitate the interactive visualisation of microbiome data in
[iSEE](https://isee.github.io/).

## Installation instructions
The development version of iSEEtree can be installed from GitHub as follow:

```
remotes::install_github("RiboRings/iSEEtree")
```

In the future, this package may be submitted to Bioconductor.

## Example
The basic functionality of iSEEtree can be explored as follows:

```
# Import TreeSE
library(mia)
data("GlobalPatterns", package = "mia")
tse <- GlobalPatterns

# Agglomerate TreeSE by Genus
tse_genus <- mergeFeaturesByRank(tse,
                                 rank = "Genus",
                                 onRankOnly = TRUE)

# Add relabundance assay
tse_genus <- transformAssay(tse_genus, method = "relabundance")

# Add PCA
tse_genus <- scater::runPCA(tse_genus, assay.type = "counts")

# Launch iSEE
if (interactive()) {
  iSEE(tse_genus)
}
```

Please note that iSEEtree was only made possible thanks to many other R and
bioinformatics software authors, which are cited either in the vignettes and/or
the paper(s) describing this package. In particular, iSEEtree implements the [miaViz](https://microbiome.github.io/miaViz/) package for microbiome data
visualisation to create panels that are specific for TreeSummarizedExperiment
objects. Not surprisingly, it also depends on the generic panels from iSEE.

## Development tools
- Continuous code testing is performed on
  [GitHub actions](https://github.com/features/actions) and include R CMD check,
  [BiocCheck](https://bioconductor.org/packages/3.16/bioc/html/BiocCheck.html)
  and testthat.
- Code coverage assessment is possible thanks to
  [codecov](https://app.codecov.io/gh/).
- The documentation website is automatically updated thanks to
  [pkgdown](https://cran.r-project.org/web/packages/pkgdown/).
- The documentation is formatted thanks to
  [devtools](https://cran.r-project.org/web/packages/devtools/) and
  [roxygen2](https://cran.r-project.org/web/packages/roxygen2/).
- All the actions above are made reproducible by
  [rworkflows](https://neurogenomics.github.io/rworkflows/)

This package was developed using
[usethis](https://cran.r-project.org/web/packages/usethis/).

## Code of Conduct
Please note that the iSEEtree project is released with a
[Contributor Code of Conduct](https://bioconductor.org/about/code-of-conduct/).
By contributing to this project, you agree to abide by its terms. If you come
across a bug or see potential for improvement, feel free to open an issue and we
will plan the next move. is missing or could be improved. If you are interested
in contributing, you can check [our issues](https://github.com/RiboRings/iSEEtree/issues)
and try to find a solution. Either way, contributions are very much appreciated.
