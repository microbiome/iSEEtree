# iSEEtree

[![issues](https://img.shields.io/github/issues/microbiome/iSEEtree)](https://github.com/microbiome/iSEEtree/issues)
[![pulls](https://img.shields.io/github/issues-pr/microbiome/iSEEtree)](https://github.com/microbiome/iSEEtree/pulls)
[![R-CMD-check](https://github.com/microbiome/iSEEtree/workflows/rworkflows/badge.svg)](https://github.com/microbiome/iSEEtree/actions)
[![codecov](https://codecov.io/gh/microbiome/iSEEtree/branch/devel/graph/badge.svg)](https://app.codecov.io/gh/microbiome/iSEEtree?branch=devel)
[![codefactor](https://www.codefactor.io/repository/github/microbiome/iseetree/badge)](https://www.codefactor.io/repository/github/microbiome/iseetree)

The goal of iSEEtree is to provide panel designs to explore hierarchical data
stored in TreeSummarizedExperiment objects. This enables the interactive
visualisation of microbiome data, cell lines and more.

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

iSEEtree originates from the joint effort of the R/Bioconductor community. It is
mainly based on the following software:

- [R](https://www.r-project.org/), statistical programming language [@core2024r]
- [mia](https://bioconductor.org/packages/release/bioc/html/mia.html), framework for microbiome data analysis [@borman2024mia]
- [iSEE](https://bioconductor.org/packages/release/bioc/html/iSEE.html), SummarizedExperiment interactive explorer [@rue2018isee]
- [TreeSummarizedExperiment](https://bioconductor.org/packages/release/bioc/html/TreeSummarizedExperiment.htmlm), S4 container for hierarchical data [@huang2021treesummarizedexperiment]
- [shiny](https://cran.r-project.org/web/packages/shiny/index.html), web app development in R [@chang2024shiny]
