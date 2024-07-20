# iSEEtree

[![issues](https://img.shields.io/github/issues/microbiome/iSEEtree)](https://github.com/microbiome/iSEEtree/issues)
[![pulls](https://img.shields.io/github/issues-pr/microbiome/iSEEtree)](https://github.com/microbiome/iSEEtree/pulls)
[![R-CMD-check](https://github.com/microbiome/iSEEtree/workflows/rworkflows/badge.svg)](https://github.com/microbiome/iSEEtree/actions)
[![codecov](https://codecov.io/gh/microbiome/iSEEtree/branch/devel/graph/badge.svg)](https://app.codecov.io/gh/microbiome/iSEEtree?branch=devel)
[![codefactor](https://www.codefactor.io/repository/github/microbiome/iseetree/badge)](https://www.codefactor.io/repository/github/microbiome/iseetree)

## Introduction

iSEEtree is a Bioconductor package for the interactive visualisation of
microbiome data stored in a TreeSummarizedExperiment (TreeSE) container. On the
one side, it leverages and extends the graphics of the iSEE package, which is
designed for the generic SummarizedExperiment class. On the other side, it
employs the statistical and visual tools for microbiome data science provided by
the mia family of packages. Thus, iSEE and mia represent the two building blocks
of iSEEtree. Detailed introductory material on these two frameworks is available
in the [iSEE-verse website](https://isee.github.io/) and the
[OMA Bioconductor book](https://microbiome.github.io/OMA/docs/devel/),
respectively.

iSEEtree is meant for new and experienced users alike, who desire to create and
interact with several graphics for microbiome data, without the need for an
in-depth knowledge of the underlying mia functionality. Current
microbiome-specific panels include phylogenetic trees, ordination plots and compositional plots, which can be further explored below in this article. Other
more generic panels are also reused from the iSEE package and can be
experimented in [this article](https://isee.github.io/iSEE/articles/basic.html).

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

## Panels

iSEEtree derives its microbiome-related visualisation methods from the
[miaViz](https://microbiome.github.io/miaViz/) package, which is code-based and
requires basic knowledge of R programming and microbiome data structures. The
following panels represent an easy-to-use interactive version of the miaViz
plotting functions:

- [AbundanceDensityPlot](https://microbiome.github.io/iSEEtree/reference/AbundanceDensityPlot.html):
  a density plot of the top features, where every line is a feature and the x
  axis shows its abundance for different samples. Its interpretation is
  explained in the OMA chapter on
  [Exploration](https://microbiome.github.io/OMA/docs/devel/pages/12_quality_control.html).
- [AbundancePlot](https://microbiome.github.io/iSEEtree/reference/AbundancePlot.html):
  a compositional barplot of the samples, where every bar is a sample composed
  by different features in different colours. Its interpretation is explained
  in the OMA chapter on
  [Community Composition](https://microbiome.github.io/OMA/docs/devel/pages/21_microbiome_community.html).
- [RDAPlot](https://microbiome.github.io/iSEEtree/reference/RDAPlot.html): an
  supervised ordination plot of the samples, where every dot is a sample on a
  reduced dimensional space and every arrow reflects the contribution of a
  sample variable. Its interpretation is explained in the OMA chapter on
  [Community Similarity](https://microbiome.github.io/OMA/docs/devel/pages/20_beta_diversity.html).
- [RowTreePlot](https://microbiome.github.io/iSEEtree/reference/RowTreePlot.html):
  a phylogenetic tree of the features, where every tip is a feature and two
  neighbouring tips are more closely related than two far apart from each other.

By default, the iSEEtree layout also includes the following panels inherited by
iSEE:

- [RowDataTable](https://isee.github.io/iSEE/articles/basic.html#row-data-tables)
- [ColumnDataTable](https://isee.github.io/iSEE/articles/basic.html#column-data-tables)
- [ReducedDimensionPlot](https://isee.github.io/iSEE/articles/basic.html#reduced-dimension-plots)
- [ComplexHeatmapPlot](https://isee.github.io/iSEE/articles/basic.html#heat-maps)

The [ColumnDataPlot](https://isee.github.io/iSEE/articles/basic.html#coldataplot)
could also prove useful for the visualisation of column variables such as
alpha diversity indices. Its interpretation is explained in the OMA chapter on
[Community Diversity](https://microbiome.github.io/OMA/docs/devel/pages/14_alpha_diversity.html).

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