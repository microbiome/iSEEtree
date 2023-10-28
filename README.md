# iSEEtree
Welcome to this repository! This is iSEEtree, an extension of the [iSEE universe](https://isee.github.io/) for panels related to TreeSummarizedExperiment objects. It is currently under development.

## Installation
The development version of iSEEtree can be installed from GitHub as follow:

```
remotes::install_github("RiboRings/iSEEtree")
```

In the future, this package may be submitted to a registry.

## API
The basic functionality of iSEEtree can be experimented with the following example:

```
# Import TreeSE
library(mia)
data("GlobalPatterns", package = "mia")
tse <- GlobalPatterns

# Agglomerate TreeSE by Genus and filter by prevalence
tse_genus <- mergeFeaturesByPrevalence(tse,
                                       rank = "Genus",
                                       prevalence = 50/100)

# Add relabundance assay
tse_genus <- transformAssay(tse_genus, method = "relabundance")

# Add PCA
tse_genus <- scater::runPCA(tse_genus, assay.type = "counts")

# Add rowTree
tse_genus <- addTaxonomyTree(tse_genus)

# Launch iSEE
if (interactive()) {
  iSEE(tse_genus)
}
```

## Dependencies
iSEEtree implements the [miaViz](https://microbiome.github.io/miaViz/) package for microbiome data visualisation to create panels that are specific for TreeSummarizedExperiment objects. Not surprisingly, it also depends on the generic panels from iSEE.

## Contributions
If you come across a bug or see potential for improvement, feel free to open an issue and we will plan the next move. is missing or could be improved. If you are interested in contributing, you can check [our issues](https://github.com/RiboRings/iSEEtree/issues) and try to find a solution. Either way, contributions are very much appreciated.
