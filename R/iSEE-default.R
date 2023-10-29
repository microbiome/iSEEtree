#' iSEE layout for TreeSE
#' 
#' Panel configuration tuned to the specific properties of
#' \code{\link[TreeSummarizedExperiment:TreeSummarizedExperiment-constructor]{TreeSummarizedExperiment}}.
#' 
#' @section Default layout: 
#' 
#' The default configuration can be overwritten by defining a different set of
#' initial panels. By default, the visualized panels include the following:
#' \itemize{
#' \item \code{RowTreePlot()}
#' \item \code{AbundancePlot()}
#' \item \code{AbundanceDensityPlot()}
#' \item \code{ReducedDimensionPlot()}
#' \item \code{ComplexHeatmapPlot()}
#' }
#'
#' @examples
#' # Import TreeSE
#' library(mia)
#' data("GlobalPatterns", package = "mia")
#' tse <- GlobalPatterns
#'
#' # Agglomerate TreeSE by Genus and filter by prevalence
#' tse_genus <- mergeFeaturesByPrevalence(tse,
#'                                        rank = "Genus",
#'                                        prevalence = 50/100)
#'
#' # Add relabundance assay
#' tse_genus <- transformAssay(tse_genus, method = "relabundance")
#'
#' # Add rowTree
#' tse_genus <- addTaxonomyTree(tse_genus)
#'
#' # Launch iSEE with custom initial panels
#' if (interactive()) {
#'   iSEE(tse_genus, initial = c(RowTreePlot(), AbundancePlot(), AbundanceDensityPlot()))
#' }
#' 
#' @name iSEE-TreeSE
NULL

#' @export
setGeneric("iSEE", iSEE::iSEE)

#' @export
#' @importFrom TreeSummarizedExperiment TreeSummarizedExperiment rowLinks
#' @importFrom SingleCellExperiment reducedDims
setMethod("iSEE", "TreeSummarizedExperiment",
  function(se, initial = c(RowTreePlot(), AbundancePlot(), AbundanceDensityPlot(),
                           ReducedDimensionPlot(), ComplexHeatmapPlot()),
           extra = NULL, colormap = ExperimentColorMap(), landingPage = createLandingPage(),
           tour = NULL, appTitle = NULL, runLocal = TRUE, voice = FALSE,
           bugs = FALSE, saveState = NULL, ...) {
    
    initial <- .check_panel(se, initial, "RowTreePlot", rowLinks)
    initial <- .check_panel(se, initial, "ReducedDimensionPlot", reducedDims)

    iSEE::iSEE(se, initial = initial, extra = extra,
               colormap = colormap,
               landingPage = landingPage,
               tour = tour,
               appTitle = appTitle,
               runLocal = runLocal,
               voice = voice,
               bugs = bugs,
               saveState = saveState,...)
})

.check_panel <- function(se, panel_list, panel_class, panel_fun) {
  no_keep <- .is_class_present(panel_list, panel_class)
  if (any(no_keep) && isEmpty(panel_fun(se))) {
    panel_list <- panel_list[!no_keep]
  }
  return(panel_list)
}

.is_class_present <- function(x, panel_class) sapply(x, function(y) is(y, panel_class))
