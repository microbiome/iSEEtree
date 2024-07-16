#' iSEE layout for TreeSE
#' 
#' Panel configuration tuned to the specific properties of
#' \code{\link[TreeSummarizedExperiment:TreeSummarizedExperiment-constructor]{TreeSummarizedExperiment}}.
#' 
#' @return
#' The iSEE method for the TreeSE container returns a default set of panels
#' typically relevant for microbiome data. This configuration can be modified
#' by defining a different set of initial panels. By default, the visualised
#' panels include the following:
#' \itemize{
#' \item \code{RowDataTable()}
#' \item \code{ColumnDataTable()}
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
#' # Agglomerate TreeSE by Genus
#' tse_genus <- agglomerateByRank(tse,
#'                                rank = "Genus",
#'                                onRankOnly = TRUE)
#'
#' # Add relabundance assay
#' tse_genus <- transformAssay(tse_genus, method = "relabundance")
#'
#' # Launch iSEE with custom initial panels
#' if (interactive()) {
#'   iSEE(tse_genus, initial = c(RowTreePlot(), AbundancePlot(), AbundanceDensityPlot()))
#' }
#' 
#' @docType methods
#' @aliases iSEE
#'   iSEE,TreeSummarizedExperiment-method
#'
#' @name iSEE-default
NULL

#' @export
setGeneric("iSEE", iSEE::iSEE)

#' @export
#' @importFrom iSEE createLandingPage ExperimentColorMap ReducedDimensionPlot
#'   ComplexHeatmapPlot RowDataTable ColumnDataTable
#' @importFrom TreeSummarizedExperiment TreeSummarizedExperiment rowLinks
#' @importFrom SingleCellExperiment reducedDims
#' @importFrom SummarizedExperiment rowData colData
#' @importFrom mia taxonomyRanks
setMethod("iSEE", "TreeSummarizedExperiment",
    function(se, initial = c(RowDataTable(), ColumnDataTable(), RowTreePlot(),
        AbundancePlot(), AbundanceDensityPlot(), ReducedDimensionPlot(),
        ComplexHeatmapPlot()), extra = NULL, colormap = ExperimentColorMap(),
        landingPage = createLandingPage(), tour = NULL, appTitle = NULL,
        runLocal = TRUE, voice = FALSE, bugs = FALSE, saveState = NULL, ...) {
    
    initial <- .check_panel(se, initial, "RowDataTable", rowData)
    initial <- .check_panel(se, initial, "ColumnDataTable", colData)
    initial <- .check_panel(se, initial, "RowTreePlot", rowLinks)
    initial <- .check_panel(se, initial, "AbundancePlot", taxonomyRanks)
    initial <- .check_panel(se, initial, "ReducedDimensionPlot", reducedDims)

    iSEE::iSEE(se, initial = initial, extra = initial, colormap = colormap,
        landingPage = landingPage, tour = tour, appTitle = appTitle,
        runLocal = runLocal, voice = voice, bugs = bugs,
        saveState = saveState, ...)
})

#' @importFrom S4Vectors isEmpty
#' @importFrom methods is
.check_panel <- function(se, panel_list, panel_class, panel_fun, wtext) {
    
    no_keep <- unlist(lapply(panel_list, function(x) is(x, panel_class)))

    if( any(no_keep) && (is.null(panel_fun(se)) || isEmpty(panel_fun(se))) ){
        panel_list <- panel_list[!no_keep]
        warning("no valid ", as.character(substitute(panel_fun)),
            " fields for ", panel_class, call. = FALSE)
    }
    
    return(panel_list)
}