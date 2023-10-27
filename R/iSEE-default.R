#' @importFrom TreeSummarizedExperiment TreeSummarizedExperiment rowLinks
#' @importFrom SingleCellExperiment reducedDims
setMethod("iSEE", "TreeSummarizedExperiment",
  function(se, initial = c(RowTreePlot(), ReducedDimensionPlot(), ComplexHeatmapPlot()),
           extra = NULL, colormap = ExperimentColorMap(), landingPage = createLandingPage(),
           tour = NULL, appTitle = NULL, runLocal = TRUE, voice = FALSE,
           bugs = FALSE, saveState = NULL,...) {
    
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
