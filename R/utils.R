#' iSEEtree utils
#' 
#' Utility functions to check the existence of specific elements in a
#' \code{\link[TreeSummarizedExperiment:TreeSummarizedExperiment-constructor]{TreeSummarizedExperiment}}
#' that are compulsory when using certain panels.
#' 
#' @return
#' \code{.check_panel} returns the input \code{initial} list of panels excluding
#' the checked panel if \code{panel_fun} is \code{NULL} or empty.
#' \code{.check_all_panels} applies \code{.check_panel} to multiple panels and
#' returns the a filtered version of \code{initial}.
#'
#' @examples
#' # Import libraries
#' library(mia)
#' library(TreeSummarizedExperiment)
#' 
#' # Import TreeSE
#' data("Tengeler2020", package = "mia")
#' tse <- Tengeler2020
#'
#' # Create list of panels
#' initial <- c(RowTreePlot(), ColumnTreePlot())
#' # If RowTreePlot is in initial, check whether rowLinks is defined
#' initial <- .check_panel(se, initial, "RowTreePlot", rowLinks)
#' # If ColumnTreePlot is in initial, check whether colLinks is defined
#' initial <- .check_panel(se, initial, "ColumnTreePlot", colLinks)
#' 
#' # View filtered list of panels
#' initial
#' 
#' @name utils
NULL

#' @rdname utils
#' @export
#' @importFrom TreeSummarizedExperiment TreeSummarizedExperiment rowLinks
#'   colLinks
#' @importFrom SingleCellExperiment reducedDims
#' @importFrom S4Vectors metadata
#' @importFrom SummarizedExperiment rowData colData
#' @importFrom mia taxonomyRanks
.check_all_panels <- function(se, initial){
    initial <- .check_panel(se, initial, "RowDataTable", rowData)
    initial <- .check_panel(se, initial, "ColumnDataTable", colData)
    initial <- .check_panel(se, initial, "RowTreePlot", rowLinks)
    initial <- .check_panel(se, initial, "ColumnTreePlot", colLinks)
    initial <- .check_panel(se, initial, "AbundancePlot", taxonomyRanks)
    initial <- .check_panel(se, initial, "ReducedDimensionPlot", reducedDims)
    initial <- .check_panel(se, initial, "LoadingPlot", reducedDims)
    initial <- .check_panel(se, initial, "ScreePlot", reducedDims)
    initial <- .check_panel(se, initial, "RDAPlot", reducedDims)
    initial <- .check_panel(se, initial, "RowGraphPlot", metadata)
    initial <- .check_panel(se, initial, "ColumnGraphPlot", metadata)
    return(initial)
}

#' @rdname utils
#' @export
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