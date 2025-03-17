#' iSEEtree utils
#' 
#' Utility functions to check the existence of specific elements in a
#' \code{\link[TreeSummarizedExperiment:TreeSummarizedExperiment-constructor]{TreeSummarizedExperiment}}
#' that are compulsory when using certain panels.
#' 
#' @param se a
#' \code{\link[SummarizedExperiment:SummarizedExperiment-class]{SummarizedExperiment}}
#' object.
#' 
#' @param initial \code{Panel vector}. A list of panel objects to check.
#' 
#' @param panel.class \code{Character vector}. A list of panel names
#'   corresponding to panel objects in \code{initial}.
#' 
#' @param panel.fun \code{Function scalar}. The element of \code{se} whose
#'   existance should be checked.
#' 
#' @param wtext \code{Character scalar}. Text of the warning message returned
#'   if \code{panel.fun} does not exist or is empty.
#' 
#' @return
#' \code{.check_panel} returns the input \code{initial} list of panels excluding
#' the checked panel if \code{panel.fun} is \code{NULL} or empty.
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
#' initial <- .check_panel(tse, initial, "RowTreePlot", rowLinks)
#' # If ColumnTreePlot is in initial, check whether colLinks is defined
#' initial <- .check_panel(tse, initial, "ColumnTreePlot", colLinks)
#' 
#' # View filtered list of panels
#' initial
#' 
#' @keywords internal
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
.check_panel <- function(se, initial, panel.class, panel.fun, wtext) {
    
    no_keep <- unlist(lapply(initial, function(x) is(x, panel.class)))

    if( any(no_keep) && (is.null(panel.fun(se)) || isEmpty(panel.fun(se))) ){
        initial <- initial[!no_keep]
        warning("no valid ", as.character(substitute(panel.fun)),
            " fields for ", panel.class, call. = FALSE)
    }
    
    return(initial)
}