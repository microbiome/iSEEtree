#' Row graph plot
#'
#' Hierarchical tree for the rows of a
#' \code{\link[TreeSummarizedExperiment:TreeSummarizedExperiment-constructor]{TreeSummarizedExperiment}}
#' object. The tree can be produced with \code{\link[mia:taxonomy-methods]{addTaxonomyTree}}
#' and gets stored in the \code{\link[TreeSummarizedExperiment:rowLinks]{rowTree}}
#' slot of the experiment object. The panel implements \code{\link[miaViz:plotTree]{plotRowTree}}
#' to generate the plot.
#'
#' @section Slot overview:
#'
#' This class inherits all slots from its parent class \linkS4class{GraphPlot}.
#'
#' @return
#' The \code{RowGraphPlot(...)} constructor creates an instance of a
#' RowGraphPlot class, where any slot and its value can be passed to \code{...}
#' as a named argument.
#'
#' @examples
#' # Import TreeSE
#' library(mia)
#' data("Tengeler2020", package = "mia")
#' tse <- Tengeler2020
#' 
#' # Store panel into object
#' panel <- RowGraphPlot()
#' # View some adjustable parameters
#' head(slotNames(panel))
#'
#' # Launch iSEE with custom initial panel
#' if (interactive()) {
#'   iSEE(tse, initial = c(panel))
#' }
#' 
#' @author Giulio Benedetti
#' 
#' @docType methods
#' @name RowGraphPlot
NULL

#' @export
#' @importFrom methods new
RowGraphPlot <- function(...) {
    new("RowGraphPlot", ...)
}

setMethod(".fullName", "RowGraphPlot", function(x) "Row graph plot")
setMethod(".panelColor", "RowGraphPlot", function(x) "orange")

#' @importFrom miaViz plotRowGraph
setMethod(".generateOutput", "RowGraphPlot",
    function(x, se, all_memory, all_contents) {
    
    panel_env <- new.env()
    all_cmds <- list()
    args <- character(0)

    all_cmds[["select"]] <- .processMultiSelections(
        x, all_memory, all_contents, panel_env
    )

    if( exists("row_selected", envir=panel_env, inherits=FALSE) ) {
        panel_env[["se"]] <- se[unlist(panel_env[["row_selected"]]), ]
    } else {
        panel_env[["se"]] <- se
    }
    
    args[["name"]] <- deparse(slot(x, "name"))
    args[["assay.type"]] <- deparse(slot(x, "assay.type"))
    args[["show.label"]] <- deparse(slot(x, "show.label"))
    args[["layout"]] <- deparse(slot(x, "layout"))
    args[["edge.type"]] <- deparse(slot(x, "edge.type"))
    args[["add.legend"]] <- deparse(slot(x, "add.legend"))

    args <- sprintf("%s=%s", names(args), args)
    args <- paste(args, collapse = ", ")
    fun_call <- sprintf("p <- plotRowGraph(se, %s)", args)

    fun_cmd <- paste(strwrap(fun_call, width = 80, exdent = 4), collapse = "\n")
    plot_out <- .textEval(fun_cmd, panel_env)
    all_cmds[["fun"]] <- fun_cmd

    list(commands=all_cmds, plot=plot_out, varname=NULL, contents=NULL)
})

#' @importFrom methods callNextMethod
setMethod(".hideInterface", "RowGraphPlot", function(x, field) {
    
    if( field %in% c("SelectionHistory", "ColumnSelectionRestrict",
        "ColumnSelectionDynamicSource", "ColumnSelectionSource") ){
        TRUE
    } else {
        callNextMethod()
    }
})

setMethod(".multiSelectionResponsive", "RowGraphPlot",
    function(x, dim = character(0)) {
    
    if( "row" %in% dim ){
        return(TRUE)
    }

    return(FALSE)
})
