#' Column graph plot
#'
#' Network organisation for the samples of a
#' \code{\link[SummarizedExperiment:SummarizedExperiment-class]{SummarizedExperiment}}
#' object. The igraph should be stored in the \code{metadata} slot by a name
#' containing \code{"graph"}. This panel uses
#' \code{\link[miaViz:plotColGraph]{plotColGraph}}
#' to generate the plot.
#'
#' @section Slot overview:
#'
#' This class inherits all slots from its parent class \linkS4class{GraphPlot}.
#'
#' @return
#' The \code{ColumnGraphPlot(...)} constructor creates an instance of a
#' ColumnGraphPlot class, where any slot and its value can be passed to
#' \code{...} as a named argument.
#'
#' @examples
#' library(mia)
#' library(miaViz)
#' data("GlobalPatterns", library = "mia")
#' data("col_graph", library = "miaViz")
#' 
#' tse <- GlobalPatterns
#' tse <- agglomerateByRank(tse,
#'                          rank = "Genus",
#'                          na.rm = TRUE)
#'                          
#' metadata(tse)$graph <- col_graph
#' 
#' # Store panel into object
#' panel <- ColumnGraphPlot()
#' # View some adjustable parameters
#' head(slotNames(panel))
#'
#' # Launch iSEE with custom initial panel
#' if (interactive()) {
#'   iSEE(tse, initial = c(panel))
#' }
#' 
#' @seealso
#' \linkS4class{GraphPlot}
#' \linkS4class{RowGraphPlot}
#' 
#' @author Giulio Benedetti
#' 
#' @docType methods
#' @name ColumnGraphPlot
NULL

#' @export
#' @importFrom methods new
ColumnGraphPlot <- function(...) {
    new("ColumnGraphPlot", ...)
}

setMethod(".fullName", "ColumnGraphPlot", function(x) "Column graph plot")
setMethod(".panelColor", "ColumnGraphPlot", function(x) "purple")

#' @importFrom miaViz plotColGraph
setMethod(".generateOutput", "ColumnGraphPlot",
    function(x, se, all_memory, all_contents) {
    
    panel_env <- new.env()
    all_cmds <- list()
    args <- character(0)

    all_cmds[["select"]] <- .processMultiSelections(
        x, all_memory, all_contents, panel_env
    )

    if( exists("col_selected", envir=panel_env, inherits=FALSE) ) {
        panel_env[["se"]] <- se[unlist(panel_env[["col_selected"]]), ]
    } else {
        panel_env[["se"]] <- se
    }
    
    args[["name"]] <- deparse(slot(x, "name"))
    args[["assay.type"]] <- deparse(slot(x, "assay.type"))
    args[["show.label"]] <- deparse(slot(x, "show.label"))
    args[["layout"]] <- deparse(slot(x, "layout"))
    args[["edge.type"]] <- deparse(slot(x, "edge.type"))
    args[["add.legend"]] <- deparse(slot(x, "add.legend"))
    
    if( "Colour" %in% slot(x, "visual_parameters") ){
        args <- .assign_viz_param(args, x, "Edge", "colour")
        args <- .assign_viz_param(args, x, "Node", "colour",
            arg.name = "colour.by")
    }
    
    if( "Shape" %in% slot(x, "visual_parameters") ){
        args <- .assign_viz_param(args, x, "Node", "shape",
            arg.name = "shape.by")
    }
    
    if( "Size" %in% slot(x, "visual_parameters") ){
        args <- .assign_viz_param(args, x, "Edge", "size",
            arg.name = "edge.width.by")
        args <- .assign_viz_param(args, x, "Node", "size", arg.name = "size.by")
    }
    
    args <- sprintf("%s=%s", names(args), args)
    args <- paste(args, collapse = ", ")
    fun_call <- sprintf("p <- miaViz::plotColGraph(se, %s)", args)

    fun_cmd <- paste(strwrap(fun_call, width = 80, exdent = 4), collapse = "\n")
    plot_out <- .textEval(fun_cmd, panel_env)
    all_cmds[["fun"]] <- fun_cmd

    list(commands=all_cmds, plot=plot_out, varname=NULL, contents=NULL)
})

#' @importFrom methods callNextMethod
setMethod(".hideInterface", "ColumnGraphPlot", function(x, field) {
    
    if( field %in% c("SelectionHistory", "ColumnSelectionRestrict",
        "ColumnSelectionDynamicSource", "ColumnSelectionSource") ){
        TRUE
    } else {
        callNextMethod()
    }
})

setMethod(".multiSelectionResponsive", "ColumnGraphPlot",
    function(x, dim = character(0)) {
    
    if( "column" %in% dim ){
        return(TRUE)
    }

    return(FALSE)
})