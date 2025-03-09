#' Row tree plot
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
#' This class inherits all slots from its parent class \linkS4class{TreePlot}.
#'
#' @return
#' The \code{RowTreePlot(...)} constructor creates an instance of a RowTreePlot
#' class, where any slot and its value can be passed to \code{...} as a named
#' argument.
#'
#' @examples
#' # Import TreeSE
#' library(mia)
#' data("Tengeler2020", package = "mia")
#' tse <- Tengeler2020
#' 
#' # Store panel into object
#' panel <- RowTreePlot()
#' # View some adjustable parameters
#' head(slotNames(panel))
#'
#' # Launch iSEE with custom initial panel
#' if (interactive()) {
#'   iSEE(tse, initial = c(panel))
#' }
#' 
#' @seealso
#' \linkS4class{TreePlot}
#' \linkS4class{ColumnTreePlot}
#' 
#' @author Giulio Benedetti
#' 
#' @docType methods
#' @name RowTreePlot
NULL

#' @export
#' @importFrom methods new
RowTreePlot <- function(...) {
    new("RowTreePlot", ...)
}

setMethod(".fullName", "RowTreePlot", function(x) "Row tree plot")
setMethod(".panelColor", "RowTreePlot", function(x) "#4EEE94")

#' @importFrom methods slot callNextMethod
#' @importFrom TreeSummarizedExperiment rowTree
#' @importFrom ape Ntip
setMethod(".defineDataInterface", "RowTreePlot", function(x, se, select_info) {
  panel_name <- .getEncodedName(x)
  out <- callNextMethod()
  
  list(.selectInput.iSEE(x, field="collapse", label="Collapse nodes:",
          choices=seq_len(rowTree(se)$Nnode) + Ntip(rowTree(se)),
          multiple = TRUE, selected=slot(x, "collapse")), out)
})

#' @importFrom miaViz plotRowTree
#' @importFrom ggtree geom_tiplab collapse rotate_tree
#' @importFrom ggplot2 geom_text aes
#' @importFrom purrr reduce
setMethod(".generateOutput", "RowTreePlot",
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
    
    args[["layout"]] <- deparse(slot(x, "layout"))
    args[["add.legend"]] <- deparse(slot(x, "add.legend"))
    args[["order.tree"]] <- deparse(slot(x, "order.tree"))
    args[["open.angle"]] <- deparse(slot(x, "open.angle"))
    
    if( slot(x, "branch.length") ){
        args[["branch.length"]] <- deparse("none")
    }
    
    if( "Colour" %in% slot(x, "visual_parameters") ){
        args <- .assign_viz_param(args, x, "Edge", "colour")
        print(args)
        args <- .assign_viz_param(args, x, "Node", "colour")
        args <- .assign_viz_param(args, x, "Tip", "colour")
    }
    
    if( "Shape" %in% slot(x, "visual_parameters") ){
        args <- .assign_viz_param(args, x, "Node", "shape")
        args <- .assign_viz_param(args, x, "Tip", "shape")
    }
    
    if( "Size" %in% slot(x, "visual_parameters") ){
        args <- .assign_viz_param(args, x, "Edge", "size")
        args <- .assign_viz_param(args, x, "Node", "size")
        args <- .assign_viz_param(args, x, "Tip", "size")
    }
  
    args <- sprintf("%s=%s", names(args), args)
    args <- paste(args, collapse = ", ")
    fun_call <- sprintf("p <- miaViz::plotRowTree(se, %s)", args)
    
    rotate_angle <- deparse(slot(x, "rotate.angle"))
    if( slot(x, "layout") != "rectangular" ){
        fun_call <- paste0(fun_call,
            sprintf("; p <- ggtree::rotate_tree(p, angle=%s)", rotate_angle))
    }

    if( slot(x, "add.tip.lab") ){
        fun_call <- paste0(fun_call, "; p <- p + geom_tiplab(size = 1)")
    }
    if( slot(x, "add.node.lab") ){
        fun_call <- paste0(fun_call,
            "; p <- p + geom_text(ggplot2::aes(label = node), hjust = -0.3, size = 2)")
    }
    
    nodes <- paste(slot(x, "collapse"), collapse = ", ")
    if( nodes != "NA" ){
        fun_call <- paste0(fun_call,
            sprintf(
                "; reduce(c(%s), function(x, y) collapse(x, y), .init = p)",
                nodes))
    }

    fun_cmd <- paste(strwrap(fun_call, width = 80, exdent = 4), collapse = "\n")
    print(fun_cmd)
    plot_out <- .textEval(fun_cmd, panel_env)
    all_cmds[["fun"]] <- fun_cmd

    list(commands=all_cmds, plot=plot_out, varname=NULL, contents=NULL)
})

#' @importFrom methods callNextMethod
setMethod(".hideInterface", "RowTreePlot", function(x, field) {
    
    if( field %in% c("SelectionHistory", "ColumnSelectionRestrict",
        "ColumnSelectionDynamicSource", "ColumnSelectionSource") ){
        TRUE
    } else {
        callNextMethod()
    }
})

setMethod(".multiSelectionResponsive", "RowTreePlot",
    function(x, dim = character(0)) {
    
    if( "row" %in% dim ){
        return(TRUE)
    }

    return(FALSE)
})
