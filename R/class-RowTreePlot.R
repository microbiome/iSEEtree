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
#' The following slots control the thresholds used in the visualization:
#' \itemize{
#' \item \code{layout}, a string specifying tree layout
#' \item \code{add_legend}, a logical indicating if color legend should appear.
#' \item \code{edge_colour_by}, a string specifying parameter to color lines by
#'   when \code{colour_parameters = "Edge"}.
#' \item \code{edge_size_by}, a string specifying parameter to size lines by
#'   when \code{size_parameters = "Edge"}.
#' \item \code{tip_colour_by}, a string specifying parameter to color tips by
#'   when \code{colour_parameters = "Tip"}.
#' \item \code{tip_size_by}, a string specifying parameter to size tips by
#'   when \code{size_parameters = "Tip"}.
#' \item \code{tip_shape_by}, a string specifying parameter to shape tips by
#'   when \code{shape_parameters = "Tip"}.
#' \item \code{node_colour_by}, a string specifying parameter to color nodes by
#'   when \code{colour_parameters = "Node"}.
#' \item \code{node_size_by}, a string specifying parameter to size nodes by
#'   when \code{size_parameters = "Node"}.
#' \item \code{node_shape_by}, a string specifying parameter to shape nodes by
#'   when \code{shape_parameters = "Node"}.
#' \item \code{order_tree}, a logical indicating if tree is ordered by
#'   alphabetic order of taxonomic levels.
#' }
#'
#' In addition, this class inherits all slots from its parent \linkS4class{Panel} class.
#'
#' @return
#' The \code{RowTreePlot(...)} constructor creates an instance of a RowTreePlot
#' class, where any slot and its value can be passed to \code{...} as a named
#' argument.
#'
#' @author Giulio Benedetti
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
#' @docType methods
#' @name RowTreePlot
NULL

#' @rdname RowTreePlot
#' @export
setClass("RowTreePlot", contains="Panel", slots=c(layout="character",
    add_legend="logical", edge_colour_by="character", tip_colour_by="character",
    order_tree="logical", tip_size_by="character", edge_size_by="character",
    tip_shape_by="character", node_size_by="character", node_shape_by="character",
    node_colour_by="character", visual_parameters="character", 
    size_parameters="character", shape_parameters="character",
    colour_parameters="character"))

#' @importFrom iSEE .singleStringError .validLogicalError
#' @importFrom S4Vectors setValidity2
setValidity2("RowTreePlot", function(x) {
    msg <- character(0)
    
    msg <- .singleStringError(msg, x, fields=c("layout", "edge_colour_by",
        "tip_colour_by", "tip_size_by", "edge_size_by", "tip_shape_by",
        "node_colour_by", "node_size_by", "node_shape_by"))
    msg <- .validLogicalError(msg, x, fields=c("add_legend", "order_tree"))
    
    if (length(msg)) {
        return(msg)
    }
    
    TRUE
})

#' @importFrom iSEE .emptyDefault
#' @importFrom methods callNextMethod
setMethod("initialize", "RowTreePlot", function(.Object, ...) {
    args <- list(...)
    args <- .emptyDefault(args, "layout", "circular")
    args <- .emptyDefault(args, "add_legend", TRUE)
    args <- .emptyDefault(args, "edge_colour_by", NA_character_)
    args <- .emptyDefault(args, "edge_size_by", NA_character_)
    args <- .emptyDefault(args, "tip_colour_by", NA_character_)
    args <- .emptyDefault(args, "tip_size_by", NA_character_)
    args <- .emptyDefault(args, "tip_shape_by", NA_character_)
    args <- .emptyDefault(args, "node_colour_by", NA_character_)
    args <- .emptyDefault(args, "node_size_by", NA_character_)
    args <- .emptyDefault(args, "node_shape_by", NA_character_)
    args <- .emptyDefault(args, "visual_parameters", NA_character_)
    args <- .emptyDefault(args, "colour_parameters", NA_character_)
    args <- .emptyDefault(args, "shape_parameters", NA_character_)
    args <- .emptyDefault(args, "size_parameters", NA_character_)
    args <- .emptyDefault(args, "order_tree", FALSE)

    do.call(callNextMethod, c(list(.Object), args))
})

#' @export
#' @importFrom methods new
RowTreePlot <- function(...) {
    new("RowTreePlot", ...)
}

#' @importFrom iSEE .getEncodedName .checkboxInput.iSEE
#' @importFrom methods slot
setMethod(".defineDataInterface", "RowTreePlot", function(x, se, select_info) {
  panel_name <- .getEncodedName(x)

  list(.checkboxInput.iSEE(x, field="order_tree", label="Order tree",
                           value=slot(x, "order_tree")))
})

#' @importFrom methods callNextMethod
setMethod(".defineInterface", "RowTreePlot", function(x, se, select_info) {
    
    out <- callNextMethod()
    list(out[1], .create_visual_box_for_rowtree(x, se), out[-1])
})

#' @importFrom iSEE .getEncodedName .createProtectedParameterObservers
#'   .createUnprotectedParameterObservers
setMethod(".createObservers", "RowTreePlot",
    function(x, se, input, session, pObjects, rObjects) {
    
    callNextMethod()
    panel_name <- .getEncodedName(x)

    .createProtectedParameterObservers(panel_name, c("layout", "add_legend",
        "RowSelectionSource", "order_tree", "size_parameters", "visual_parameters",
        "shape_parameters", "colour_parameters"), input=input, pObjects=pObjects,
        rObjects=rObjects)
    
    .createUnprotectedParameterObservers(panel_name, c("edge_colour_by",
        "tip_colour_by", "tip_size_by", "tip_shape_by", "node_size_by",
        "node_shape_by", "node_colour_by", "edge_size_by"), input=input,
        pObjects=pObjects, rObjects=rObjects)
    
    invisible(NULL)
})

setMethod(".fullName", "RowTreePlot", function(x) "Row tree plot")

#' @importMethodsFrom iSEE .panelColor
setMethod(".panelColor", "RowTreePlot", function(x) "#4EEE94")

#' @importFrom iSEE .getEncodedName
#' @importFrom shiny plotOutput
#' @importFrom shinyWidgets addSpinner
setMethod(".defineOutput", "RowTreePlot", function(x) {
    panel_name <- .getEncodedName(x)
    
    addSpinner(plotOutput(panel_name,
        height = paste0(slot(x, "PanelHeight"), "px")), color=.panelColor(x))
})

#' @importFrom iSEE .processMultiSelections .textEval
#' @importFrom miaViz plotRowTree
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
    args[["add_legend"]] <- deparse(slot(x, "add_legend"))
    args[["order_tree"]] <- deparse(slot(x, "order_tree"))
    
    if( "Colour" %in% slot(x, "visual_parameters") ){
        args <- .assign_viz_param(args, x, "Edge", "colour")
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
    args <- paste(args, collapse=", ")
    fun_call <- sprintf("p <- miaViz::plotRowTree(se, %s)", args)

    fun_cmd <- paste(strwrap(fun_call, width = 80, exdent = 4), collapse = "\n")
    plot_out <- .textEval(fun_cmd, panel_env)
    all_cmds[["fun"]] <- fun_cmd

    list(commands=all_cmds, plot=plot_out, varname=NULL, contents=NULL)
})

#' @importFrom iSEE .getEncodedName .retrieveOutput
#' @importFrom shiny renderPlot
#' @importFrom methods callNextMethod
setMethod(".renderOutput", "RowTreePlot",
    function(x, se, output, pObjects, rObjects) {

    panel_name <- .getEncodedName(x)
    force(se) # defensive programming to avoid bugs due to delayed evaluation
    
    output[[panel_name]] <- renderPlot({
        .retrieveOutput(panel_name, se, pObjects, rObjects)
    })
    
    callNextMethod()
})

#' @importFrom grDevices pdf dev.off
setMethod(".exportOutput", "RowTreePlot",
    function(x, se, all_memory, all_contents) {
            
    contents <- .generateOutput(x, se, all_memory=all_memory,
        all_contents=all_contents)
    
    newpath <- paste0(.getEncodedName(x), ".pdf")
            
    pdf(newpath, width=slot(x, "PanelHeight") / 75,
        height=slot(x, "PanelWidth") * 2)
    
    print(contents$plot)
    dev.off()
            
    newpath
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
    function(x, dims = character(0)) {
    
    if( "row" %in% dims ){
        return(TRUE)
    }

    return(FALSE)
})

#' @importFrom methods callNextMethod
#' @importFrom iSEE .getEncodedName .getPanelColor .addTourStep
setMethod(".definePanelTour", "RowTreePlot", function(x) {
    rbind(c(paste0("#", .getEncodedName(x)), sprintf(
        "The <font color=\"%s\">RowTreePlot</font> panel contains a phylogenetic
        tree from the 
        <i><a href='https://microbiome.github.io/miaViz/reference/plotTree.html'>miaViz</a></i>
        package.", .getPanelColor(x))),
    .addTourStep(x, "DataBoxOpen", "The <i>Data parameters</i> box shows the
        available parameters that can be tweaked to control the data on
        the heatmap.<br/><br/><strong>Action:</strong> click on this
        box to open up available options."),
    .addTourStep(x, "VisualBoxOpen", "The <i>Visual parameters</i> box shows
        the available visual parameters that can be tweaked in this
        tree.<br/><br/><strong>Action:</strong> click on this box to
        open up available options."),
    callNextMethod())
})

#' @importFrom iSEE .getEncodedName .selectInput.iSEE .checkboxInput.iSEE
#'   .radioButtons.iSEE .conditionalOnRadio .addSpecificTour
#' @importFrom SummarizedExperiment rowData
#' @importFrom TreeSummarizedExperiment rowTreeNames
.create_visual_box_for_rowtree <- function(x, se) {
    panel_name <- .getEncodedName(x)
    .addSpecificTour(class(x)[1], "layout", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_layout + .selectize-control"), intro = "Here, we can select the
            layout of the tree.")))})
    .addSpecificTour(class(x)[1], "add_legend", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_add_legend"), intro = "Here, we can choose
            whether or not to show a legend.")))})
    .addSpecificTour(class(x)[1], "edge_colour", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_edge_colour"), intro = "Here, we can choose
            whether or not to colour the lines by a variable from the
            <code>rowData</code>. When active, the available options are listed
            and one of them can be selected.")))})
    .addSpecificTour(class(x)[1], "tip_colour", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_tip_colour"), intro = "Here, we can choose
            whether or not to colour the tips by a variable from the
            <code>rowData</code>. When active, the available options are listed
            and one of them can be selected.")))})
    .addSpecificTour(class(x)[1], "node_colour", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_node_colour"), intro = "Here, we can choose
            whether or not to colour the nodes by a variable from the
            <code>rowData</code>. When active, the available options are listed
            and one of them can be selected.")))})
    .addSpecificTour(class(x)[1], "order_tree", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_order_tree"), intro = "Here, we can order
            the tree alphabetically.")))})
    .addSpecificTour(class(x)[1], "tip_colour_by", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_tip_colour_by + .selectize-control"), intro = "Here, we can
            choose how to colour the  tips by.")))})
    .addSpecificTour(class(x)[1], "tip_size_by", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_tip_size_by + .selectize-control"), intro = "Here, we can 
            choose how to size the tree tips by.")))})
    .addSpecificTour(class(x)[1], "tip_shape_by", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_tip_shape_by + .selectize-control"), intro = "Here, we can 
            choose how to shape the tree tips by.")))})
    .addSpecificTour(class(x)[1], "edge_colour_by", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_edge_colour_by + .selectize-control"), intro = "Here, we can 
            choose how to colour the tree edges by.")))})
    .addSpecificTour(class(x)[1], "edge_size_by", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_edge_size_by + .selectize-control"), intro = "Here, we can 
            choose how to size the tree edges by.")))})
    .addSpecificTour(class(x)[1], "node_size_by", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_node_size_by + .selectize-control"), intro = "Here, we can 
            choose how to size the tree nodes by.")))})
    .addSpecificTour(class(x)[1], "node_shape_by", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_node_shape_by + .selectize-control"), intro = "Here, we can 
            choose how to shape the tree nodes by.")))})
    .addSpecificTour(class(x)[1], "node_colour_by", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
             "_node_colour_by + .selectize-control"), intro = "Here, we can 
            choose how to colour the tree nodes by.")))})
    .addSpecificTour(class(x)[1], "visual_parameters", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_visual_parameters"), intro = "Here, we can 
            choose to show the different visual parameters.")))})
    .addSpecificTour(class(x)[1], "colour_parameters", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_colour_parameters"), intro = "Here, we can make 
            the colour depend on the value of a
            categorical column data field for each plot components
            (line, tip, node).")))})
    .addSpecificTour(class(x)[1], "shape_parameters", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_shape_parameters"), intro = "Here, we can make 
            the shape depend on the value of a
            categorical column data field for each plot components
            (line, tip, node).")))})
    .addSpecificTour(class(x)[1], "size_parameters", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_size_parameters"), intro = "Here, we can make 
            the size depend on the value of a
            categorical column data field for each plot components
            (line, tip, node).")))})
    
    # Define what parameters the user can adjust
    collapseBox(paste0(panel_name, "_VisualBoxOpen"),
        title="Visual parameters", open=FALSE,
        # Tree layout
        .checkboxGroupInput.iSEE(x, field="visual_parameters", label=NULL,
            inline=TRUE, selected=slot(x, "visual_parameters"),
            choices=c("Colour", "Size", "Shape")),
        
        .conditionalOnCheckGroup(
            paste0(panel_name, "_visual_parameters"), "Colour",
            list(
                .checkboxGroupInput.iSEE(x, field="colour_parameters",
                    inline=TRUE, selected=slot(x, "colour_parameters"),
                    choices=c("Edge", "Node", "Tip"), label="Colour by:"),
                .conditionalOnCheckGroup(
                    paste0(panel_name, "_colour_parameters"), "Edge",
                        .selectInput.iSEE(x, field="edge_colour_by",
                            label="Color lines by", choices=names(rowData(se)),
                            selected=slot(x, "edge_colour_by"))),
                .conditionalOnCheckGroup(
                    paste0(panel_name, "_colour_parameters"), "Node",
                        .selectInput.iSEE(x, field="node_colour_by",
                            label="Color nodes by", choices=names(rowData(se)),
                            selected=slot(x, "node_colour_by"))),
                .conditionalOnCheckGroup(
                    paste0(panel_name, "_colour_parameters"), "Tip",
                        .selectInput.iSEE(x, field="tip_colour_by",
                            label="Color tips by", choices=names(rowData(se)),
                            selected=slot(x, "tip_colour_by"))))),
        
        .conditionalOnCheckGroup(
            paste0(panel_name, "_visual_parameters"), "Size",
            list(
                .checkboxGroupInput.iSEE(x, field="size_parameters",
                    inline=TRUE, selected=slot(x, "size_parameters"),
                    choices=c("Edge", "Node", "Tip"), label="Size by:"),
                .conditionalOnCheckGroup(
                    paste0(panel_name, "_size_parameters"), "Edge",
                        .selectInput.iSEE(x, field="edge_size_by",
                            label="Size lines by", choices=names(rowData(se)),
                            selected=slot(x, "edge_size_by"))),
                .conditionalOnCheckGroup(
                    paste0(panel_name, "_size_parameters"), "Node",
                        .selectInput.iSEE(x, field="node_size_by",
                            label="Size nodes by", choices=names(rowData(se)),
                            selected=slot(x, "node_size_by"))),
                .conditionalOnCheckGroup(
                    paste0(panel_name, "_size_parameters"), "Tip",
                        .selectInput.iSEE(x, field="tip_size_by",
                            label="Size tips by", choices=names(rowData(se)),
                            selected=slot(x, "tip_size_by"))))),
        
        .conditionalOnCheckGroup(
            paste0(panel_name, "_visual_parameters"), "Shape",
            list(
                .checkboxGroupInput.iSEE(x, field="shape_parameters",
                    inline=TRUE, selected=slot(x, "shape_parameters"),
                    choices=c("Node", "Tip"), label="Shape by:"),
                .conditionalOnCheckGroup(
                    paste0(panel_name, "_shape_parameters"), "Node",
                        .selectInput.iSEE(x, field="node_shape_by",
                            label="Shape nodes by", choices=names(rowData(se)),
                            selected=slot(x, "node_shape_by"))),
                .conditionalOnCheckGroup(
                    paste0(panel_name, "_shape_parameters"), "Tip",
                        .selectInput.iSEE(x, field="tip_shape_by",
                            label="Shape tips by", choices=names(rowData(se)),
                            selected=slot(x, "tip_shape_by"))))),
    
        .selectInput.iSEE(x, field="layout", label="Layout:",
            choices=c("circular", "rectangular", "slanted", "fan",
                "inward_circular", "radial", "unrooted", "equal_angle",
                "daylight", "dendrogram", "ape", "ellipse", "roundrect"),
                selected=slot(x, "layout")),
        # Colour legend
        .checkboxInput.iSEE(x, field="add_legend", label="View legend",
            value=slot(x, "add_legend")))
}

#' @importFrom methods slot
.assign_viz_param <- function(args, x, element, aesthetic) {
  
    param_name <- paste(tolower(element), aesthetic, "by", sep = "_")
  
    if( element %in% slot(x, paste(aesthetic, "parameters", sep = "_")) ){
        args[[param_name]] <- deparse(slot(x, param_name))
    }
  
    return(args)
}