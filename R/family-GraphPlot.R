#' Graph plot
#'
#' The Graph plot is a virtual class that creates the network structure of
#' either the features or samples of a
#' \code{\link[TreeSummarizedExperiment:TreeSummarizedExperiment-constructor]{TreeSummarizedExperiment}}
#' object. The \linkS4class{RowGraphPlot} and \linkS4class{ColumnGraphPlot}
#' classes belong to this family and are specialised to visualise the feature
#' or sample graphs stored in metadata, respectively.
#'
#' @section Slot overview:
#' The following slots control the thresholds used in the visualisation:
#' \itemize{
#' \item \code{name}: \code{Character scalar}. Metadata name containing graph.
#'   (Default: \code{"graph"})
#' \item \code{assay.type}: \code{Character scalar}. Assay type to use.
#'   (Default: \code{"counts"})
#' \item \code{layout}: \code{Character scalar}. Graph layout.
#'   (Default: \code{"kk"})
#'  \item \code{edge.type}: \code{Character scalar}. Edge type.
#'   (Default: \code{"fan"})
#' \item \code{show.label}: \code{Logical scalar}. Should node labels be shown.
#'   (Default: \code{FALSE})
#' \item \code{add.legend}: \code{Logical scalar}. Should legend be shown.
#'   (Default: \code{TRUE})
#' \item \code{edge.colour.by}: \code{Character scalar}. Parameter to colour
#'   lines by when \code{colour_parameters = "Edge"}. (Default: \code{NULL})
#' \item \code{edge.size.by}: \code{Character scalar}. Parameter to size lines
#'   by when \code{size_parameters = "Edge"}. (Default: \code{NULL})
#' \item \code{node.colour.by}: \code{Character scalar}. Parameter to colour
#'   nodes by when \code{colour_parameters = "Node"}. (Default: \code{NULL})
#' \item \code{node.size.by}: \code{Character scalar}. Parameter to size nodes
#'   by when \code{size_parameters = "Node"}. (Default: \code{NULL})
#' \item \code{node.shape.by}: \code{Character scalar}. Parameter to shape nodes
#'   by when \code{shape_parameters = "Node"}. (Default: \code{NULL})
#' }
#'
#' In addition, this class inherits all slots from its parent class
#' \linkS4class{Panel}.
#'
#' @author Giulio Benedetti
#' 
#' @docType methods
#' @name GraphPlot
NULL

#' @importFrom S4Vectors setValidity2
setValidity2("GraphPlot", function(x) {
    msg <- character(0)
    
    msg <- .singleStringError(msg, x, fields=c("name", "assay.type", "layout",
        "edge.type", "edge.colour.by", "edge.size.by", "colour.by", "shape.by",
        "size.by"))
    msg <- .validLogicalError(msg, x, fields=c("add.legend", "show.label"))

    if (length(msg)) {
        return(msg)
    }
    
    TRUE
})

#' @importFrom methods callNextMethod
setMethod("initialize", "GraphPlot", function(.Object, ...) {
    args <- list(...)
    args <- .emptyDefault(args, "name", "graph")
    args <- .emptyDefault(args, "assay.type", "counts")
    args <- .emptyDefault(args, "layout", "kk")
    args <- .emptyDefault(args, "edge.type", "fan")
    args <- .emptyDefault(args, "show.label", FALSE)
    args <- .emptyDefault(args, "add.legend", TRUE)
    args <- .emptyDefault(args, "edge.colour.by", NA_character_)
    args <- .emptyDefault(args, "edge.size.by", NA_character_)
    args <- .emptyDefault(args, "colour.by", NA_character_)
    args <- .emptyDefault(args, "size.by", NA_character_)
    args <- .emptyDefault(args, "shape.by", NA_character_)

    do.call(callNextMethod, c(list(.Object), args))
})

#' @importFrom methods slot
#' @importFrom S4Vectors metadata
#' @importFrom SummarizedExperiment assayNames
setMethod(".defineDataInterface", "GraphPlot", function(x, se, select_info) {
  panel_name <- .getEncodedName(x)
  list(.selectInput.iSEE(x, field="name", label="Graph name:",
            choices=names(metadata(se)), selected=slot(x, "name")),
       .selectInput.iSEE(x, field="assay.type", label="Assay type:",
            choices=assayNames(se), selected=slot(x, "assay.type")),
       .checkboxInput.iSEE(x, field="show.label", label="Show node labels",
          value=slot(x, "show.label")))
})

#' @importFrom methods callNextMethod
setMethod(".defineInterface", "GraphPlot", function(x, se, select_info) {
    out <- callNextMethod()
    list(out[1], .create_visual_box_for_graph(x, se), out[-1])
})

setMethod(".createObservers", "GraphPlot",
    function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()
    panel_name <- .getEncodedName(x)

    .createProtectedParameterObservers(panel_name, c("layout", "assay.type",
        "name", "edge.type", "show.label", "add.legend", "RowSelectionSource"),
        input=input, pObjects=pObjects, rObjects=rObjects)
    
    .createUnprotectedParameterObservers(panel_name, c("edge.colour.by",
        "edge.size.by", "size.by", "shape.by", "size.by"), input=input,
        pObjects=pObjects, rObjects=rObjects)
    
    invisible(NULL)
})

#' @importFrom shiny plotOutput
#' @importFrom shinyWidgets addSpinner
setMethod(".defineOutput", "GraphPlot", function(x) {
    panel_name <- .getEncodedName(x)

    addSpinner(plotOutput(panel_name,
        height = paste0(slot(x, "PanelHeight"), "px")), color=.panelColor(x))
})

#' @importFrom shiny renderPlot
#' @importFrom methods callNextMethod
setMethod(".renderOutput", "GraphPlot",
    function(x, se, output, pObjects, rObjects) {

    panel_name <- .getEncodedName(x)
    force(se) # defensive programming to avoid bugs due to delayed evaluation
    
    output[[panel_name]] <- renderPlot({
        .retrieveOutput(panel_name, se, pObjects, rObjects)
    })
    
    callNextMethod()
})

#' @importFrom grDevices pdf dev.off
setMethod(".exportOutput", "GraphPlot",
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
setMethod(".definePanelTour", "GraphPlot", function(x) {
    rbind(c(paste0("#", .getEncodedName(x)), sprintf(
        "The <font color=\"%s\">%s</font> panel contains a network or graph
        from the 
        <i><a href='https://microbiome.github.io/miaViz/reference/plotGraph.html'>miaViz</a></i>
        package.", .getPanelColor(x), .fullName(x))),
    .addTourStep(x, "DataBoxOpen", "The <i>Data parameters</i> box shows the
        available parameters that can be tweaked to control the data on
        the graph.<br/><br/><strong>Action:</strong> click on this
        box to open up available options."),
    .addTourStep(x, "VisualBoxOpen", "The <i>Visual parameters</i> box shows
        the available visual parameters that can be tweaked in this
        tree.<br/><br/><strong>Action:</strong> click on this box to
        open up available options."),
    callNextMethod())
})

#' @importFrom SummarizedExperiment rowData colData
.create_visual_box_for_graph <- function(x, se) {
    panel_name <- .getEncodedName(x)
    gr_data <- switch(substr(panel_name, 1, 3),
        Row = rowData(se), Col = colData(se))

    .addSpecificTour(class(x)[1], "layout", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_layout + .selectize-control"), intro = "Here, we can select the
            layout of the network")))})
    .addSpecificTour(class(x)[1], "edge.type", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_edge\\.type + .selectize-control"), intro = "Here, we can select
            the edge type of the network")))})
    .addSpecificTour(class(x)[1], "add.legend", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_add\\.legend"), intro = "Here, we can choose
            whether or not to show a legend.")))})
    .addSpecificTour(class(x)[1], "edge.colour.by", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_edge\\.colour\\.by + .selectize-control"), intro = "Here, we can
            choose whether or not to colour the lines by a variable from the
            <code>metadata</code>. When active, the available options are listed
            and one of them can be selected.")))})
    .addSpecificTour(class(x)[1], "edge.size.by", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_edge\\.width\\.by + .selectize-control"), intro = "Here, we can
            choose whether or not to colour the tips by a variable from the
            <code>metadata</code>. When active, the available options are listed
            and one of them can be selected.")))})
    .addSpecificTour(class(x)[1], "colour.by", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_colour\\.by + .selectize-control"), intro = "Here, we can choose
            whether or not to colour the nodes by a variable from the
            <code>metadata</code>. When active, the available options are listed
            and one of them can be selected.")))})
    .addSpecificTour(class(x)[1], "shape.by", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_shape\\.by + .selectize-control"), intro = "Here, we can order
            the tree alphabetically.")))})
    .addSpecificTour(class(x)[1], "size.by", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_size\\.by + .selectize-control"), intro = "Here, we can
            choose how to colour the  tips by.")))})
    
    # Define what parameters the user can adjust
    collapseBox(paste0(panel_name, "_VisualBoxOpen"),
        title="Visual parameters", open=FALSE,
        # Graph layout
        .selectInput.iSEE(x, field="layout", label="Layout:",
            choices=c("auto", "fan", "igraph", "dendrogram", "linear", "matrix",
                "treemap", "circlepack", "partition", "hive", "cactustree",
                "backbone", "centrality", "eigen", "fabric", "focus", "pmds",
                "stress", "unrooted", "htree"), selected=slot(x, "layout")),
        .selectInput.iSEE(x, field="edge.type", label="Edge type:",
            choices=c("link", "arc", "parallel", "fan", "loop", "diagonal",
                "elbow", "bend", "hive", "span", "point", "tile", "density",
                "force", "path", "minimal", "sf"), selected=slot(x, "edge.type")),
        .checkboxInput.iSEE(x, field="add.legend", label="View legend",
            value=slot(x, "add.legend")),
        .selectInput.iSEE(x, field="edge.colour.by", label="Color edges by",
            choices=names(gr_data), selected=slot(x, "edge.colour.by")),
        .selectInput.iSEE(x, field="edge.size.by", label="Widen edges by",
            choices=names(gr_data), selected=slot(x, "edge.size.by")),
        .selectInput.iSEE(x, field="colour.by", label="Color nodes by",
            choices=names(gr_data), selected=slot(x, "colour.by")),
        .selectInput.iSEE(x, field="shape.by", label="Shape nodes by",
            choices=names(gr_data), selected=slot(x, "shape.by")),
        .selectInput.iSEE(x, field="size.by", label="Size edges by",
            choices=names(gr_data), selected=slot(x, "size.by")))

}