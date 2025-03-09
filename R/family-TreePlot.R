#' Tree plot
#'
#' The TreePlot is a virtual class that creates a hierarchical tree of either
#' the features or samples of a
#' \code{\link[TreeSummarizedExperiment:TreeSummarizedExperiment-constructor]{TreeSummarizedExperiment}}
#' object. The \linkS4class{RowTreePlot} and \linkS4class{ColumnTreePlot}
#' classes belong to this family and are specialised to visualise the rowTree
#' and the colTree, respectively.
#'
#' @section Slot overview:
#' The following slots control the thresholds used in the visualisation:
#' \itemize{
#' \item \code{layout}: \code{Character scalar}. Tree layout.
#'   (Default: \code{"fan"})
#' \item \code{add.legend}: \code{Logical scalar}. Should legend be shown.
#'   (Default: \code{TRUE})
#' \item \code{edge.colour.by}: \code{Character scalar}. Parameter to colour
#'   lines by when \code{colour_parameters = "Edge"}. (Default: \code{NULL})
#' \item \code{edge.size.by}: \code{Character scalar}. Parameter to size lines
#'   by when \code{size_parameters = "Edge"}. (Default: \code{NULL})
#' \item \code{tip.colour.by}: \code{Character scalar}. Parameter to colour tips
#'   by when \code{colour_parameters = "Tip"}. (Default: \code{NULL})
#' \item \code{tip.size.by}: \code{Character scalar}. Parameter to size tips by
#'   when \code{size_parameters = "Tip"}. (Default: \code{NULL})
#' \item \code{tip.shape.by}: \code{Character scalar}. Parameter to shape tips
#'   by when \code{shape_parameters = "Tip"}. (Default: \code{NULL})
#' \item \code{node.colour.by}: \code{Character scalar}. Parameter to colour
#'   nodes by when \code{colour_parameters = "Node"}. (Default: \code{NULL})
#' \item \code{node.size.by}: \code{Character scalar}. Parameter to size nodes
#'   by when \code{size_parameters = "Node"}. (Default: \code{NULL})
#' \item \code{node.shape.by}: \code{Character scalar}. Parameter to shape nodes
#'   by when \code{shape_parameters = "Node"}. (Default: \code{NULL})
#' \item \code{order.tree}: \code{Logical scalar}. Should the tree be ordered
#'   alphabetically by the taxonomic levels. (Default: \code{FALSE})
#' \item \code{open.angle}: \code{Numeric scalar}. Angle by which the tree is
#'   opened when \code{layout} is \code{"fan"}. (Default: \code{0})
#' \item \code{rotate.angle}: \code{Numeric scalar}. Angle by which the tree is
#'   rotated. (Default: \code{0})
#' \item \code{branch.length}: \code{Logical scalar}. Should branch length be
#'   equalised. (Default: \code{FALSE})
#' }
#'
#' In addition, this class inherits all slots from its parent
#' \linkS4class{Panel} class.
#' 
#' @seealso
#' \linkS4class{RowTreePlot}
#' \linkS4class{ColumnTreePlot}
#' 
#' @author Giulio Benedetti
#' 
#' @docType methods
#' @name TreePlot
NULL

#' @importFrom S4Vectors setValidity2
setValidity2("TreePlot", function(x) {
    msg <- character(0)
    
    msg <- .singleStringError(msg, x, fields=c("layout", "edge.colour.by",
        "tip.colour.by", "tip.size.by", "edge.size.by", "tip.shape.by",
        "node.colour.by", "node.size.by", "node.shape.by"))
    msg <- .validLogicalError(msg, x, fields=c("add.legend", "order.tree",
        "branch.length", "add.node.lab", "add.tip.lab"))
    msg <- .validNumberError(msg, x, "open.angle", lower=0, upper=360)
    msg <- .validNumberError(msg, x, "rotate.angle", lower=0, upper=360)

    if (length(msg)) {
        return(msg)
    }
    
    TRUE
})

#' @importFrom methods callNextMethod
setMethod("initialize", "TreePlot", function(.Object, ...) {
    args <- list(...)
    args <- .emptyDefault(args, "layout", "fan")
    args <- .emptyDefault(args, "add.legend", TRUE)
    args <- .emptyDefault(args, "edge.colour.by", NA_character_)
    args <- .emptyDefault(args, "edge.size.by", NA_character_)
    args <- .emptyDefault(args, "tip.colour.by", NA_character_)
    args <- .emptyDefault(args, "tip.size.by", NA_character_)
    args <- .emptyDefault(args, "tip.shape.by", NA_character_)
    args <- .emptyDefault(args, "node.colour.by", NA_character_)
    args <- .emptyDefault(args, "node.size.by", NA_character_)
    args <- .emptyDefault(args, "node.shape.by", NA_character_)
    args <- .emptyDefault(args, "visual_parameters", NA_character_)
    args <- .emptyDefault(args, "colour_parameters", NA_character_)
    args <- .emptyDefault(args, "shape_parameters", NA_character_)
    args <- .emptyDefault(args, "size_parameters", NA_character_)
    args <- .emptyDefault(args, "order.tree", FALSE)
    args <- .emptyDefault(args, "collapse", NA_character_)
    args <- .emptyDefault(args, "add.node.lab", FALSE)
    args <- .emptyDefault(args, "add.tip.lab", FALSE)
    args <- .emptyDefault(args, "open.angle", 0)
    args <- .emptyDefault(args, "rotate.angle", 0)
    args <- .emptyDefault(args, "branch.length", FALSE)

    do.call(callNextMethod, c(list(.Object), args))
})

#' @importFrom methods slot
setMethod(".defineDataInterface", "TreePlot", function(x, se, select_info) {
  panel_name <- .getEncodedName(x)
  list(.checkboxInput.iSEE(x, field="add.node.lab", label="Show node labels",
          value=slot(x, "add.node.lab")),
       .checkboxInput.iSEE(x, field="add.tip.lab", label="Show tip labels",
          value=slot(x, "add.tip.lab")),
       .sliderInput.iSEE(x, field="open.angle", label="Open by angle:",
          min=0, max=360, step=1, value=slot(x, "open.angle")),
       .sliderInput.iSEE(x, field="rotate.angle", label="Rotate by angle:",
          min=0, max=360, step=1, value=slot(x, "rotate.angle")),
       .checkboxInput.iSEE(x, field="order.tree", label="Order tree",
          value=slot(x, "order.tree")),
       .checkboxInput.iSEE(x, field="branch.length", label="Equalise length",
          value=slot(x, "branch.length")))
})

#' @importFrom methods callNextMethod
setMethod(".defineInterface", "TreePlot", function(x, se, select_info) {
    out <- callNextMethod()
    list(out[1], .create_visual_box_for_tree(x, se), out[-1])
})

setMethod(".createObservers", "TreePlot",
    function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()
    panel_name <- .getEncodedName(x)

    .createProtectedParameterObservers(panel_name, c("layout", "add.legend",
        "RowSelectionSource", "order.tree", "size_parameters", "visual_parameters",
        "shape_parameters", "colour_parameters", "open.angle", "rotate.angle",
        "branch.length", "collapse"), input=input, pObjects=pObjects,
        rObjects=rObjects)
    
    .createUnprotectedParameterObservers(panel_name, c("edge.colour.by",
        "tip.colour.by", "tip.size.by", "tip.shape.by", "node.size.by",
        "node.shape.by", "node.colour.by", "edge.size.by", "add.node.lab",
        "add.tip.lab"), input=input, pObjects=pObjects, rObjects=rObjects)
    
    invisible(NULL)
})

#' @importFrom shiny plotOutput
#' @importFrom shinyWidgets addSpinner
setMethod(".defineOutput", "TreePlot", function(x) {
    panel_name <- .getEncodedName(x)

    addSpinner(plotOutput(panel_name,
        height = paste0(slot(x, "PanelHeight"), "px")), color=.panelColor(x))
})

#' @importFrom shiny renderPlot
#' @importFrom methods callNextMethod
setMethod(".renderOutput", "TreePlot",
    function(x, se, output, pObjects, rObjects) {

    panel_name <- .getEncodedName(x)
    force(se) # defensive programming to avoid bugs due to delayed evaluation
    
    output[[panel_name]] <- renderPlot({
        .retrieveOutput(panel_name, se, pObjects, rObjects)
    })
    
    callNextMethod()
})

#' @importFrom grDevices pdf dev.off
setMethod(".exportOutput", "TreePlot",
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
setMethod(".definePanelTour", "TreePlot", function(x) {
    rbind(c(paste0("#", .getEncodedName(x)), sprintf(
        "The <font color=\"%s\">%s</font> panel contains a phylogenetic
        tree from the 
        <i><a href='https://microbiome.github.io/miaViz/reference/plotTree.html'>miaViz</a></i>
        package.", .getPanelColor(x), .fullName(x))),
    .addTourStep(x, "DataBoxOpen", "The <i>Data parameters</i> box shows the
        available parameters that can be tweaked to control the data on
        the tree.<br/><br/><strong>Action:</strong> click on this
        box to open up available options."),
    .addTourStep(x, "VisualBoxOpen", "The <i>Visual parameters</i> box shows
        the available visual parameters that can be tweaked in this
        tree.<br/><br/><strong>Action:</strong> click on this box to
        open up available options."),
    callNextMethod())
})

#' @importFrom SummarizedExperiment rowData colData
.create_visual_box_for_tree <- function(x, se) {
    panel_name <- .getEncodedName(x)
    tr_data <- switch(substr(panel_name, 1, 3),
        Row = rowData(se), Col = colData(se))
    
    .addSpecificTour(class(x)[1], "layout", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_layout + .selectize-control"), intro = "Here, we can select the
            layout of the tree.")))})
    .addSpecificTour(class(x)[1], "add.legend", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_add\\.legend"), intro = "Here, we can choose
            whether or not to show a legend.")))})
    .addSpecificTour(class(x)[1], "edge_colour", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_edge_colour"), intro = "Here, we can choose
            whether or not to colour the lines by a variable from the
            <code>metadata</code>. When active, the available options are listed
            and one of them can be selected.")))})
    .addSpecificTour(class(x)[1], "tip_colour", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_tip_colour"), intro = "Here, we can choose
            whether or not to colour the tips by a variable from the
            <code>metadata</code>. When active, the available options are listed
            and one of them can be selected.")))})
    .addSpecificTour(class(x)[1], "node_colour", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_node_colour"), intro = "Here, we can choose
            whether or not to colour the nodes by a variable from the
            <code>metadata</code>. When active, the available options are listed
            and one of them can be selected.")))})
    .addSpecificTour(class(x)[1], "order.tree", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_order\\.tree"), intro = "Here, we can order
            the tree alphabetically.")))})
    .addSpecificTour(class(x)[1], "tip.colour.by", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_tip\\.colour\\.by + .selectize-control"), intro = "Here, we can
            choose how to colour the  tips by.")))})
    .addSpecificTour(class(x)[1], "tip.size.by", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_tip\\.size\\.by + .selectize-control"), intro = "Here, we can 
            choose how to size the tree tips by.")))})
    .addSpecificTour(class(x)[1], "tip.shape.by", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_tip\\.shape\\.by + .selectize-control"), intro = "Here, we can 
            choose how to shape the tree tips by.")))})
    .addSpecificTour(class(x)[1], "edge.colour.by", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_edge\\.colour\\.by + .selectize-control"), intro = "Here, we can 
            choose how to colour the tree edges by.")))})
    .addSpecificTour(class(x)[1], "edge.size.by", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_edge\\.size\\.by + .selectize-control"), intro = "Here, we can 
            choose how to size the tree edges by.")))})
    .addSpecificTour(class(x)[1], "node.size.by", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_node\\.size\\.by + .selectize-control"), intro = "Here, we can 
            choose how to size the tree nodes by.")))})
    .addSpecificTour(class(x)[1], "node.shape.by", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_node\\.shape\\.by + .selectize-control"), intro = "Here, we can 
            choose how to shape the tree nodes by.")))})
    .addSpecificTour(class(x)[1], "node.colour.by", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
             "_node\\.colour\\.by + .selectize-control"), intro = "Here, we can 
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
        .selectInput.iSEE(x, field="layout", label="Layout:",
            choices=c("fan", "rectangular", "circular", "slanted",
                "inward_circular", "radial", "unrooted", "equal_angle",
                "daylight", "dendrogram", "ape", "ellipse", "roundrect"),
                selected=slot(x, "layout")),
        .checkboxInput.iSEE(x, field="add.legend", label="View legend",
            value=slot(x, "add.legend")),
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
                        .selectInput.iSEE(x, field="edge.colour.by",
                            label="Colour lines by", choices=names(tr_data),
                            selected=slot(x, "edge.colour.by"))),
                .conditionalOnCheckGroup(
                    paste0(panel_name, "_colour_parameters"), "Node",
                        .selectInput.iSEE(x, field="node.colour.by",
                            label="Colour nodes by", choices=names(tr_data),
                            selected=slot(x, "node.colour.by"))),
                .conditionalOnCheckGroup(
                    paste0(panel_name, "_colour_parameters"), "Tip",
                        .selectInput.iSEE(x, field="tip.colour.by",
                            label="Colour tips by", choices=names(tr_data),
                            selected=slot(x, "tip.colour.by"))))),
        
        .conditionalOnCheckGroup(
            paste0(panel_name, "_visual_parameters"), "Size",
            list(
                .checkboxGroupInput.iSEE(x, field="size_parameters",
                    inline=TRUE, selected=slot(x, "size_parameters"),
                    choices=c("Edge", "Node", "Tip"), label="Size by:"),
                .conditionalOnCheckGroup(
                    paste0(panel_name, "_size_parameters"), "Edge",
                        .selectInput.iSEE(x, field="edge.size.by",
                            label="Size lines by", choices=names(tr_data),
                            selected=slot(x, "edge.size.by"))),
                .conditionalOnCheckGroup(
                    paste0(panel_name, "_size_parameters"), "Node",
                        .selectInput.iSEE(x, field="node.size.by",
                            label="Size nodes by", choices=names(tr_data),
                            selected=slot(x, "node.size.by"))),
                .conditionalOnCheckGroup(
                    paste0(panel_name, "_size_parameters"), "Tip",
                        .selectInput.iSEE(x, field="tip.size.by",
                            label="Size tips by", choices=names(tr_data),
                            selected=slot(x, "tip.size.by"))))),
        
        .conditionalOnCheckGroup(
            paste0(panel_name, "_visual_parameters"), "Shape",
            list(
                .checkboxGroupInput.iSEE(x, field="shape_parameters",
                    inline=TRUE, selected=slot(x, "shape_parameters"),
                    choices=c("Node", "Tip"), label="Shape by:"),
                .conditionalOnCheckGroup(
                    paste0(panel_name, "_shape_parameters"), "Node",
                        .selectInput.iSEE(x, field="node.shape.by",
                            label="Shape nodes by", choices=names(tr_data),
                            selected=slot(x, "node.shape.by"))),
                .conditionalOnCheckGroup(
                    paste0(panel_name, "_shape_parameters"), "Tip",
                        .selectInput.iSEE(x, field="tip.shape.by",
                            label="Shape tips by", choices=names(tr_data),
                            selected=slot(x, "tip.shape.by"))))))
}

#' @importFrom methods slot
.assign_viz_param <- function(args, x, element, aesthetic) {
  
    param_name <- paste(tolower(element), aesthetic, "by", sep = ".")
    
    if( element %in% slot(x, paste(aesthetic, "parameters", sep = "_")) ){
        args[[param_name]] <- deparse(slot(x, param_name))
    }
  
    return(args)
}