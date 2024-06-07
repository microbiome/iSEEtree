#' Abundance density plot
#'
#' Density abundance profile of single features in a
#' \code{\link[TreeSummarizedExperiment:TreeSummarizedExperiment-constructor]{TreeSummarizedExperiment}}.
#' The panel implements \code{\link[miaViz:plotAbundanceDensity]{plotAbundanceDensity}}
#' to generate the plot.
#'
#' @section Slot overview:
#' The following slots control the thresholds used in the visualization:
#' \itemize{
#' \item \code{layout}, a string specifying abundance layout (jitter, density or points). 
#' \item \code{assay.type}, a string specifying the assay to visualize.
#' \item \code{n}, a number indicating the number of top taxa to visualize.
#' \item \code{flipped}, a logical specifying if the axis should be switched.
#' \item \code{order_descending}, a string specifying the descending order.
#' }
#'
#' In addition, this class inherits all slots from its parent \linkS4class{Panel} class.
#'
#' @return
#' The \code{AbundanceDensityPlot(...)} constructor creates an instance of an
#' AbundanceDensityPlot class, where any slot and its value can be passed to
#' \code{...} as a named argument.
#'
#' @author Giulio Benedetti
#' @examples
#' # Import TreeSE
#' library(mia)
#' data("Tengeler2020", package = "mia")
#' tse <- Tengeler2020
#' 
#' # Add relabundance assay
#' tse <- transformAssay(tse, method = "relabundance")
#'
#' # Launch iSEE
#' if (interactive()) {
#'   iSEE(tse)
#' }
#' 
#' @docType methods
#' @aliases AbundanceDensityPlot-class
#'   initialize,AbundanceDensityPlot-method
#'
#' @name AbundanceDensityPlot
NULL

#' @export
setClass("AbundanceDensityPlot", contains="Panel", slots=c(layout="character",
    assay.type="character", n="numeric", dots_colour="character",
    dots_colour_by="character", add_legend="logical", flipped="logical",
    order_descending="logical", dots_shape="character", dots_shape_by="character"))

#' @importFrom iSEE .singleStringError .validNumberError
#' @importFrom S4Vectors setValidity2
setValidity2("AbundanceDensityPlot", function(x) {
    msg <- character(0)
    
    msg <- .singleStringError(msg, x, fields=c("layout", "assay.type"))
    msg <- .validNumberError(msg, x, "n", lower=1, upper=Inf)
    msg <- .validLogicalError(msg, x, fields=c("add_legend", "flipped", "order_descending"))
    
    if( length(msg) ){
        return(msg)
    }
    
    TRUE
})

#' @importFrom iSEE .emptyDefault
#' @importFrom methods callNextMethod
setMethod("initialize", "AbundanceDensityPlot", function(.Object, ...) {
    args <- list(...)
    args <- .emptyDefault(args, "layout", "jitter")
    args <- .emptyDefault(args, "assay.type", "counts")
    args <- .emptyDefault(args, "n", 5)
    args <- .emptyDefault(args, "add_legend", TRUE)
    args <- .emptyDefault(args, "flipped", FALSE)
    args <- .emptyDefault(args, "dots_colour", "None")
    args <- .emptyDefault(args, "dots_shape", "None")
    args <- .emptyDefault(args, "dots_colour_by", NA_character_)
    args <- .emptyDefault(args, "dots_shape_by", NA_character_)
    args <- .emptyDefault(args, "order_descending", TRUE)
    
    do.call(callNextMethod, c(list(.Object), args))
})

#' @export
#' @importFrom methods new
AbundanceDensityPlot <- function(...) {
    new("AbundanceDensityPlot", ...)
}

#' @importFrom iSEE .getEncodedName .selectInput.iSEE .numericInput.iSEE
#' @importFrom methods slot
#' @importFrom SummarizedExperiment assayNames
setMethod(".defineDataInterface", "AbundanceDensityPlot",
    function(x, se, select_info) {
    
    panel_name <- .getEncodedName(x)
    
    list(.selectInput.iSEE(x, field="assay.type", label="Assay type",
            choices=assayNames(se), selected=slot(x, "assay.type")),
        # Number of taxa
        .numericInput.iSEE(x, field="n", label="Number of taxa",
            value=slot(x, "n"), min=1, max=nrow(se), step=1),
        
        .checkboxInput.iSEE(x, field="flipped", label="Switch axes",
            value=slot(x, "flipped")),
        
        .selectInput.iSEE(x, field="order_descending", label="Order decreasing",
            choices=c(TRUE, FALSE, NA),
            selected=slot(x, "order_descending")),
        
        .radioButtons.iSEE(
            x, field="dots_shape", label="Dot shape:", inline=TRUE,
            choices=c("None", "Column data"),
            selected=slot(x, "dots_shape")),
        
        .conditionalOnRadio(
            paste0(panel_name, "_dots_shape"), "Column data",
            iSEE:::.selectInputHidden(x, field="dots_shape_by",
                label="Shape dots by", choices=names(colData(se)), 
                selected=slot(x, "dots_shape_by"))))
})

#' @importFrom methods callNextMethod
setMethod(".defineInterface", "AbundanceDensityPlot",
    function(x, se, select_info) {
    
    out <- callNextMethod()
    list(out[1], .create_visual_box_for_abunddens_plot(x, se), out[-1])
})

#' @importFrom iSEE .getEncodedName .createProtectedParameterObservers
#'   .createUnprotectedParameterObservers
setMethod(".createObservers", "AbundanceDensityPlot",
    function(x, se, input, session, pObjects, rObjects) {
    
    callNextMethod()
    panel_name <- .getEncodedName(x)
    
    .createProtectedParameterObservers(panel_name,
        c("layout", "assay.type", "n", "add_legend", "flipped", "order_descending"),
        input=input, pObjects=pObjects, rObjects=rObjects)
    
    .createUnprotectedParameterObservers(panel_name,
        c("dots_colour", "dots_colour_by", "dots_shape", "dots_shape_by"),
        input=input, pObjects=pObjects, rObjects=rObjects)
    
    invisible(NULL)
})

setMethod(".fullName", "AbundanceDensityPlot",
    function(x) "Abundance density plot")

#' @importMethodsFrom iSEE .panelColor
setMethod(".panelColor", "AbundanceDensityPlot", function(x) "#8B5A2B")

#' @importFrom iSEE .getEncodedName
#' @importFrom shiny plotOutput
#' @importFrom shinyWidgets addSpinner
setMethod(".defineOutput", "AbundanceDensityPlot", function(x) {
    plot_name <- .getEncodedName(x)
    
    addSpinner(
        plotOutput(plot_name, height = paste0(slot(x, "PanelHeight"), "px")),
        color=.panelColor(x))
})

#' @importMethodsFrom iSEE .generateOutput
#' @importFrom iSEE .processMultiSelections .textEval
#' @importFrom miaViz plotRowTree
setMethod(".generateOutput", "AbundanceDensityPlot",
    function(x, se, all_memory, all_contents) {
    
    panel_env <- new.env()
    all_cmds <- list()
    args <- character(0)
    
    all_cmds[["select"]] <- .processMultiSelections(
        x, all_memory, all_contents, panel_env
    )
    
    if( exists("row_selected", envir=panel_env, inherits=FALSE) ){
        panel_env[["se"]] <- se[unlist(panel_env[["row_selected"]]), ]
    } else {
        panel_env[["se"]] <- se
    }
    
    args[["layout"]] <- deparse(slot(x, "layout"))
    args[["add_legend"]] <- deparse(slot(x, "add_legend"))
    args[["assay.type"]] <- deparse(slot(x, "assay.type"))
    args[["flipped"]] <- deparse(slot(x , "flipped"))
    args[["order_descending"]] <- deparse(slot(x, "order_descending"))
    
    if( is.na(slot(x, "n")) || slot(x, "n") <= 0 ){
        args[["n"]] <- 5
    } else if( slot(x, "n") > nrow(se) ){
        args[["n"]] <- nrow(se)
    } else {
        args[["n"]] <- deparse(slot(x, "n"))
    }
    
    if( slot(x, "dots_colour") == "Column data" ){
        args[["colour_by"]] <- deparse(slot(x, "dots_colour_by"))
    }
    
    if (slot(x, "dots_shape") == "Column data") {
        args[["shape_by"]] <- deparse(slot(x, "dots_shape_by"))
    }
    
    args <- sprintf("%s=%s", names(args), args)
    args <- paste(args, collapse=", ")
    fun_call <- sprintf("p <- miaViz::plotAbundanceDensity(se, %s)", args)
    
    fun_cmd <- paste(strwrap(fun_call, width = 80, exdent = 4), collapse = "\n")
    plot_out <- .textEval(fun_cmd, panel_env)
    all_cmds[["fun"]] <- fun_cmd
    
    list(commands=all_cmds, plot=plot_out, varname=NULL, contents=NULL)
})

#' @importFrom iSEE .getEncodedName .retrieveOutput
#' @importFrom shiny renderPlot
#' @importFrom methods callNextMethod
setMethod(".renderOutput", "AbundanceDensityPlot",
            function(x, se, output, pObjects, rObjects) {
    
    plot_name <- .getEncodedName(x)
    force(se) # defensive programming to avoid bugs due to delayed evaluation
    
    output[[plot_name]] <- renderPlot({
        .retrieveOutput(plot_name, se, pObjects, rObjects)
    })
    
    callNextMethod()
})

#' @importFrom methods callNextMethod
setMethod(".hideInterface", "AbundanceDensityPlot", function(x, field) {
    
    if ( field %in% c("SelectionHistory", "ColumnSelectionRestrict",
                    "ColumnSelectionDynamicSource", "ColumnSelectionSource") ){
        TRUE
    } else {
        callNextMethod()
    }
})

setMethod(".multiSelectionResponsive", "AbundanceDensityPlot",
            function(x, dims = character(0)) {
    
    if ("row" %in% dims) {
        return(TRUE)
    }
        return(FALSE)
})

#' @importFrom methods callNextMethod
#' @importFrom iSEE .getEncodedName .addTourStep
setMethod(".definePanelTour", "AbundanceDensityPlot", function(x) {
    rbind(c(paste0("#", .getEncodedName(x)), sprintf(
        "The <font color=\"%s\">Abundance Density Plot</font> panel
        contains a representation of the abundance density 
        (i.e. most abundant taxa).", .getPanelColor(x))),
    .addTourStep(x, "DataBoxOpen", "The <i>Data parameters</i> box shows the
        available parameters that can be tweaked to control the data on
        the plot.<br/><br/><strong>Action:</strong> click on this
        box to open up available options."),
    .addTourStep(x, "Visual", "The <i>Visual parameters</i> box shows
        the available visual parameters that can be tweaked in this
        plot.<br/><br/><strong>Action:</strong> click on this box to
        open up available options."),
    callNextMethod())
})

#' @importFrom iSEE .getEncodedName collapseBox .selectInput.iSEE
#'   .radioButtons.iSEE .conditionalOnRadio
#' @importFrom methods slot
#' @importFrom SummarizedExperiment colData
.create_visual_box_for_abunddens_plot <- function(x, se) {
    
    panel_name <- .getEncodedName(x)
    
    .addSpecificTour(class(x)[1], "layout", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_layout + .selectize-control"), intro = "Here, we can select the
            layout of the plot.")))})
    .addSpecificTour(class(x)[1], "add_legend", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_add_legend"), intro = "Here, we can choose
            whether or not to show a legend.")))})
    .addSpecificTour(class(x)[1], "dots_colour", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_dots_colour"), intro = "Here, we can choose
            whether or not to show colors.")))})
    .addSpecificTour(class(x)[1], "dots_colour_by", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_dots_colour_by"), intro = "Here, we can choose
            the way you want to map the colors.")))})
    .addSpecificTour(class(x)[1], "assay.type", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_assay\\.type + .selectize-control"), intro = "Here, we can choose
            the assay to be transformed.")))})
    .addSpecificTour(class(x)[1], "n", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_n"), intro = "Here, we can choose
            the number of taxa to be selected.")))})
    .addSpecificTour(class(x)[1], "flipped", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_flipped"), intro = "Here, we can choose
            whether or not to switch the axis.")))})
    .addSpecificTour(class(x)[1], "order_descending", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_order_descending + .selectize-control"), intro = "Here, we can choose
            whether or not to use descending order.
            If NA uses the order found in the <code>SummarizedExperiment</code>
            object.")))})
    .addSpecificTour(class(x)[1], "dots_shape", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_dots_shape"), intro = "Here, we can choose
            whether or not to change points shape.")))})
    .addSpecificTour(class(x)[1], "dots_shape_by", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_dots_shape_by"), intro = "Here, we can choose
            to group by the different point shape groups.")))})
    
    # Define what parameters the user can adjust
    collapseBox(
        paste0(panel_name, "_Visual"), title="Visual parameters", open=FALSE,
            # Tree layout
            .selectInput.iSEE(x, field="layout", label="Layout",
                choices=c("jitter", "density", "point"),
                selected=slot(x, "layout")),
            # Colour legend
            .checkboxInput.iSEE(x, field="add_legend", label="View legend",
                value=slot(x, "add_legend")),
            .radioButtons.iSEE(
                x, field="dots_colour", label="Dot color:", inline=TRUE,
                choices=c("None", "Column data"),
                selected=slot(x, "dots_colour")),
            .conditionalOnRadio(
                paste0(panel_name, "_dots_colour"), "Column data",
                iSEE:::.selectInputHidden(x, field="dots_colour_by",
                    label="Color dots by", choices=names(colData(se)), 
                    selected=slot(x, "dots_colour_by"))))
}
