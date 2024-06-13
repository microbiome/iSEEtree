#' Abundance plot
#'
#' Composite abundance profile of all features in a
#' \code{\link[TreeSummarizedExperiment:TreeSummarizedExperiment-constructor]{TreeSummarizedExperiment}}
#' object. The panel implements \code{\link[miaViz:plotAbundance]{plotAbundance}}
#' to generate the plot.
#'
#' @section Slot overview:
#' The following slots control the thresholds used in the visualization:
#' \itemize{
#' \item \code{rank}, a string specifying the taxonomic rank to visualize.
#' \item \code{use_relative}, a logical indicating if the relative values should be calculated. 
#' \item \code{add_legend}, a logical indicating if the color legend should appear.
#' }
#'
#' In addition, this class inherits all slots from its parent \linkS4class{Panel} class.
#'
#' @return
#' The \code{AbundancePlot(...)} constructor creates an instance of an
#' AbundancePlot class, where any slot and its value can be passed to \code{...}
#' as a named argument.
#'
#' @author Giulio Benedetti
#' @examples
#' # Import TreeSE
#' library(mia)
#' data("Tengeler2020", package = "mia")
#' tse <- Tengeler2020
#' 
#' # Launch iSEE
#' if (interactive()) {
#'   iSEE(tse)
#' }
#' 
#' @docType methods
#' @aliases AbundancePlot-class
#'   initialize,AbundancePlot-method
#'
#' @name AbundancePlot
NULL

#' @export
setClass("AbundancePlot", contains="Panel",
    slots=c(rank="character", use_relative="logical", add_legend="logical",
            order_sample_by="character", order_sample="character", 
            decreasing="logical"))

#' @importFrom S4Vectors setValidity2
setValidity2("AbundancePlot", function(x) {
    
    msg <- character(0)
    msg <- .singleStringError(msg, x, fields="rank")
    msg <- .validLogicalError(msg, x, fields=c("add_legend", "use_relative"))
    
    if( length(msg) ){
        return(msg)
    }
    TRUE
})

#' @importFrom methods callNextMethod
setMethod("initialize", "AbundancePlot", function(.Object, ...) {
    args <- list(...)
    args <- .emptyDefault(args, "rank", NA_character_)
    args <- .emptyDefault(args, "order_sample_by", NA_character_)
    args <- .emptyDefault(args, "add_legend", TRUE)
    args <- .emptyDefault(args, "use_relative", TRUE)
    args <- .emptyDefault(args, "decreasing", FALSE)
    args <- .emptyDefault(args, "order_sample", "None")
    
    do.call(callNextMethod, c(list(.Object), args))
})

#' @export
#' @importFrom methods new
AbundancePlot <- function(...) {
    new("AbundancePlot", ...)
}

#' @importFrom iSEE .getEncodedName .checkboxInput.iSEE
#' @importFrom methods slot
setMethod(".defineDataInterface", "AbundancePlot", function(x, se, select_info) {
    panel_name <- .getEncodedName(x)
          
    list(.checkboxInput.iSEE(x, field="use_relative", label="Use relative values",
                            value=slot(x, "use_relative")),
        .radioButtons.iSEE(x, field="order_sample", label="Ordering sample:",
                            inline=TRUE, choices=c("None", "Column data", "Row data"),
                            selected=slot(x, "order_sample")),
        .conditionalOnRadio(
            paste0(panel_name, "_order_sample"), "Column data",
            list(
                .selectInput.iSEE(x, field="order_sample_by",
                    label="Order sample by", choices=names(colData(se)), 
                    selected=slot(x, "order_sample_by")),
                .checkboxInput.iSEE(x, field="decreasing",
                    label="Order decreasingly", value=slot(x, "decreasing")))),
        .conditionalOnRadio(
            paste0(panel_name, "_order_sample"), "Row data",
                .selectInput.iSEE(x, field="order_sample_by",
                    label="Order sample by", choices=unique(rowData(se)$Phylum), 
                    selected=slot(x, "order_sample_by"))))
})

#' @importFrom methods callNextMethod
setMethod(".defineInterface", "AbundancePlot", function(x, se, select_info) {
     
    out <- callNextMethod()
    list(out[1], .create_visual_box_for_abund_plot(x, se), out[-1])
})

#' @importMethodsFrom iSEE .createObservers
setMethod(".createObservers", "AbundancePlot",
    function(x, se, input, session, pObjects, rObjects) {
    
    callNextMethod()
    panel_name <- .getEncodedName(x)
    
    .createProtectedParameterObservers(panel_name, c("rank", "use_relative", "add_legend"),
        input=input, pObjects=pObjects, rObjects=rObjects)
    
    .createUnprotectedParameterObservers(panel_name, c("order_sample", "order_sample_by",
        "decreasing"), input=input, pObjects=pObjects, rObjects=rObjects)
    
    invisible(NULL)
})

setMethod(".fullName", "AbundancePlot", function(x) "Abundance plot")

setMethod(".panelColor", "AbundancePlot", function(x) "#00E5EE")

#' @importFrom iSEE .getEncodedName
#' @importFrom shiny plotOutput
#' @importFrom shinyWidgets addSpinner
setMethod(".defineOutput", "AbundancePlot", function(x) {
    panel_name <- .getEncodedName(x)
    
    addSpinner(plotOutput(panel_name,
        height = paste0(slot(x, "PanelHeight"), "px")), color=.panelColor(x))
})

#' @importFrom miaViz plotRowTree
#' @importFrom iSEE .processMultiSelections
setMethod(".generateOutput", "AbundancePlot",
    function(x, se, all_memory, all_contents) {
    
    panel_env <- new.env()
    all_cmds <- list()
    args <- character(0)
    
    all_cmds[["select"]] <- .processMultiSelections(
        x, all_memory, all_contents, panel_env
    )
    
    if( exists("col_selected", envir=panel_env, inherits=FALSE) ){
        panel_env[["se"]] <- se[ , unlist(panel_env[["col_selected"]])]
    } else {
        panel_env[["se"]] <- se
    }
    
    args[["rank"]] <- deparse(slot(x, "rank"))
    args[["add_legend"]] <- deparse(slot(x, "add_legend"))
    args[["use_relative"]] <- deparse(slot(x, "use_relative"))
    
    if (slot(x, "order_sample") == "Column data") {
        args[["order_sample_by"]] <- deparse(slot(x, "order_sample_by"))
        args[["decreasing"]] <- deparse(slot(x, "decreasing"))
    }
    
    if (slot(x, "order_sample") == "Row data") {
        args[["order_sample_by"]] <- deparse(slot(x, "order_sample_by"))
    }
    
    args <- sprintf("%s=%s", names(args), args)
    args <- paste(args, collapse=", ")
    fun_call <- sprintf("p <- miaViz::plotAbundance(se, %s)", args)
    
    fun_cmd <- paste(strwrap(fun_call, width = 80, exdent = 4), collapse = "\n")
    plot_out <- .textEval(fun_cmd, panel_env)
    all_cmds[["fun"]] <- fun_cmd
    
    list(commands=all_cmds, plot=plot_out, varname=NULL, contents=NULL)
})

#' @importFrom iSEE .getEncodedName .retrieveOutput
#' @importFrom shiny renderPlot
#' @importFrom methods callNextMethod
setMethod(".renderOutput", "AbundancePlot",
    function(x, se, output, pObjects, rObjects) {
    
    panel_name <- .getEncodedName(x)
    force(se) # defensive programming to avoid bugs due to delayed evaluation
    
    output[[panel_name]] <- renderPlot({
        .retrieveOutput(panel_name, se, pObjects, rObjects)
    })
    
    callNextMethod()
})

#' @importFrom methods callNextMethod
setMethod(".hideInterface", "AbundancePlot", function(x, field) {
    if( field %in% c("SelectionHistory", "RowSelectionRestrict",
        "RowSelectionDynamicSource", "RowSelectionSource") ){
        TRUE
    } else {
        callNextMethod()
    }
})

setMethod(".multiSelectionResponsive", "AbundancePlot",
    function(x, dims = character(0)) {
    
    if( "column" %in% dims ){
        return(TRUE)
    }
    return(FALSE)
})

#' @importFrom methods callNextMethod
#' @importFrom iSEE .getEncodedName .addTourStep
setMethod(".definePanelTour", "AbundancePlot", function(x) {
    rbind(c(paste0("#", .getEncodedName(x)), sprintf(
        "The <font color=\"%s\">Abundance Plot</font> panel
        contains a representation of the relative abundance
        for each taxonomic rank. Each column corresponds to
        a sample of the <code>SummarizedExperiment</code>
        object.", .getPanelColor(x))),
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

#' @importFrom iSEE .getEncodedName .selectInput.iSEE .checkboxInput.iSEE
# .addSpecificTour
#' @importFrom methods slot
#' @importFrom SummarizedExperiment rowData
.create_visual_box_for_abund_plot <- function(x, se) {
    
    panel_name <- .getEncodedName(x)
    
    .addSpecificTour(class(x)[1], "rank", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_rank + .selectize-control"), intro = "Here, we can select the
            taxonomic rank.")))})
    .addSpecificTour(class(x)[1], "add_legend", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_add_legend"), intro = "Here, we can choose
            whether or not to show a legend.")))})
    .addSpecificTour(class(x)[1], "use_relative", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_use_relative"), intro = "Here, we can choose
            whether to use relative or absolute values.")))})
    .addSpecificTour(class(x)[1], "order_sample", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_order_sample"), intro = "Here, we can choose
            how to order the abundance plot by.")))})
    
    # Define what parameters the user can adjust
    collapseBox(paste0(panel_name, "_Visual"),
        title="Visual parameters", open=FALSE,
        # Tree layout
        .selectInput.iSEE(x, field="rank", label="Rank",
            choices=names(rowData(se)), selected=slot(x, "rank")),
        # Colour legend
        .checkboxInput.iSEE(x, field="add_legend", label="View legend",
            value=slot(x, "add_legend")))
    
}