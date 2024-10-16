#' Loading plot
#'
#' Contribution of single features in a
#' \code{\link[TreeSummarizedExperiment:TreeSummarizedExperiment-constructor]{TreeSummarizedExperiment}}
#' to the components of a target reduced dimension. The panel implements
#' \code{\link[miaViz:plotLoadings]{plotLoadings}} to generate the plot.
#'
#' @section Slot overview:
#' The following slots control the thresholds used in the visualisation:
#' \itemize{
#' \item \code{dimred}, a string specifying the dimred to visualize.
#' \item \code{layout}, a string specifying abundance layout (barplot or heatmap).
#' \item \code{ncomponents}, a number indicating the number of components to visualize.
#' }
#'
#' In addition, this class inherits all slots from its parent \linkS4class{Panel} class.
#'
#' @return
#' The \code{LoadingPlot(...)} constructor creates an instance of an
#' LoadingPlot class, where any slot and its value can be passed to
#' \code{...} as a named argument.
#'
#' @author Giulio Benedetti
#' @examples
#' # Import libraries
#' library(mia)
#' library(scater)
#' 
#' # Import TreeSE
#' data("Tengeler2020", package = "mia")
#' tse <- Tengeler2020
#' 
#' # Add relabundance assay
#' tse <- transformAssay(tse, method = "relabundance")
#' 
#' # Add reduced dimensions
#' tse <- runPCA(tse, assay.type = "relabundance")
#'
#' # Store panel into object
#' panel <- LoadingPlot()
#' # View some adjustable parameters
#' head(slotNames(panel))
#'
#' # Launch iSEE with custom initial panel
#' if (interactive()) {
#'   iSEE(tse, initial = c(panel))
#' }
#' 
#' @docType methods
#' @name LoadingPlot
NULL

#' @rdname LoadingPlot
#' @export
setClass("LoadingPlot", contains="Panel", slots=c(dimred="character",
    layout="character", ncomponents="numeric", add.tree="logical"))

#' @importFrom iSEE .singleStringError .validNumberError .validLogicalError
#' @importFrom S4Vectors setValidity2
setValidity2("LoadingPlot", function(x) {
    msg <- character(0)
    
    msg <- .singleStringError(msg, x, fields=c("dimred", "layout"))
    msg <- .validNumberError(msg, x, "ncomponents", lower=1, upper=Inf)
    msg <- .validLogicalError(msg, x, fields="add.tree")

    if( length(msg) ){
        return(msg)
    }
    
    TRUE
})

#' @importFrom iSEE .emptyDefault
#' @importFrom methods callNextMethod
setMethod("initialize", "LoadingPlot", function(.Object, ...) {
    args <- list(...)
    args <- .emptyDefault(args, "dimred", "PCA")
    args <- .emptyDefault(args, "layout", "heatmap")
    args <- .emptyDefault(args, "ncomponents", 5)
    args <- .emptyDefault(args, "add.tree", FALSE)
    
    do.call(callNextMethod, c(list(.Object), args))
})

#' @export
#' @importFrom methods new
LoadingPlot <- function(...) {
    new("LoadingPlot", ...)
}

#' @importFrom iSEE .getEncodedName .selectInput.iSEE .numericInput.iSEE
#' @importFrom methods slot
#' @importFrom SingleCellExperiment reducedDim reducedDimNames
setMethod(".defineDataInterface", "LoadingPlot",
    function(x, se, select_info) {
    
    panel_name <- .getEncodedName(x)
    
    list(.selectInput.iSEE(x, field="dimred", label="Reduced dimension",
            choices=reducedDimNames(se), selected=slot(x, "dimred")),
        # Number of components
        .numericInput.iSEE(x, field="ncomponents", label="Number of components",
            value=slot(x, "ncomponents"), min=1, step=1,
            max=ncol(reducedDim(se, slot(x, "dimred")))))
})

#' @importFrom methods callNextMethod
setMethod(".defineInterface", "LoadingPlot",
    function(x, se, select_info) {
    
    out <- callNextMethod()
    list(out[1], .create_visual_box_for_loading_plot(x, se), out[-1])
})

#' @importFrom iSEE .getEncodedName .createProtectedParameterObservers
#'   .createUnprotectedParameterObservers
setMethod(".createObservers", "LoadingPlot",
    function(x, se, input, session, pObjects, rObjects) {
    
    callNextMethod()
    panel_name <- .getEncodedName(x)
    
    .createProtectedParameterObservers(panel_name,
        c("dimred", "ncomponents"),
        input=input, pObjects=pObjects, rObjects=rObjects)
    
    .createUnprotectedParameterObservers(panel_name,
        c("layout", "add.tree"),
        input=input, pObjects=pObjects, rObjects=rObjects)
    
    invisible(NULL)
})

setMethod(".fullName", "LoadingPlot",
    function(x) "Loading plot")

#' @importMethodsFrom iSEE .panelColor
setMethod(".panelColor", "LoadingPlot", function(x) "yellow")

#' @importFrom iSEE .getEncodedName
#' @importFrom shiny plotOutput
#' @importFrom shinyWidgets addSpinner
setMethod(".defineOutput", "LoadingPlot", function(x) {
    plot_name <- .getEncodedName(x)
    
    addSpinner(
        plotOutput(plot_name, height = paste0(slot(x, "PanelHeight"), "px")),
        color=.panelColor(x))
})

#' @importMethodsFrom iSEE .generateOutput
#' @importFrom iSEE .processMultiSelections .textEval
#' @importFrom miaViz plotLoadings
setMethod(".generateOutput", "LoadingPlot",
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
    
    args[["dimred"]] <- deparse(slot(x, "dimred"))
    args[["layout"]] <- deparse(slot(x, "layout"))
    args[["add.tree"]] <- deparse(slot(x , "add.tree"))

    if( is.na(slot(x, "ncomponents")) || slot(x, "ncomponents") <= 0 ){
        args[["ncomponents"]] <- 5
    } else if( slot(x, "ncomponents") > ncol(reducedDim(se, slot(x, "dimred"))) ){
        args[["ncomponents"]] <- ncol(reducedDim(se, slot(x, "dimred")))
    } else {
        args[["ncomponents"]] <- deparse(slot(x, "ncomponents"))
    }
    
    args <- sprintf("%s=%s", names(args), args)
    args <- paste(args, collapse=", ")
    fun_call <- sprintf("p <- miaViz::plotLoadings(se, %s)", args)
    
    fun_cmd <- paste(strwrap(fun_call, width = 80, exdent = 4), collapse = "\n")
    plot_out <- .textEval(fun_cmd, panel_env)
    all_cmds[["fun"]] <- fun_cmd
    
    list(commands=all_cmds, plot=plot_out, varname=NULL, contents=NULL)
})

#' @importFrom iSEE .getEncodedName .retrieveOutput
#' @importFrom shiny renderPlot
#' @importFrom methods callNextMethod
setMethod(".renderOutput", "LoadingPlot",
            function(x, se, output, pObjects, rObjects) {
    
    plot_name <- .getEncodedName(x)
    force(se) # defensive programming to avoid bugs due to delayed evaluation
    
    output[[plot_name]] <- renderPlot({
        .retrieveOutput(plot_name, se, pObjects, rObjects)
    })
    
    callNextMethod()
})

#' @importFrom grDevices pdf dev.off
setMethod(".exportOutput", "LoadingPlot",
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
setMethod(".hideInterface", "LoadingPlot", function(x, field) {
    
    if ( field %in% c("SelectionHistory", "ColumnSelectionRestrict",
                    "ColumnSelectionDynamicSource", "ColumnSelectionSource") ){
        TRUE
    } else {
        callNextMethod()
    }
})

setMethod(".multiSelectionResponsive", "LoadingPlot",
            function(x, dims = character(0)) {
    
    if ("row" %in% dims) {
        return(TRUE)
    }
        return(FALSE)
})

#' @importFrom methods callNextMethod
#' @importFrom iSEE .getEncodedName .addTourStep
setMethod(".definePanelTour", "LoadingPlot", function(x) {
    rbind(c(paste0("#", .getEncodedName(x)), sprintf(
        "The <font color=\"%s\">Loading Plot</font> panel
        contains a representation of the taxa contributions to the target
        reduced dimensions.", .getPanelColor(x))),
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
#'   .radioButtons.iSEE .conditionalOnRadio .checkboxInput.iSEE
#' @importFrom methods slot
#' @importFrom SummarizedExperiment colData
.create_visual_box_for_loading_plot <- function(x, se) {
    
    panel_name <- .getEncodedName(x)
    
    .addSpecificTour(class(x)[1], "layout", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_layout + .selectize-control"), intro = "Here, we can select the
            layout of the plot.")))})
    .addSpecificTour(class(x)[1], "add.tree", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_add\\.tree"), intro = "Here, we can choose
            whether or not to show the phylogenetic tree.")))})
    
    # Define what parameters the user can adjust
    collapseBox(
        paste0(panel_name, "_Visual"), title="Visual parameters", open=FALSE,
            # Panel layout
            .selectInput.iSEE(x, field="layout", label="Layout",
                choices=c("barplot", "heatmap"),
                selected=slot(x, "layout")),
            # Add tree
            .checkboxInput.iSEE(x, field="add.tree", label="View tree",
                value=slot(x, "add.tree")))
}
