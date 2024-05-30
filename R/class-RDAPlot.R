#' RDA plot
#'
#' CCA/RDA plot for the rows of a
#' \code{\link[TreeSummarizedExperiment:TreeSummarizedExperiment-constructor]{TreeSummarizedExperiment}}
#' object. The reduced dimension can be produced with \code{\link[mia:runCCA]{runRDA}}
#' and gets stored in the \code{\link[SingleCellExperiment:reducedDims]{reducedDim}}
#' slot of the experiment object. The panel implements \code{\link[miaViz:plotCCA]{plotRDA}}
#' to generate the plot.
#'
#' @section Slot overview:
#' The following slots control the thresholds used in the visualization:
#' \itemize{
#' \item \code{add.ellipse}, a string specifying ellipse layout (filled, coloured or absent). 
#' \item \code{colour_by}, a string specifying the parameter to color by.
#' \item \code{add.vectors}, a logical indicating if vectors should appear in the plot.
#' \item \code{vec.text}, a logical indicating if text should be encased in a box.
#' }
#'
#' In addition, this class inherits all slots from its parent \linkS4class{Panel} class.
#'
#' @return
#' The \code{RDAPlot(...)} contructor creates an instance of a RDAPlot class,
#' where any slot and its value can be passed to \code{...} as a named argument.
#'
#' @author Giulio Benedetti
#' @examples
#' # Import TreeSE
#' library(mia)
#' data("enterotype", package = "mia")
#' tse <- enterotype
#'
#' # Run RDA and store results into TreeSE
#' tse <- runRDA(tse,
#'               formula = assay ~ ClinicalStatus + Gender + Age,
#'               FUN = vegan::vegdist,
#'               distance = "bray",
#'               na.action = na.exclude)
#'
#' # Launch iSEE with custom initial panels
#' if (interactive()) {
#'   iSEE(tse, initial = c(RDAPlot()))
#' }
#' 
#' #' @docType methods
#' @aliases RDAPlot-class
#'   initialize,RDAPlot-method
#'
#' @name RDAPlot
NULL

setClassUnion("charlog", c("character", "logical"))

#' @export
setClass("RDAPlot", contains="Panel", slots=c(dimred="character",
    add.ellipse="charlog", colour_by="character", vec.text="logical",
    add.vectors="logical"))

#' @importFrom iSEE .singleStringError .validLogicalError
#' @importFrom S4Vectors setValidity2
setValidity2("RDAPlot", function(x) {
    msg <- character(0)
    
    msg <- .singleStringError(msg, x, fields=c("dimred", "colour_by"))
    msg <- .validLogicalError(msg, x, fields=c("vec.text", "add.vectors"))
    
    if( length(msg) ){
        return(msg)
    }
    
    TRUE
})

#' @importFrom iSEE .emptyDefault
#' @importFrom methods callNextMethod
setMethod("initialize", "RDAPlot", function(.Object, ...) {
    args <- list(...)
    args <- .emptyDefault(args, "dimred", "RDA")
    args <- .emptyDefault(args, "add.ellipse", "fill")
    args <- .emptyDefault(args, "colour_by", NA_character_)
    args <- .emptyDefault(args, "add.vectors", TRUE)
    args <- .emptyDefault(args, "vec.text", TRUE)
    
    do.call(callNextMethod, c(list(.Object), args))
})

#' @export
#' @importFrom methods new
RDAPlot <- function(...) {
    new("RDAPlot", ...)
}

#' @importFrom iSEE .getEncodedName .selectInput.iSEE .numericInput.iSEE
#' @importFrom SingleCellExperiment reducedDimNames
#' @importFrom methods slot
setMethod(".defineDataInterface", "RDAPlot", function(x, se, select_info) {
    panel_name <- .getEncodedName(x)
    
    list(.selectInput.iSEE(x, field="dimred", label="Reduced dimension",
        choices=reducedDimNames(se), selected=slot(x, "dimred")))
})

#' @importFrom methods callNextMethod
setMethod(".defineInterface", "RDAPlot", function(x, se, select_info) {
    
    out <- callNextMethod()
    list(out[1], .create_visual_box_for_rda(x, se), out[-1])
})

#' @importFrom iSEE .getEncodedName .createProtectedParameterObservers
setMethod(".createObservers", "RDAPlot",
    function(x, se, input, session, pObjects, rObjects) {
    
    callNextMethod()
    panel_name <- .getEncodedName(x)
    
    .createProtectedParameterObservers(panel_name, c("dimred", "add.ellipse",
        "colour_by", "vec.text", "add.vectors"), input=input, pObjects=pObjects,
        rObjects=rObjects)
    
    invisible(NULL)
})

setMethod(".fullName", "RDAPlot", function(x) "RDA plot")

setMethod(".panelColor", "RDAPlot", function(x) "#CD5B45")

#' @importFrom iSEE .getEncodedName
#' @importFrom shiny plotOutput
#' @importFrom shinyWidgets addSpinner
setMethod(".defineOutput", "RDAPlot", function(x) {
    panel_name <- .getEncodedName(x)
    
    addSpinner(plotOutput(panel_name,
        height = paste0(slot(x, "PanelHeight"), "px")), color=.panelColor(x))
})

#' @importFrom iSEE .processMultiSelections .textEval
#' @importFrom miaViz plotRowTree
setMethod(".generateOutput", "RDAPlot",
    function(x, se, all_memory, all_contents) {
    
    panel_env <- new.env()
    all_cmds <- list()
    args <- character(0)
    
    all_cmds[["select"]] <- .processMultiSelections(
        x, all_memory, all_contents, panel_env
    )
    
    if( is.null(panel_env[["col_selected"]]) ){
        panel_env[["se"]] <- se
    } else {
        panel_env[["se"]] <- se[ , unlist(panel_env[["col_selected"]])]
    }

    args[["dimred"]] <- deparse(slot(x, "dimred"))
    args[["add.ellipse"]] <- deparse(slot(x, "add.ellipse"))
    args[["colour_by"]] <- deparse(slot(x, "colour_by"))
    args[["vec.text"]] <- deparse(slot(x, "vec.text"))
    args[["add.vectors"]] <- deparse(slot(x, "add.vectors"))
    
    args <- sprintf("%s=%s", names(args), args)
    args <- paste(args, collapse=", ")
    fun_call <- sprintf("p <- miaViz::plotRDA(se, %s)", args)
    
    fun_cmd <- paste(strwrap(fun_call, width = 80, exdent = 4), collapse = "\n")
    plot_out <- .textEval(fun_cmd, panel_env)
    all_cmds[["fun"]] <- fun_cmd
    
    list(commands=all_cmds, plot=plot_out, varname=NULL, contents=NULL)
})

#' @importMethodsFrom iSEE .renderOutput
#' @importFrom iSEE .getEncodedName .retrieveOutput
#' @importFrom shiny renderPlot
setMethod(".renderOutput", "RDAPlot",
    function(x, se, output, pObjects, rObjects) {
    
    panel_name <- .getEncodedName(x)
    force(se) # defensive programming to avoid bugs due to delayed evaluation
    
    output[[panel_name]] <- renderPlot({
        .retrieveOutput(panel_name, se, pObjects, rObjects)
    })

    callNextMethod()
})

#' @importFrom methods callNextMethod
setMethod(".hideInterface", "RDAPlot", function(x, field) {
    
    if( field %in% c("SelectionHistory", "RowSelectionRestrict",
        "RowSelectionDynamicSource", "RowSelectionSource") ){
        TRUE
    } else {
        callNextMethod()
    }

})

setMethod(".multiSelectionResponsive", "RDAPlot", function(x, dims = character(0)) {
    
    if( "column" %in% dims ){
        return(TRUE)
    }
    return(FALSE)
})

#' @importFrom methods callNextMethod
#' @importFrom iSEE .getEncodedName .addTourStep
setMethod(".definePanelTour", "RDAPlot", function(x) {
    rbind(c(paste0("#", .getEncodedName(x)), sprintf(
        "The <font color=\"%s\">RDA Plot</font> panel contains a representation
        of the correlation between variables of our dataset.", .getPanelColor(x))),
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
#'   .conditionalOnCheckSolo
#' @importFrom methods slot
#' @importFrom SummarizedExperiment colData
.create_visual_box_for_rda <- function(x, se) {
    panel_name <- .getEncodedName(x)
    
    .addSpecificTour(class(x)[1], "colour_by", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_colour_by + .selectize-control"), intro = "Here, we can select how
            colours are mapped on the plot.")))})
    .addSpecificTour(class(x)[1], "add.ellipse", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_add.ellipse + .selectize-control"), intro = "Here, we can choose
            whether or not to add an ellispe.")))})
    .addSpecificTour(class(x)[1], "add.vectors", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_add.vectors + .selectize-control"), intro = "Here, we can choose
            whether or not to add vectors")))})
    .addSpecificTour(class(x)[1], "vec.text", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "vec.text + .selectize-control"), intro = "Here, we can choose
            the vectors text.")))})
    
    # Define what parameters the user can adjust
    collapseBox(paste0(panel_name, "_Visual"),
        title="Visual parameters", open=FALSE,
        .selectInput.iSEE(x, field="colour_by", label="Color by",
            choices=names(colData(se)), selected=slot(x, "colour_by")),
        .selectInput.iSEE(x, field="add.ellipse", label="Ellipse style",
            choices = c("fill", "colour", "FALSE"),
            selected=slot(x, "add.ellipse")),
        .checkboxInput.iSEE(x, field="add.vectors", label="Add vectors",
            value=slot(x, "add.vectors")),
        .conditionalOnCheckSolo(paste0(panel_name, "_add.vectors"), TRUE,
            .checkboxInput.iSEE(x, field="vec.text", label="Unboxed labels",
                value=slot(x, "vec.text"))))
}
