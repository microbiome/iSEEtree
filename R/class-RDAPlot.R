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
#' \item \code{confidence.level}, a numeric between 0 and 1 to adjust confidence level.
#' \item \code{ellipse.alpha}, a numeric between 0 and 1 t o adjust ellipse opacity.
#' \item \code{ellipse.linewidth}, a numeric specifying the size of ellipses.
#' \item \code{ellipse.linetype}, a numeric specifying the style of ellipses.
#' \item \code{vec.size}, a numeric specifying the size of vectors.
#' \item \code{vec.colour}, a string specifying the colour of vectors.
#' \item \code{vec.linetype}, a numeric specifying the style of vector lines.
#' \item \code{arrow.size}, a numeric specifying the size of arrows.
#' \item \code{label.colour}, a string specifying the colour of text and labels.
#' \item \code{label.size}, a numeric specifying the size of text and labels. 
#' \item \code{add.significance}, a logical indicating if variance and p-value
#'  should appear in the labels.
#' \item \code{add.expl.var}, a logical indicating if variance shoud appear 
#'  on the coordinate axes.
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
    add.vectors="logical", ellipse.alpha="numeric", confidence.level="numeric",
    add.significance="logical", add.expl.var="logical", ellipse.linewidth="numeric",
    ellipse.linetype="numeric", vec.size="numeric", vec.colour="character",
    vec.linetype="numeric", arrow.size="numeric", label.colour="character",
    label.size="numeric", vector_parameters="logical", visual_parameters="character",
    arrow_parameters="logical", ellipse_parameters="logical", label_parameters="logical"))

#' @importFrom iSEE .singleStringError .validLogicalError .validNumberError
#' @importFrom S4Vectors setValidity2
setValidity2("RDAPlot", function(x) {
    msg <- character(0)
    
    msg <- .singleStringError(msg, x, fields=c("dimred", "colour_by", "vec.colour",
                                    "label.colour", "visual_parameters"))
    msg <- .validLogicalError(msg, x, fields=c("vec.text", "add.vectors",
                                    "add.significance", "add.expl.var",
                                    "vector_parameters", "ellipse_parameters",
                                    "arrow_parameters", "label_parameters"))
    msg <- .validNumberError(msg, x, "ellipse.alpha", lower=0, upper=1)
    msg <- .validNumberError(msg, x, "confidence.level", lower=0, upper=1)
    msg <- .validNumberError(msg, x, "ellipse.linewidth", lower=0, upper=1)
    msg <- .validNumberError(msg, x, "ellipse.linetype", lower=1, upper=6)
    msg <- .validNumberError(msg, x, "vec.size", lower=0, upper=1)
    msg <- .validNumberError(msg, x, "vec.linetype", lower=1, upper=6)
    msg <- .validNumberError(msg, x, "arrow.size", lower=0, upper=1)
    msg <- .validNumberError(msg, x, "label.size", lower=0, upper=10)
    
    
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
    args <- .emptyDefault(args, "vec.colour", "black")
    args <- .emptyDefault(args, "label.colour", "black")
    args <- .emptyDefault(args, "add.ellipse", "fill")
    args <- .emptyDefault(args, "colour_by", NA_character_)
    args <- .emptyDefault(args, "add.vectors", TRUE)
    args <- .emptyDefault(args, "vec.text", TRUE)
    args <- .emptyDefault(args, "vector_parameters", FALSE)
    args <- .emptyDefault(args, "visual_parameters", NA_character_)
    args <- .emptyDefault(args, "arrow_parameters", FALSE)
    args <- .emptyDefault(args, "ellipse_parameters", FALSE)
    args <- .emptyDefault(args, "label_parameters", FALSE)
    args <- .emptyDefault(args, "add.expl.var", TRUE)
    args <- .emptyDefault(args, "add.significance", TRUE)
    args <- .emptyDefault(args, "ellipse.alpha", 0.2)
    args <- .emptyDefault(args, "confidence.level", 0.95)
    args <- .emptyDefault(args, "ellipse.linewidth", 0.1)
    args <- .emptyDefault(args, "ellipse.linetype", 1)
    args <- .emptyDefault(args, "vec.size", 0.5)
    args <- .emptyDefault(args, "vec.linetype", 1)
    args <- .emptyDefault(args, "arrow.size", 0.25)
    args <- .emptyDefault(args, "label.size", 4)
    
    do.call(callNextMethod, c(list(.Object), args))
})

#' @export
#' @importFrom methods new
RDAPlot <- function(...) {
    new("RDAPlot", ...)
}

#' @importFrom iSEE .getEncodedName .selectInput.iSEE .checkboxInput.iSEE .sliderInput.iSEE
#' @importFrom SingleCellExperiment reducedDimNames
#' @importFrom methods slot
setMethod(".defineDataInterface", "RDAPlot", function(x, se, select_info) {
    panel_name <- .getEncodedName(x)
    
    list(.selectInput.iSEE(x, field="dimred", label="Reduced dimension",
            choices=reducedDimNames(se), selected=slot(x, "dimred")),
        .sliderInput.iSEE(x, field="confidence.level", label="Confidence level",
            min = 0.90, max = 0.999, step= 0.001, value=slot(x, "confidence.level")),
        .checkboxInput.iSEE(x, field="add.significance", label="Show variance in labels",
            value=slot(x, "add.significance")),
        .checkboxInput.iSEE(x, field="add.expl.var", label="Show variance in axes",
            value=slot(x, "add.expl.var")))
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
        "colour_by", "vec.text", "add.vectors", "add.expl.var", "add.significance",
        "confidence.level", "ellipse.alpha", "ellipse.linewidth", "ellipse.linetype",
        "vec.size", "vec.colour", "vec.linetype", "arrow.size", "label.colour",
        "label.size", "vector_parameters", "arrow_parameters", "visual_parameters",
        "ellipse_parameters", "label_parameters"),
        input=input, pObjects=pObjects, rObjects=rObjects)
    
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
    args[["colour_by"]] <- deparse(slot(x, "colour_by"))
    args[["confidence.level"]] <- deparse(slot(x, "confidence.level"))
    args[["add.expl.var"]] <- deparse(slot(x, "add.expl.var"))
    args[["add.significance"]] <- deparse(slot(x, "add.significance"))
    
    if( slot(x, "vector_parameters") == TRUE ){
        args[["vec.size"]] <- deparse(slot(x, "vec.size"))
        args[["vec.colour"]] <- deparse(slot(x, "vec.colour"))
        args[["vec.linetype"]] <- deparse(slot(x, "vec.linetype"))
        args[["vec.text"]] <- deparse(slot(x, "vec.text"))
        args[["add.vectors"]] <- deparse(slot(x, "add.vectors"))
    }
    
    if( slot(x, "arrow_parameters") == TRUE ){
        args[["arrow.size"]] <- deparse(slot(x, "arrow.size"))
    }
    
    if( slot(x, "ellipse_parameters") == TRUE ){
        args[["ellipse.linewidth"]] <- deparse(slot(x, "ellipse.linewidth"))
        args[["ellipse.linetype"]] <- deparse(slot(x, "ellipse.linetype"))
        args[["ellipse.alpha"]] <- deparse(slot(x, "ellipse.alpha"))
        args[["add.ellipse"]] <- deparse(slot(x, "add.ellipse"))
    }
    
    if( slot(x, "label_parameters") == TRUE ){
        args[["label.colour"]] <- deparse(slot(x, "label.colour"))
        args[["label.size"]] <- deparse(slot(x, "label.size"))
    }
    
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
        of the correlation between variables of our dataset."
        , .getPanelColor(x))),
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

#' @importFrom iSEE .getEncodedName .selectInput.iSEE .checkboxInput.iSEE .sliderInput.iSEE
#'   .conditionalOnCheckSolo .checkboxGroupInput.iSEE .conditionalOnCheckGroup
#'   .numericInput.iSEE
#' @importFrom methods slot
#' @importFrom SummarizedExperiment colData
.create_visual_box_for_rda <- function(x, se) {
    panel_name <- .getEncodedName(x)
    
    .addSpecificTour(class(x)[1], "dimred", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_dimred + .selectize-control"), intro = "Here, we can select the
            ordination method used.")))})
    .addSpecificTour(class(x)[1], "colour_by", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_colour_by + .selectize-control"), intro = "Here, we can select how
            colours are mapped on the plot.")))})
    .addSpecificTour(class(x)[1], "add.ellipse", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_add\\.ellipse + .selectize-control"), intro = "Here, we can choose
            whether or not to add an ellipse and adjust its style.")))})
    .addSpecificTour(class(x)[1], "add.vectors", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_add\\.vectors"), intro = "Here, we can choose
            whether or not to add vectors")))})
    .addSpecificTour(class(x)[1], "vec.text", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_vec\\.text"), intro = "Here, we can choose
            whether or not to add a box around labels.")))})
    .addSpecificTour(class(x)[1], "confidence.level", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_confidence\\.level"), intro = "Here, we can adjust
            the confidence level.")))})
    .addSpecificTour(class(x)[1], "ellipse.alpha", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_ellipse\\.alpha"), intro = "Here, we can choose
            the ellipse opacity.")))})
    .addSpecificTour(class(x)[1], "add.expl.var", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_add\\.expl\\.var"), intro = "Here, we can choose
            whether or not to to show the explained variance.")))})
    .addSpecificTour(class(x)[1], "add.significance", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_add\\.significance"), intro = "Here, we can choose
            whether or not to add variance in the axes.")))})
    .addSpecificTour(class(x)[1], "ellipse.linewidth", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_ellipse\\.linewidth"), intro = "Here, we can adjust
            the size of outline ellipses.")))})
    .addSpecificTour(class(x)[1], "ellipse.linetype", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_ellipse\\.linetype"), intro = "Here, we can adjust
            the style of outline ellipses.")))})
    .addSpecificTour(class(x)[1], "vec.size", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_vec\\.size"), intro = "Here, we can adjust
            the vectors thickness.")))})
    .addSpecificTour(class(x)[1], "vec.colour", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_vec\\.colour + .selectize-control"), intro = "Here, we can adjust
            the vectors colour.")))})
    .addSpecificTour(class(x)[1], "vec.linetype", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_vec\\.linetype"), intro = "Here, we can adjust
            the vectors style")))})
    .addSpecificTour(class(x)[1], "arrow.size", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_arrow\\.size"), intro = "Here, we can adjust
            the arrows size")))})
    .addSpecificTour(class(x)[1], "label.colour", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_label\\.colour + .selectize-control"), intro = "Here, we can adjust
            the labels colour")))})
    .addSpecificTour(class(x)[1], "label.size", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_label\\.size"), intro = "Here, we can adjust
            the labels size")))})
    .addSpecificTour(class(x)[1], "visual_parameters", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_visual_parameters"), intro = "Here, we can choose
            the visual parameters to adjust")))})
    
    
    # Define what parameters the user can adjust
    collapseBox(paste0(panel_name, "_Visual"),
        title="Visual parameters", open=FALSE,
        
        .checkboxGroupInput.iSEE(x, field="visual_parameters", label="Visual parameters:",
            inline=TRUE, selected=slot(x, "visual_parameters"),
            choices=c("Vector", "Arrow", "Ellipse", "Label")),
        .selectInput.iSEE(x, field="colour_by", label="Color by",
            choices=names(colData(se)), selected=slot(x, "colour_by")),
        .conditionalOnCheckGroup(
            paste0(panel_name, "_visual_parameters"), "Arrow",
                .sliderInput.iSEE(x, field="arrow.size", label="Arrows size",
                    min=0.01, max=0.99, step=0.01, value=slot(x, "arrow.size"))),
        .conditionalOnCheckGroup(
            paste0(panel_name, "_visual_parameters"), "Label",
            list(
                .selectInput.iSEE(x, field="label.colour", label="Labels colour",
                    choices=c("black","purple","pink","blue","green","red",
                    "yellow","orange"), selected=slot(x, "label.colour")),
                .sliderInput.iSEE(x, field="label.size", label="Labels size",
                    min=2.5, max=5.5, step=0.1, value=slot(x, "label.size")))),
        .conditionalOnCheckGroup(
          paste0(panel_name, "_visual_parameters"), "Ellipse",
          list(
              .selectInput.iSEE(x, field="add.ellipse", label="Ellipse style",
                  choices = c("fill", "colour", "FALSE"),
                  selected=slot(x, "add.ellipse")),
              .sliderInput.iSEE(x, field="ellipse.alpha", label="Ellipse opacity",
                  min=0.01, max=0.99, step=0.01, value=slot(x, "ellipse.alpha")),
              .sliderInput.iSEE(x, field="ellipse.linewidth", label="Ellipse outline size",
                  min=0.01, max=0.99, step=0.01, value=slot(x, "ellipse.linewidth")),
              .numericInput.iSEE(x, field="ellipse.linetype", label="Ellipse outline style",
                  value=slot(x, "ellipse.linetype"), min=1, max=6, step=1))),
        .conditionalOnCheckGroup(
            paste0(panel_name, "_visual_parameters"), "Vector",
            list(
                .checkboxInput.iSEE(x, field="add.vectors", label="Add vectors",
                    value=slot(x, "add.vectors")),
                .sliderInput.iSEE(x, field="vec.size", label="Vectors thickness",
                    min=0.01, max=0.99, step=0.01, value=slot(x, "vec.size")),
                .selectInput.iSEE(x, field="vec.colour", label="Vectors colour",
                    choices=c("black","purple","pink","blue","green","red",
                    "yellow","orange"), selected=slot(x, "vec.colour")),
                .numericInput.iSEE(x, field="vec.linetype", label="Vectors style",
                    value=slot(x, "vec.linetype"), min=1, max=6, step=1))),
        .conditionalOnCheckSolo(paste0(panel_name, "_add.vectors"), TRUE,
            .checkboxInput.iSEE(x, field="vec.text", label="Unboxed labels",
                value=slot(x, "vec.text"))))
}
