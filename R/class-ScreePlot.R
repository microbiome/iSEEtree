#' Scree plot
#'
#' Contribution of each reduced dimension component to explained variance. The
#' reduced dimension should be stored in the
#' \code{\link[SingleCellExperiment:reducedDims]{reducedDim}} slot of a
#' \code{\link[SingleSummarizedExperiment:SingleSummarizedExperiment-class]{SingleSummarizedExperiment}}.
#' This panel uses \code{\link[miaViz:plotScree]{plotScree}} to generate the
#' plot.
#'
#' @section Slot overview:
#' The following slots control the thresholds used in the visualisation:
#' \itemize{
#' \item \code{dimred} \code{Character scalar} or \code{integer scalar}.
#'   Determines the reduced dimension to plot. This is used when \code{x} is a
#'   \code{TreeSummarizedExperiment} to extract the eigenvalues from
#'   \code{reducedDim(x, dimred)}.
#'
#' \item \code{show.barplot}: \code{Logical scalar}. Whether to show a barplot.
#'   (Default: \code{TRUE})
#'
#' \item \code{show.points}: \code{Logical scalar}. Whether to show points.
#'   (Default: \code{TRUE})
#'
#' \item \code{show.line}: \code{Logical scalar}. Whether to show lines.
#'   (Default: \code{TRUE})
#'
#' \item \code{show.labels}: \code{Logical scalar}. Whether to show a label for
#'   each point. (Default: \code{FALSE})
#'
#' \item \code{add.proportion}: \code{Logical scalar}. Whether to show proportion of
#'   explained variance, i.e., raw eigenvalues. (Default: \code{TRUE})
#'
#' \item \code{add.cumulative}: \code{Logical scalar}. Whether to show cumulative
#'   explained variance calculated from eigenvalues. (Default: \code{FALSE})
#'
#' \item \code{n}: \code{Integer scalar}. Number of eigenvalues to plot. If
#'   unspecified, all eigenvalues are plotted. (Default: \code{NULL})
#'
#' \item \code{show.names}: \code{Logical scalar}. Whether to show names of the
#'    components on the x-axis. If \code{FALSE}, indices are shown instead.
#'    (Default: \code{FALSE})
#'
#' \item \code{eig.name}: \code{Character scalar}. The name of the attribute in
#'   \code{reducedDim(x, dimred)} that contains the eigenvalues.
#'   (Default: \code{c("eig", "varExplained")})
#' }
#' 
#' In addition, this class inherits all slots from its parent class
#' \code{\link[iSEE:Panel-class]{Panel}}.
#'
#' @return
#' The \code{ScreePlot(...)} constructor creates an instance of an ScreePlot
#' class, where any slot and its value can be passed to \code{...} as a named
#' argument.
#'
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
#' panel <- ScreePlot()
#' # View some adjustable parameters
#' head(slotNames(panel))
#'
#' # Launch iSEE with custom initial panel
#' if (interactive()) {
#'   iSEE(tse, initial = c(panel))
#' }
#' 
#' @seealso
#' \linkS4class{LoadingPlot}
#' \linkS4class{RDAPlot}
#' 
#' @author Giulio Benedetti
#' 
#' @docType methods
#' @name ScreePlot
NULL

#' @importFrom S4Vectors setValidity2
setValidity2("ScreePlot", function(x) {
    msg <- character(0)
    
    msg <- .singleStringError(msg, x, fields=c("dimred", "eig.name"))
    msg <- .validNumberError(msg, x, "n", lower=1, upper=Inf)
    msg <- .validLogicalError(msg, x, fields=c("show.barplot", "show.points",
        "show.line", "show.labels", "add.proportion", "add.cumulative",
        "show.names"))

    if( length(msg) ){
        return(msg)
    }
    
    TRUE
})

#' @importFrom methods callNextMethod
setMethod("initialize", "ScreePlot", function(.Object, ...) {
    args <- list(...)
    args <- .emptyDefault(args, "dimred", "PCA")
    args <- .emptyDefault(args, "show.barplot", TRUE)
    args <- .emptyDefault(args, "show.line", TRUE)
    args <- .emptyDefault(args, "show.labels", FALSE)
    args <- .emptyDefault(args, "add.proportion", TRUE)
    args <- .emptyDefault(args, "add.cumulative", FALSE)
    args <- .emptyDefault(args, "n", 5)
    args <- .emptyDefault(args, "show.names", FALSE)
    args <- .emptyDefault(args, "show.points", TRUE)
    args <- .emptyDefault(args, "eig.name", NA_character_)
    
    do.call(callNextMethod, c(list(.Object), args))
})

#' @export
#' @importFrom methods new
ScreePlot <- function(...) {
    new("ScreePlot", ...)
}

#' @importFrom methods slot
#' @importFrom SingleCellExperiment reducedDim reducedDimNames
setMethod(".defineDataInterface", "ScreePlot",
    function(x, se, select_info) {
    
    panel_name <- .getEncodedName(x)
    
    list(.selectInput.iSEE(x, field="dimred", label="Reduced dimension:",
            choices=reducedDimNames(se), selected=slot(x, "dimred")),
        # Number of components
        .numericInput.iSEE(x, field="n", label="Number of components:",
            value=slot(x, "n"), min=1, step=1,
            max=ncol(reducedDim(se, slot(x, "dimred")))),
        .checkboxInput.iSEE(x, field="show.barplot", label="Show barplot",
            value=slot(x, "show.barplot")),
        .checkboxInput.iSEE(x, field="add.proportion", label="Add proportion",
            value=slot(x, "add.proportion")),
        .checkboxInput.iSEE(x, field="add.cumulative", label="Add cumulative",
            value=slot(x, "add.cumulative")))
})

#' @importFrom methods callNextMethod
setMethod(".defineInterface", "ScreePlot",
    function(x, se, select_info) {
    
    out <- callNextMethod()
    list(out[1], .create_visual_box_for_scree_plot(x, se))
})

setMethod(".createObservers", "ScreePlot",
    function(x, se, input, session, pObjects, rObjects) {
    
    callNextMethod()
    panel_name <- .getEncodedName(x)
    
    .createProtectedParameterObservers(panel_name, c("dimred", "n",
        "show.barplot", "show.points", "show.line"), input=input,
        pObjects=pObjects, rObjects=rObjects)
    
    .createUnprotectedParameterObservers(panel_name, c("show.labels",
        "add.proportion", "add.cumulative", "show.names"), input=input,
        pObjects=pObjects, rObjects=rObjects)
    
    invisible(NULL)
})

setMethod(".fullName", "ScreePlot", function(x) "Scree plot")
setMethod(".panelColor", "ScreePlot", function(x) "#0066CC")

#' @importFrom shiny plotOutput
#' @importFrom shinyWidgets addSpinner
setMethod(".defineOutput", "ScreePlot", function(x) {
    plot_name <- .getEncodedName(x)
    
    addSpinner(
        plotOutput(plot_name, height = paste0(slot(x, "PanelHeight"), "px")),
        color=.panelColor(x))
})

#' @importFrom miaViz plotScree
setMethod(".generateOutput", "ScreePlot",
    function(x, se, all_memory, all_contents) {
    
    panel_env <- new.env()
    all_cmds <- list()
    args <- character(0)
    
    args[["dimred"]] <- deparse(slot(x, "dimred"))
    args[["show.barplot"]] <- deparse(slot(x , "show.barplot"))
    args[["show.points"]] <- deparse(slot(x , "show.points"))
    args[["show.line"]] <- deparse(slot(x , "show.line"))
    args[["add.proportion"]] <- deparse(slot(x , "add.proportion"))
    args[["add.cumulative"]] <- deparse(slot(x , "add.cumulative"))
    args[["show.names"]] <- deparse(slot(x , "show.names"))
    args[["show.labels"]] <- deparse(slot(x , "show.labels"))
    
    if( is.na(slot(x, "n")) || slot(x, "n") <= 0 ){
        args[["n"]] <- 5
    } else if( slot(x, "n") > ncol(reducedDim(se, slot(x, "dimred"))) ){
        args[["n"]] <- ncol(reducedDim(se, slot(x, "dimred")))
    } else {
        args[["n"]] <- deparse(slot(x, "n"))
    }
    
    args <- sprintf("%s=%s", names(args), args)
    args <- paste(args, collapse=", ")
    fun_call <- sprintf("p <- miaViz::plotScree(se, %s)", args)
    
    fun_cmd <- paste(strwrap(fun_call, width = 80, exdent = 4), collapse = "\n")
    plot_out <- .textEval(fun_cmd, panel_env)
    all_cmds[["fun"]] <- fun_cmd
    
    list(commands=all_cmds, plot=plot_out, varname=NULL, contents=NULL)
})

#' @importFrom shiny renderPlot
#' @importFrom methods callNextMethod
setMethod(".renderOutput", "ScreePlot",
            function(x, se, output, pObjects, rObjects) {
    
    plot_name <- .getEncodedName(x)
    force(se) # defensive programming to avoid bugs due to delayed evaluation
    
    output[[plot_name]] <- renderPlot({
        .retrieveOutput(plot_name, se, pObjects, rObjects)
    })
    
    callNextMethod()
})

#' @importFrom grDevices pdf dev.off
setMethod(".exportOutput", "ScreePlot",
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
setMethod(".definePanelTour", "ScreePlot", function(x) {
    rbind(c(paste0("#", .getEncodedName(x)), sprintf(
        "The <font color=\"%s\">Scree Plot</font> panel
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

#' @importFrom methods slot
.create_visual_box_for_scree_plot <- function(x, se) {
    panel_name <- .getEncodedName(x)
    
    .addSpecificTour(class(x)[1], "show.points", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_show\\.points"), intro = "Here, we can choose
            whether or not to show the points.")))})
    .addSpecificTour(class(x)[1], "show.line", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_show\\.line"), intro = "Here, we can choose
            whether or not to connect the points with a line.")))})
    .addSpecificTour(class(x)[1], "show.labels", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_show\\.labels"), intro = "Here, we can choose
            whether or not to show the component labels.")))})
    .addSpecificTour(class(x)[1], "show.names", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_show\\.names"), intro = "Here, we can choose
            whether or not to show the component names.")))})
    
    collapseBox(
        paste0(panel_name, "_Visual"), title="Visual parameters", open=FALSE,
            list(.checkboxInput.iSEE(x, field="show.points",
                label="Show points", value=slot(x, "show.points")),
            .checkboxInput.iSEE(x, field="show.line", label="Show line",
                value=slot(x, "show.line")),
            .checkboxInput.iSEE(x, field="show.labels", label="Show labels",
                value=slot(x, "show.labels")),
            .checkboxInput.iSEE(x, field="show.names", label="Show names",
                value=slot(x, "show.names"))))
}
