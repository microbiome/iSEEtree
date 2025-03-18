#' Prevalence plot
#'
#' Prevalence plot of all or agglomerated features in a
#' \code{\link[SummarizedExperiment:SummarizedExperiment-constructor]{SummarizedExperiment}}
#' object. The panel implements \code{\link[miaViz:plotAbundance]{plotPrevalence}}
#' to generate the plot.
#'
#' @section Slot overview:
#' The following slots control the thresholds used in the visualization:
#' \itemize{
#' \item \code{detection} \code{Numeric scalar}. Detection threshold between 0
#'   and 1 for absence/presence. (Defualt: \code{0})
#' 
#' \item \code{prevalence} \code{Numeric scalar}. Prevalence threshold between 0
#'   and 1. The required prevalence is strictly greater by default. To
#'   include the limit, set \code{include.lowest} to \code{TRUE}. (Default:
#'   \code{0})
#' 
#' \item \code{assay.type} \code{Character scalar}. The name of the assay to
#'   show. (Default: \code{"relabundance"})
#' 
#' \item \code{rank} \code{Character scalar}. The taxonomic rank to visualise.
#'   (Default: \code{NULL})
#'   
#' \item \code{include.lowest} \code{Logical scalar}. Should features with
#'   prevalence equal to \code{prevalence} be included. (Default: \code{FALSE})
#' }
#'
#' In addition, this class inherits all slots from its parent class
#' \code{\link[iSEE:Panel-class]{Panel}}.
#'
#' @return
#' The \code{PrevalencePlot(...)} constructor creates an instance of an
#' PrevalencePlot class, where any slot and its value can be passed to
#' \code{...} as a named argument.
#'
#' @author Giulio Benedetti
#' @examples
#' # Import TreeSE
#' library(mia)
#' data("Tengeler2020", package = "mia")
#' tse <- Tengeler2020
#' 
#' tse <- transformAssay(tse,
#'                       assay.type = "counts",
#'                       method = "relabundance")
#' 
#' # Store panel into object
#' panel <- PrevalencePlot()
#' # View some adjustable parameters
#' head(slotNames(panel))
#' 
#' # Launch iSEE with custom initial panel
#' if (interactive()) {
#'   iSEE(tse, initial = c(panel))
#' }
#' 
#' @docType methods
#' @name PrevalencePlot
NULL

#' @importFrom S4Vectors setValidity2
setValidity2("PrevalencePlot", function(x) {
    
    msg <- character(0)
    msg <- .singleStringError(msg, x, fields=c("assay.type", "rank"))
    msg <- .validLogicalError(msg, x, fields="include.lowest")
    msg <- .validNumberError(msg, x, "detection", lower=0, upper=1)
    msg <- .validNumberError(msg, x, "prevalence", lower=0, upper=1)
    
    if( length(msg) ){
        return(msg)
    }
    TRUE
})

#' @importFrom methods callNextMethod
setMethod("initialize", "PrevalencePlot", function(.Object, ...) {
    args <- list(...)
    args <- .emptyDefault(args, "detection", 0)
    args <- .emptyDefault(args, "prevalence", 0)
    args <- .emptyDefault(args, "include.lowest", FALSE)
    args <- .emptyDefault(args, "assay.type", "relabundance")
    args <- .emptyDefault(args, "rank", NA_character_)

    do.call(callNextMethod, c(list(.Object), args))
})

#' @export
#' @importFrom methods new
PrevalencePlot <- function(...) {
    new("PrevalencePlot", ...)
}

#' @importFrom methods slot
#' @importFrom SummarizedExperiment assayNames
#' @importFrom mia taxonomyRanks
setMethod(".defineDataInterface", "PrevalencePlot", function(x, se, select_info) {
    panel_name <- .getEncodedName(x)
          
    list(.selectInput.iSEE(x, field="assay.type", label="Assay type:",
            choices=assayNames(se), selected=slot(x, "assay.type")),
        .numericInput.iSEE(x, field="prevalence", label="Prevalence threshold:",
            value=slot(x, "prevalence"), min=0, max=1, step=0.01),
        .checkboxInput.iSEE(x, field="include.lowest", label="Include lowest",
            value=slot(x, "include.lowest")),
        .numericInput.iSEE(x, field="detection", label="Detection threshold:",
            value=slot(x, "detection"), min=0, max=1, step=0.01),
        .selectInput.iSEE(x, field="rank", label="Rank",
            choices=taxonomyRanks(se), selected=slot(x, "rank")))
})

#' @importFrom methods callNextMethod
setMethod(".defineInterface", "PrevalencePlot", function(x, se, select_info) {
    out <- callNextMethod()
    #list(out[1], .create_visual_box_for_prev_plot(x, se), out[-1])
})

setMethod(".createObservers", "PrevalencePlot",
    function(x, se, input, session, pObjects, rObjects) {
    
    callNextMethod()
    panel_name <- .getEncodedName(x)
    
    .createProtectedParameterObservers(panel_name, c("assay.type", "prevalence",
        "detection", "include.lowest"), input=input, pObjects=pObjects,
        rObjects=rObjects)
    
    # .createUnprotectedParameterObservers(panel_name, c(),
    #     input=input, pObjects=pObjects, rObjects=rObjects)
    
    invisible(NULL)
})

setMethod(".fullName", "PrevalencePlot", function(x) "Prevalence plot")
setMethod(".panelColor", "PrevalencePlot", function(x) "grey")

#' @importFrom shiny plotOutput
#' @importFrom shinyWidgets addSpinner
setMethod(".defineOutput", "PrevalencePlot", function(x) {
    panel_name <- .getEncodedName(x)
    
    addSpinner(plotOutput(panel_name,
        height = paste0(slot(x, "PanelHeight"), "px")), color=.panelColor(x))
})

#' @importFrom miaViz plotPrevalence
setMethod(".generateOutput", "PrevalencePlot",
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
    
    args[["assay.type"]] <- deparse(slot(x, "assay.type"))
    args[["rank"]] <- deparse(slot(x, "rank"))
    args[["prevalence"]] <- deparse(slot(x, "prevalence"))
    args[["detection"]] <- deparse(slot(x, "detection"))
    args[["include.lowest"]] <- deparse(slot(x, "include.lowest"))
    
    args <- sprintf("%s=%s", names(args), args)
    args <- paste(args, collapse=", ")
    fun_call <- sprintf("p <- miaViz::plotPrevalence(se, %s)", args)
    
    fun_cmd <- paste(strwrap(fun_call, width = 80, exdent = 4), collapse = "\n")
    plot_out <- .textEval(fun_cmd, panel_env)
    all_cmds[["fun"]] <- fun_cmd
    
    list(commands=all_cmds, plot=plot_out, varname=NULL, contents=NULL)
})

#' @importFrom shiny renderPlot
#' @importFrom methods callNextMethod
setMethod(".renderOutput", "PrevalencePlot",
    function(x, se, output, pObjects, rObjects) {
    
    panel_name <- .getEncodedName(x)
    force(se) # defensive programming to avoid bugs due to delayed evaluation
    
    output[[panel_name]] <- renderPlot({
        .retrieveOutput(panel_name, se, pObjects, rObjects)
    })
    
    callNextMethod()
})

#' @importFrom grDevices pdf dev.off
setMethod(".exportOutput", "PrevalencePlot",
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
setMethod(".hideInterface", "PrevalencePlot", function(x, field) {
    if( field %in% c("SelectionHistory", "RowSelectionRestrict",
        "RowSelectionDynamicSource", "RowSelectionSource") ){
        TRUE
    } else {
        callNextMethod()
    }
})

setMethod(".multiSelectionResponsive", "PrevalencePlot",
    function(x, dim = character(0)) {
    
    if( "column" %in% dim ){
        return(TRUE)
    }
    return(FALSE)
})

#' @importFrom methods callNextMethod
setMethod(".definePanelTour", "PrevalencePlot", function(x) {
    rbind(c(paste0("#", .getEncodedName(x)), sprintf(
        "The <font color=\"%s\">Prevalence Plot</font> panel
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

#' @importFrom methods slot
#' @importFrom mia taxonomyRanks
#' @importFrom SummarizedExperiment rowData
.create_visual_box_for_prev_plot <- function(x, se) {
    
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
    .addSpecificTour(class(x)[1], "decreasing", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_decreasing"), intro = "Here, we can choose
            whether or not to plot by decreasing order.")))})
    .addSpecificTour(class(x)[1], "order_sample_by_row", function(panel_name) {
        data.frame(rbind(c(element = paste0("#", panel_name,
            "_order_sample_by_row + .selectize-control"), intro = "Here, we can
            choose a variable from the <code>rowData</code> to order the plot by.
            It has to be part of the rank selected in the Visual Parameters.")))})
    .addSpecificTour(class(x)[1], "order_sample_by_column", function(panel_name) {
      data.frame(rbind(c(element = paste0("#", panel_name,
            "_order_sample_by_column + .selectize-control"), intro = "Here, we can
            choose a variable from the <code>colData</code> to order the plot by.")))})
    
    # Define what parameters the user can adjust
    collapseBox(paste0(panel_name, "_Visual"),
        title="Visual parameters", open=FALSE,
        # Rank
        .selectInput.iSEE(x, field="rank", label="Rank",
            choices=taxonomyRanks(se), selected=slot(x, "rank")),
        # Colour legend
        .checkboxInput.iSEE(x, field="add_legend", label="View legend",
            value=slot(x, "add_legend")))
    
}

#' @importFrom SummarizedExperiment rowData
#' @importFrom mia taxonomyRanks
#' @importFrom utils stack
.list_taxa <- function(se){
  
    row_data <- as.data.frame(rowData(se)[ , taxonomyRanks(se)])
    names(row_data) <- taxonomyRanks(se)

    tax_opts <- unique(stack(row_data))
    tax_opts <- tax_opts[tax_opts$values != "" & !is.na(tax_opts$ind), ]
    tax_list <- lapply(split(tax_opts$values, tax_opts$ind), sort)

    return(tax_list)
}
