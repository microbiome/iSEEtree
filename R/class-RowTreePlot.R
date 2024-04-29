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
#' \item \code{colour_edge}, a string specifying color mapping for tree lines.
#' \item \code{colour_edge_by}, a string specifying parameter to color lines by
#'   when \code{colour_edge = "Row data"}.
#' \item \code{colour_tip}, a string specifying color mapping for tree nodes.
#' \item \code{colour_tip_by}, a string specifying parameter to color nodes by
#'   when \code{colour_tip = "Row data"}.
#' }
#'
#' In addition, this class inherits all slots from its parent \linkS4class{Panel} class.
#'
#' @section Constructor:
#' \code{RowTreePlot(...)} creates an instance of a RowTreePlot class,
#' where any slot and its value can be passed to \code{...} as a named argument.
#'
#' @author Giulio Benedetti
#' @examples
#' # Import TreeSE
#' library(mia)
#' data("GlobalPatterns", package = "mia")
#' tse <- GlobalPatterns
#' 
#' # Agglomerate TreeSE by Genus
#' tse_genus <- agglomerateByRank(tse,
#'                                rank = "Genus",
#'                                onRankOnly = TRUE)
#'
#' # Launch iSEE
#' if (interactive()) {
#'   iSEE(tse_genus)
#' }
#' 
#' @docType methods
#' @aliases RowTreePlot-class
#'   initialize,RowTreePlot-method
#'
#' @name RowTreePlot
NULL

#' @export
setClass("RowTreePlot", contains="Panel",
         slots=c(layout="character", add_legend="logical",
                 edge_colour="character", edge_colour_by="character",
                 tip_colour="character", tip_colour_by="character",
                 open_visual_box="logical"))

#' @importFrom iSEE .singleStringError .validLogicalError
#' @importFrom S4Vectors setValidity2
setValidity2("RowTreePlot", function(x) {
  msg <- character(0)
  
  msg <- .singleStringError(msg, x,
    fields=c("layout", "edge_colour", "edge_colour_by", "tip_colour", "tip_colour_by")
  )
  
  msg <- .validLogicalError(msg, x, fields="add_legend")
  
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
  args <- .emptyDefault(args, "edge_colour", "None")
  args <- .emptyDefault(args, "edge_colour_by", NA_character_)
  args <- .emptyDefault(args, "tip_colour", "None")
  args <- .emptyDefault(args, "tip_colour_by", NA_character_)
  args <- .emptyDefault(args, "open_visual_box", FALSE)

  do.call(callNextMethod, c(list(.Object), args))
})

#' @export
#' @importFrom methods new
RowTreePlot <- function(...) {
  new("RowTreePlot", ...)
}

#' @importFrom iSEE .getEncodedName .selectInput.iSEE .checkboxInput.iSEE
#'   .radioButtons.iSEE .conditionalOnRadio .addSpecificTour
#' @importFrom SummarizedExperiment rowData assayNames
#' @importFrom TreeSummarizedExperiment rowTreeNames
setMethod(".defineInterface", "RowTreePlot", function(x, se, select_info) {
  tab_name <- .getEncodedName(x)
  
  .addSpecificTour(class(x)[1], "layout", function(plot_name) {
      data.frame(rbind(c(
          element = paste0("#", plot_name, "_layout + .selectize-control"),
          intro = "Here, we can select the layout of the tree."
      )))}
  )
  
  .addSpecificTour(class(x)[1], "add_legend", function(plot_name) {
      data.frame(rbind(c(
          element = paste0("#", plot_name, "_add_legend + .selectize-control"),
          intro = "Here, we can choose whether or not to show the legend."
      )))}
  )
  
  .addSpecificTour(class(x)[1], "edge_colour", function(plot_name) {
      data.frame(rbind(c(
          element = paste0("#", plot_name, "_edge_colour + .selectize-control"),
          intro = "Here, we can choose whether or not to colour the lines by
          one of the variables in the <code>rowData</code>. When activated, the
          available options are listed and one of them can be selected."
      )))}
  )
  
  .addSpecificTour(class(x)[1], "tip_colour", function(plot_name) {
      data.frame(rbind(c(
          element = paste0("#", plot_name, "_tip_colour + .selectize-control"),
          intro = "Here, we can choose whether or not to colour the nodes by
          one of the variables in the <code>rowData</code>. When activated, the
          available options are listed and one of them can be selected."
      )))}
  )

  # Define what parameters the user can adjust
  collapseBox(paste0(tab_name, "_Visual"),
                     title="Visual parameters",
                     open=FALSE,
    # Tree layout
    .selectInput.iSEE(
      x, field="layout", label="Layout",
      choices=c("circular", "rectangular", "slanted", "fan", "inward_circular",
                "radial", "unrooted", "equal_angle", "daylight", "dendrogram",
                "ape", "ellipse", "roundrect"), selected=slot(x, "layout")
    ),
    # Colour legend
    .checkboxInput.iSEE(
      x, field="add_legend", label="View legend", value=slot(x, "add_legend")
    ),
    .radioButtons.iSEE(
      x, field="edge_colour", label="Line color:", inline=TRUE,
      choices=c("None", "Row data"), selected=slot(x, "edge_colour")
    ),
    .conditionalOnRadio(
      paste0(tab_name, "_edge_colour"), "Row data",
      iSEE:::.selectInputHidden(x, field="edge_colour_by",
                                label="Color lines by",
                                choices=names(rowData(se)), 
                                selected=slot(x, "edge_colour_by"))
    ),
    .radioButtons.iSEE(
      x, field="tip_colour", label="Node color:", inline=TRUE,
      choices=c("None", "Row data"), selected=slot(x, "tip_colour")
    ),
    .conditionalOnRadio(
      paste0(tab_name, "_tip_colour"), "Row data",
      iSEE:::.selectInputHidden(x, field="tip_colour_by",
                                label="Color nodes by",
                                choices=names(rowData(se)), 
                                selected=slot(x, "tip_colour_by"))
    )
  )
})

#' @importFrom iSEE .getEncodedName .createProtectedParameterObservers
setMethod(".createObservers", "RowTreePlot", function(x, se, input, session, pObjects, rObjects) {
  callNextMethod()
  
  panel_name <- .getEncodedName(x)

  .createProtectedParameterObservers(
    panel_name,
    c("layout", "add_legend", "edge_colour", "edge_colour_by", "tip_colour", "tip_colour_by"),
    input=input, pObjects=pObjects, rObjects=rObjects
  )
  
  invisible(NULL)
})

setMethod(".fullName", "RowTreePlot", function(x) "Row tree plot")

setMethod(".panelColor", "RowTreePlot", function(x) "#4EEE94")

#' @importFrom iSEE .getEncodedName
#' @importFrom shiny plotOutput
setMethod(".defineOutput", "RowTreePlot", function(x) {
  plotOutput(.getEncodedName(x))
})

#' @importFrom iSEE .processMultiSelections .textEval
#' @importFrom miaViz plotRowTree
setMethod(".generateOutput", "RowTreePlot", function(x, se, all_memory, all_contents) {
  plot_env <- new.env()
  plot_env$se <- se
  
  selected <- .processMultiSelections(x, all_memory, all_contents, plot_env)
  
  # simplify this to plotRowTree
  fn_call <- "gg <- %s(se"
  
  args <- list()
  args[["layout"]] <- deparse(slot(x, "layout"))
  args[["add_legend"]] <- deparse(slot(x, "add_legend"))
  if (slot(x, "edge_colour") == "Row data") {
    args[["edge_colour_by"]] <- deparse(slot(x, "edge_colour_by"))
  }
  if (slot(x, "tip_colour") == "Row data") {
    args[["tip_colour_by"]] <- deparse(slot(x, "tip_colour_by"))
  }

  args <- paste(sprintf("%s=%s", names(args), unlist(args)), collapse=", ")
  fn_call <- paste(fn_call, args, sep = ", ")
  fn_call <- paste0(fn_call, ")")
  fn_call <- paste(strwrap(fn_call, exdent=4), collapse="\n")

  plot_env$.customFUN <- miaViz::plotRowTree
  tmp_call <- sprintf(fn_call, ".customFUN")
  .textEval(tmp_call, plot_env)
  
  commands <- sprintf(fn_call, "PlotRowTree")
  
  commands <- sub("^gg <- ", "", commands) # to avoid an unnecessary variable.
  list(contents=plot_env$gg, commands=list(select=selected, plot=commands))
})

#' @importFrom iSEE .getEncodedName .retrieveOutput
#' @importFrom shiny renderPlot
setMethod(".renderOutput", "RowTreePlot", function(x, se, output, pObjects, rObjects) {
  plot_name <- .getEncodedName(x)
  force(se) # defensive programming to avoid difficult bugs due to delayed evaluation.
  output[[plot_name]] <- renderPlot({
    .retrieveOutput(plot_name, se, pObjects, rObjects)$contents
  })
})

#' @importFrom iSEE .getEncodedName .getPanelColor .addTourStep
setMethod(".definePanelTour", "RowTreePlot", function(x) {
  rbind(
    c(paste0("#", .getEncodedName(x)), sprintf(
      "The <font color=\"%s\">RowTreePlot</font> panel contains a phylogenetic
      tree from the <i><a href='https://microbiome.github.io/miaViz/reference/plotTree.html'>miaViz</a></i>
      package.", .getPanelColor(x))),
    .addTourStep(x, "open_visual_box", "The <i>Visual parameters</i> box shows
                 the available visual parameters that can be tweaked in this
                 tree.<br/><br/><strong>Action:</strong> click on this box to
                 open up available options."),
    callNextMethod()
  )
})
