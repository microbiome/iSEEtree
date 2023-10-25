#' Row tree plot
#'
#' A dimensionality reduction plot that dynamically recomputes the coordinates for the samples,
#' based on the selected subset of samples (and possibly features) in transmitting panels.
#' All samples in active and saved multiple selections are used here.
#'
#' @section Slot overview:
#' The following slots control the thresholds used in the visualization:
#' \itemize{
#' \item \code{layout}, a string specifying 
#' \item \code{add_legend}, an integer scalar specifying
#' \item \code{colour_tip}, 
#' \item \code{colour_edge_by}, string indicating
#' \item \code{colour_tip}, 
#' \item \code{colour_tip_by}, 
#' }
#'
#' In addition, this class inherits all slots from its parent \linkS4class{ColumnDotPlot},
#' \linkS4class{DotPlot} and \linkS4class{Panel} classes.
#'
#' @section Constructor:
#' \code{RowTreePlot(...)} creates an instance of a RowTreePlot class,
#' where any slot and its value can be passed to \code{...} as a named argument.
#'
#' @author Giulio Benedetti
#' @examples
#' library(mia)
#' data("GlobalPatterns", package = "mia")
#' tse <- GlobalPatterns
#' 
#' rowData(tse)$prevalence <- getPrevalence(tse,
#'                                          detection = 1/100,
#'                                          sort = FALSE,
#'                                          assay.type = "counts",
#'                                          as_relative = TRUE)
#'                                          
#' tse_genus <- mergeFeaturesByPrevalence(tse,
#'                                        rank = "Genus",
#'                                        prevalence = 50/100)
#'                                        
#' tse_genus <- addTaxonomyTree(tse_genus)
#' tse_genus <- transformAssay(tse_genus, method = "relabundance")
#'
#' if (interactive()) {
#'   iSEE(tse_genus)
#' }
#'
#' @name RowTreePlot-class
NULL

#' @export
setClass("RowTreePlot", contains="Panel",
         slots=c(layout="character", add_legend="logical",
                 edge_colour="character", edge_colour_by="character",
                 tip_colour="character", tip_colour_by="character"))

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

#' @importFrom methods callNextMethod
setMethod("initialize", "RowTreePlot", function(.Object, ...) {
  extra_args <- list(...)
  extra_args <- .emptyDefault(extra_args, "layout", "circular")
  extra_args <- .emptyDefault(extra_args, "add_legend", TRUE)
  extra_args <- .emptyDefault(extra_args, "edge_colour", "None")
  extra_args <- .emptyDefault(extra_args, "edge_colour_by", NA_character_)
  extra_args <- .emptyDefault(extra_args, "tip_colour", "None")
  extra_args <- .emptyDefault(extra_args, "tip_colour_by", NA_character_)

  do.call(callNextMethod, c(list(.Object), extra_args))
})

#' @importFrom methods new
RowTreePlot <- function(...) {
  new("RowTreePlot", ...)
}

#' @importFrom TreeSummarizedExperiment rowTreeNames rowData assayNames
setMethod(".defineDataInterface", "RowTreePlot", function(x, se, select_info) {
  tab_name <- .getEncodedName(x)
  
  # Define what parameters the user can adjust
  list(
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

setMethod(".createObservers", "RowTreePlot", function(x, se, input, session, pObjects, rObjects) {
  callNextMethod()
  
  panel_name <- .getEncodedName(x)

  .createProtectedParameterObservers(
    panel_name,
    c("layout", "add_legend", "edge_colour_by", "tip_colour_by"),
    input=input, pObjects=pObjects, rObjects=rObjects
  )
  
  invisible(NULL)
})

setMethod(".fullName", "RowTreePlot", function(x) "Row tree plot")

setMethod(".panelColor", "RowTreePlot", function(x) "#8B5A2B")

setMethod(".defineOutput", "RowTreePlot", function(x) {
  plotOutput(.getEncodedName(x))
})

#' @importFrom miaViz plotRowTree
setMethod(".generateOutput", "RowTreePlot", function(x, se, all_memory, all_contents) {
  plot_env <- new.env()
  plot_env$se <- se
  
  selected <- .processMultiSelections(x, all_memory, all_contents, plot_env)
  
  # simplify this to plotRowTree
  fn_call <- "gg <- %s(se"
  
  extra_args <- list()
  extra_args[["layout"]] <- deparse(slot(x, "layout"))
  extra_args[["add_legend"]] <- deparse(slot(x, "add_legend"))
  extra_args[["edge_colour_by"]] <- deparse(slot(x, "edge_colour_by"))
  extra_args[["tip_colour_by"]] <- deparse(slot(x, "tip_colour_by"))

  extra_args <- paste(sprintf("%s=%s", names(extra_args), unlist(extra_args)), collapse=", ")
  fn_call <- paste(fn_call, extra_args, sep = ", ")
  fn_call <- paste0(fn_call, ")")
  fn_call <- paste(strwrap(fn_call, exdent=4), collapse="\n")

  plot_env$.customFUN <- miaViz::plotRowTree
  tmp_call <- sprintf(fn_call, ".customFUN")
  .textEval(tmp_call, plot_env)
  
  commands <- sprintf(fn_call, "PlotRowTree")
  
  commands <- sub("^gg <- ", "", commands) # to avoid an unnecessary variable.
  list(contents=plot_env$gg, commands=list(select=selected, plot=commands))
})

setMethod(".renderOutput", "RowTreePlot", function(x, se, output, pObjects, rObjects) {
  plot_name <- .getEncodedName(x)
  force(se) # defensive programming to avoid difficult bugs due to delayed evaluation.
  output[[plot_name]] <- renderPlot({
    .retrieveOutput(plot_name, se, pObjects, rObjects)$contents
  })
})

