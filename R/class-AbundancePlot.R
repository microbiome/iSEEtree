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
#' \item \code{add_legend}, a logical indicating if the color legend should appear.
#' }
#'
#' In addition, this class inherits all slots from its parent \linkS4class{Panel} class.
#'
#' @section Constructor:
#' \code{AbundancePlot(...)} creates an instance of a AbundancePlot class,
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
#' @aliases AbundancePlot-class
#'   initialize,AbundancePlot-method
#'
#' @name AbundancePlot
NULL

#' @export
setClass("AbundancePlot", contains="Panel",
         slots=c(rank="character", add_legend="logical"))

#' @importFrom S4Vectors setValidity2
setValidity2("AbundancePlot", function(x) {
  msg <- character(0)
  
  msg <- .singleStringError(msg, x, fields="rank")
  
  msg <- .validLogicalError(msg, x, fields="add_legend")
  
  if (length(msg)) {
    return(msg)
  }
  TRUE
})

#' @importFrom methods callNextMethod
setMethod("initialize", "AbundancePlot", function(.Object, ...) {
  extra_args <- list(...)
  extra_args <- .emptyDefault(extra_args, "rank", NA_character_)
  extra_args <- .emptyDefault(extra_args, "add_legend", TRUE)
  
  do.call(callNextMethod, c(list(.Object), extra_args))
})

#' @export
#' @importFrom methods new
AbundancePlot <- function(...) {
  new("AbundancePlot", ...)
}

#' @importFrom methods slot
#' @importFrom SummarizedExperiment rowData
setMethod(".defineInterface", "AbundancePlot", function(x, se, select_info) {
  tab_name <- .getEncodedName(x)
  
  # Define what parameters the user can adjust
  collapseBox(paste0(tab_name, "_Visual"),
              title="Visual parameters",
              open=FALSE,
              # Tree layout
              .selectInput.iSEE(
                x, field="rank", label="Rank",
                choices=names(rowData(se)), selected=slot(x, "rank")
              ),
              # Colour legend
              .checkboxInput.iSEE(
                x, field="add_legend", label="View legend", value=slot(x, "add_legend")
              )
  )
})

#' @importMethodsFrom iSEE .createObservers
setMethod(".createObservers", "AbundancePlot", function(x, se, input, session, pObjects, rObjects) {
  callNextMethod()
  
  panel_name <- .getEncodedName(x)
  
  .createProtectedParameterObservers(
    panel_name,
    c("rank", "add_legend"),
    input=input, pObjects=pObjects, rObjects=rObjects
  )
  
  invisible(NULL)
})

setMethod(".fullName", "AbundancePlot", function(x) "Abundance plot")

setMethod(".panelColor", "AbundancePlot", function(x) "#00E5EE")

setMethod(".defineOutput", "AbundancePlot", function(x) {
  plotOutput(.getEncodedName(x))
})

#' @importFrom miaViz plotRowTree
setMethod(".generateOutput", "AbundancePlot", function(x, se, all_memory, all_contents) {
  plot_env <- new.env()
  plot_env$se <- se
  
  selected <- .processMultiSelections(x, all_memory, all_contents, plot_env)
  
  # simplify this to plotRowTree
  fn_call <- "gg <- %s(se"
  
  extra_args <- list()
  extra_args[["rank"]] <- deparse(slot(x, "rank"))
  extra_args[["add_legend"]] <- deparse(slot(x, "add_legend"))
  
  extra_args <- paste(sprintf("%s=%s", names(extra_args), unlist(extra_args)), collapse=", ")
  fn_call <- paste(fn_call, extra_args, sep = ", ")
  fn_call <- paste0(fn_call, ")")
  fn_call <- paste(strwrap(fn_call, exdent=4), collapse="\n")
  
  plot_env$.customFUN <- miaViz::plotAbundance
  tmp_call <- sprintf(fn_call, ".customFUN")
  .textEval(tmp_call, plot_env)
  
  commands <- sprintf(fn_call, "AbundancePlot")
  
  commands <- sub("^gg <- ", "", commands) # to avoid an unnecessary variable.
  list(contents=plot_env$gg, commands=list(select=selected, plot=commands))
})

#' @importMethodsFrom iSEE .renderOutput
#' @importFrom iSEE .getEncodedName .retrieveOutput
#' @importFrom shiny renderPlot
setMethod(".renderOutput", "AbundancePlot", function(x, se, output, pObjects, rObjects) {
  plot_name <- .getEncodedName(x)
  force(se) # defensive programming to avoid difficult bugs due to delayed evaluation.
  output[[plot_name]] <- renderPlot({
    .retrieveOutput(plot_name, se, pObjects, rObjects)$contents
  })
})