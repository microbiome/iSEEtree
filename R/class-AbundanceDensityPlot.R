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
#' }
#'
#' In addition, this class inherits all slots from its parent \linkS4class{Panel} class.
#'
#' @section Constructor:
#' \code{AbundanceDensityPlot(...)} creates an instance of an AbundanceDensityPlot
#' class, where any slot and its value can be passed to \code{...} as a named argument.
#'
#' @author Giulio Benedetti
#' @examples
#' # Import TreeSE
#' library(mia)
#' data("GlobalPatterns", package = "mia")
#' tse <- GlobalPatterns
#' 
#' # Agglomerate TreeSE by Genus
#' tse_genus <- mergeFeaturesByRank(tse,
#'                                  rank = "Genus",
#'                                  onRankOnly = TRUE)
#'
#' # Add relabundance assay
#' tse_genus <- transformAssay(tse_genus, method = "relabundance")
#'
#' # Launch iSEE
#' if (interactive()) {
#'   iSEE(tse_genus)
#' }
#'
#' @name AbundanceDensityPlot-class
NULL

#' @export
setClass("AbundanceDensityPlot", contains="Panel",
         slots=c(layout="character", assay.type="character", n="numeric"))

#' @importFrom S4Vectors setValidity2
setValidity2("AbundanceDensityPlot", function(x) {
  msg <- character(0)
  
  msg <- .singleStringError(msg, x,
                            fields=c("layout", "assay.type")
  )

  msg <- .validNumberError(msg, x, "n", lower=1, upper=Inf)
  
  if (length(msg)) {
    return(msg)
  }
  TRUE
})

#' @importFrom methods callNextMethod
setMethod("initialize", "AbundanceDensityPlot", function(.Object, ...) {
  extra_args <- list(...)
  extra_args <- .emptyDefault(extra_args, "layout", "jitter")
  extra_args <- .emptyDefault(extra_args, "assay.type", "counts")
  extra_args <- .emptyDefault(extra_args, "n", 5)
  
  do.call(callNextMethod, c(list(.Object), extra_args))
})

#' @export
#' @importFrom methods new
AbundanceDensityPlot <- function(...) {
  new("AbundanceDensityPlot", ...)
}

#' @importFrom SummarizedExperiment rowData assayNames
#' @importFrom TreeSummarizedExperiment rowTreeNames
setMethod(".defineInterface", "AbundanceDensityPlot", function(x, se, select_info) {
  tab_name <- .getEncodedName(x)
  
  # Define what parameters the user can adjust
  collapseBox(paste0(tab_name, "_Visual"),
              title="Visual parameters",
              open=FALSE,
              # Tree layout
              .selectInput.iSEE(
                x, field="layout", label="Layout",
                choices=c("jitter", "density", "point"), selected=slot(x, "layout")
              ),
              .selectInput.iSEE(
                x, field="assay.type", label="Assay type",
                choices=assayNames(se), selected=slot(x, "assay.type")
              ),
              # Number of taxa
              .numericInput.iSEE(
                x, field="n", label="Number of taxa", value=slot(x, "n")
              )
  )
})

setMethod(".createObservers", "AbundanceDensityPlot", function(x, se, input, session, pObjects, rObjects) {
  callNextMethod()
  
  panel_name <- .getEncodedName(x)
  
  .createProtectedParameterObservers(
    panel_name,
    c("layout", "assay.type", "n"),
    input=input, pObjects=pObjects, rObjects=rObjects
  )
  
  invisible(NULL)
})

setMethod(".fullName", "AbundanceDensityPlot", function(x) "Abundance density plot")

setMethod(".panelColor", "AbundanceDensityPlot", function(x) "#8B5A2B")

setMethod(".defineOutput", "AbundanceDensityPlot", function(x) {
  plotOutput(.getEncodedName(x))
})

#' @importFrom miaViz plotRowTree
setMethod(".generateOutput", "AbundanceDensityPlot", function(x, se, all_memory, all_contents) {
  plot_env <- new.env()
  plot_env$se <- se
  
  selected <- .processMultiSelections(x, all_memory, all_contents, plot_env)
  
  # simplify this to plotRowTree
  fn_call <- "gg <- %s(se"
  
  extra_args <- list()
  extra_args[["layout"]] <- deparse(slot(x, "layout"))
  extra_args[["assay.type"]] <- deparse(slot(x, "assay.type"))
  extra_args[["n"]] <- deparse(slot(x, "n"))

  extra_args <- paste(sprintf("%s=%s", names(extra_args), unlist(extra_args)), collapse=", ")
  fn_call <- paste(fn_call, extra_args, sep = ", ")
  fn_call <- paste0(fn_call, ")")
  fn_call <- paste(strwrap(fn_call, exdent=4), collapse="\n")
  
  plot_env$.customFUN <- miaViz::plotAbundanceDensity
  tmp_call <- sprintf(fn_call, ".customFUN")
  .textEval(tmp_call, plot_env)
  
  commands <- sprintf(fn_call, "AbundanceDensityPlot")
  
  commands <- sub("^gg <- ", "", commands) # to avoid an unnecessary variable.
  list(contents=plot_env$gg, commands=list(select=selected, plot=commands))
})

setMethod(".renderOutput", "AbundanceDensityPlot", function(x, se, output, pObjects, rObjects) {
  plot_name <- .getEncodedName(x)
  force(se) # defensive programming to avoid difficult bugs due to delayed evaluation.
  output[[plot_name]] <- renderPlot({
    .retrieveOutput(plot_name, se, pObjects, rObjects)$contents
  })
})
