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

#' @importFrom methods new
AbundancePlot <- function(...) {
  new("AbundancePlot", ...)
}

#' @importFrom SummarizedExperiment rowData assayNames
#' @importFrom TreeSummarizedExperiment rowTreeNames
#' @importFrom mia taxonomyRanks
setMethod(".defineInterface", "AbundancePlot", function(x, se, select_info) {
  tab_name <- .getEncodedName(x)
  
  # Define what parameters the user can adjust
  collapseBox(paste0(tab_name, "_Visual"),
              title="Visual parameters",
              open=FALSE,
              # Tree layout
              .selectInput.iSEE(
                x, field="rank", label="Rank",
                choices=taxonomyRanks(se), selected=slot(x, "rank")
              ),
              # Colour legend
              .checkboxInput.iSEE(
                x, field="add_legend", label="View legend", value=slot(x, "add_legend")
              )
  )
})

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

setMethod(".panelColor", "AbundancePlot", function(x) "#8B5A2B")

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

setMethod(".renderOutput", "AbundancePlot", function(x, se, output, pObjects, rObjects) {
  plot_name <- .getEncodedName(x)
  force(se) # defensive programming to avoid difficult bugs due to delayed evaluation.
  output[[plot_name]] <- renderPlot({
    .retrieveOutput(plot_name, se, pObjects, rObjects)$contents
  })
})