setClassUnion("charlog", c("character", "logical"))

#' @export
setClass("RDAPlot", contains="Panel",
         slots=c(add.ellipse="charlog", colour_by="character", vec.text="logical", add.vectors="logical"))

#' @importFrom S4Vectors setValidity2
setValidity2("RDAPlot", function(x) {
  msg <- character(0)
  
  msg <- .singleStringError(msg, x, fields="colour_by")
  
  msg <- .validLogicalError(msg, x, fields=c("vec.text", "add.vectors"))
  
  if (length(msg)) {
    return(msg)
  }
  TRUE
})

#' @importFrom methods callNextMethod
setMethod("initialize", "RDAPlot", function(.Object, ...) {
  extra_args <- list(...)
  extra_args <- .emptyDefault(extra_args, "add.ellipse", "fill")
  extra_args <- .emptyDefault(extra_args, "colour_by", NA_character_)
  extra_args <- .emptyDefault(extra_args, "vec.text", TRUE)
  extra_args <- .emptyDefault(extra_args, "add.vectors", TRUE)
  
  do.call(callNextMethod, c(list(.Object), extra_args))
})

#' @export
#' @importFrom methods new
RDAPlot <- function(...) {
  new("RDAPlot", ...)
}

#' @importFrom SummarizedExperiment colData
setMethod(".defineInterface", "RDAPlot", function(x, se, select_info) {
  tab_name <- .getEncodedName(x)
  
  # Define what parameters the user can adjust
  collapseBox(paste0(tab_name, "_Visual"),
              title="Visual parameters",
              open=FALSE,
              .selectInput.iSEE(
                x, field="colour_by", label="Color by",
                choices=names(colData(se)), selected=slot(x, "colour_by")
              ),
              .selectInput.iSEE(
                x, field="add.ellipse", label="Ellipse style",
                choices = c("fill", "colour", "FALSE"), selected=slot(x, "add.ellipse")
              ),
              .checkboxInput.iSEE(
                x, field="add.vectors", label="Add vectors", value=slot(x, "add.vectors")
              ),
              .conditionalOnCheckSolo(
                paste0(tab_name, "_add.vectors"), TRUE,
                .checkboxInput.iSEE(x, field="vec.text",
                                    label="Unboxed labels",
                                    value=slot(x, "vec.text"))
              )
  )
})

setMethod(".createObservers", "RDAPlot", function(x, se, input, session, pObjects, rObjects) {
  callNextMethod()
  
  panel_name <- .getEncodedName(x)
  
  .createProtectedParameterObservers(
    panel_name,
    c("add.ellipse", "colour_by", "vec.text", "add.vectors"),
    input=input, pObjects=pObjects, rObjects=rObjects
  )
  
  invisible(NULL)
})

setMethod(".fullName", "RDAPlot", function(x) "RDA plot")

setMethod(".panelColor", "RDAPlot", function(x) "#CD5B45")

setMethod(".defineOutput", "RDAPlot", function(x) {
  plotOutput(.getEncodedName(x))
})

#' @importFrom miaViz plotRowTree
setMethod(".generateOutput", "RDAPlot", function(x, se, all_memory, all_contents) {
  plot_env <- new.env()
  plot_env$se <- se
  
  selected <- .processMultiSelections(x, all_memory, all_contents, plot_env)
  
  # simplify this to plotRowTree
  fn_call <- "gg <- %s(se"
  
  extra_args <- list()
  extra_args[["add.ellipse"]] <- deparse(slot(x, "add.ellipse"))
  extra_args[["colour_by"]] <- deparse(slot(x, "colour_by"))
  extra_args[["vec.text"]] <- deparse(slot(x, "vec.text"))
  extra_args[["add.vectors"]] <- deparse(slot(x, "add.vectors"))
  
  extra_args <- paste(sprintf("%s=%s", names(extra_args), unlist(extra_args)), collapse=", ")
  fn_call <- paste(fn_call, extra_args, sep = ", ")
  fn_call <- paste0(fn_call, ")")
  fn_call <- paste(strwrap(fn_call, exdent=4), collapse="\n")
  
  plot_env$.customFUN <- function(se, ...) miaViz::plotRDA(se, "RDA", ...)
  tmp_call <- sprintf(fn_call, ".customFUN")
  .textEval(tmp_call, plot_env)
  
  commands <- sprintf(fn_call, "plotRDA")

  commands <- sub("^gg <- ", "", commands) # to avoid an unnecessary variable.
  list(contents=plot_env$gg, commands=list(select=selected, plot=commands))
})

setMethod(".renderOutput", "RDAPlot", function(x, se, output, pObjects, rObjects) {
  plot_name <- .getEncodedName(x)
  force(se) # defensive programming to avoid difficult bugs due to delayed evaluation.
  output[[plot_name]] <- renderPlot({
    .retrieveOutput(plot_name, se, pObjects, rObjects)$contents
  })
})
