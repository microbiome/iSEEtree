setClass("RowTreePlot", contains="RowDotPlot")

library(S4Vectors)
setValidity2("RowTreePlot", function(object) {
  msg <- character(0)

  msg <- .validNumberError(msg, object, "LogFC", lower=0, upper=Inf) # must be non-negative.

  if (length(msg)) {
    return(msg)
  }
  TRUE
})

setMethod("initialize", "RowTreePlot",
          function(.Object, LogFC=0, ...)
          {
            callNextMethod(.Object, LogFC=LogFC, ColumnSelectionType="Union", ...)
          })

RowTreePlot <- function(...) {
  new("RowTreePlot", ...)
}

setMethod(".fullName", "RowTreePlot", function(x) "Row tree")
setMethod(".panelColor", "RowTreePlot", function(x) "#8B5A2B")

library(shiny)
setMethod(".defineDataInterface", "RowTreePlot", function(x, se, select_info) {
  plot_name <- .getEncodedName(x)
  list(
    numericInput(paste0(plot_name, "_LogFC"), label="Log-FC threshold",
                 min=0, value=x[["LogFC"]])
  )
})


# choose which fields to display
setMethod(".hideInterface", "PlotRowTree", function(x, field) {
  if (field %in% c("RowSelectionSource", "RowSelectionType",
                   "RowSelectionSaved", "RowSelectionDynamicSource")) {
    TRUE
  } else if (field %in% "ColumnSelectionSource") {
    FALSE
  } else {
    callNextMethod()
  }
})

# creating the observers
setMethod(".createObservers", "RowTreePlot",
          function(x, se, input, session, pObjects, rObjects)
          {
            callNextMethod()

            plot_name <- .getEncodedName(x)

            .createUnprotectedParameterObservers(plot_name,
                                                 fields="LogFC",
                                                 input=input,
                                                 pObjects=pObjects,
                                                 rObjects=rObjects)
          })

# making the plot
#' importFrom miaViz plotRowTree
setMethod(".generateDotPlot", "RowTreePlot", function(x, envir) {

  commands <- "miaViz::plotRowTree(se)"

  eval(parse(text=commands), envir=envir)

  list(commands=commands, contents=envir$tab)
})

# visualising panel
tree_plot <- RowTreePlot(PanelWidth=8L, DataBoxOpen=TRUE)
