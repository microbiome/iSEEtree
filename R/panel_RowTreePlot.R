setClass("RowTreePlot", contains="RowDotPlot",
         slots = c(NodeColour = "character"))

library(S4Vectors)
setValidity2("RowTreePlot", function(object) {
  msg <- character(0)

  msg <- .validStringError(msg, object, "NodeColour")

  if (length(msg)) {
    return(msg)
  }
  TRUE
})

setMethod("initialize", "RowTreePlot",
          function(.Object, NodeColour="blue", ...)
          {
            callNextMethod(.Object, NodeColour=NodeColour, ColumnSelectionType="Union", ...)
          })

RowTreePlot <- function(...) {
  new("RowTreePlot", ...)
}

#' importFrom iSEE .fullName
setMethod(".fullName", "RowTreePlot", function(x) "Row tree")
setMethod(".panelColor", "RowTreePlot", function(x) "#8B5A2B")

library(shiny)
setMethod(".defineDataInterface", "RowTreePlot", function(x, se, select_info) {
  plot_name <- .getEncodedName(x)
  list(
    textInput(paste0(plot_name, "_NodeColour"),
              label="Node colour",
              value=x[["NodeColour"]])
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

            # instructs iSEE which params will break the plot
            .createUnprotectedParameterObservers(plot_name,
                                                 fields="NodeColour",
                                                 input=input,
                                                 pObjects=pObjects,
                                                 rObjects=rObjects)
          })

# making the plot
setMethod(".generateDotPlotData", "RowTreePlot", function(x, envir) {
  commands <- character(0)

  commands <- c(commands,
                "plot.data <- data.frame(X=numeric(0), Y=numeric(0));")

  commands <- c(commands,
                "plot.data <- plot.data[colnames(se),,drop=FALSE];",
                "rownames(plot.data) <- colnames(se);")

  eval(parse(text=commands), envir=envir)

  list(data_cmds=commands, plot_title="Tree plot",
       x_lab="a", y_lab="b")
})

#' importFrom miaViz plotRowTree
setMethod(".generateDotPlot", "RowTreePlot", function(x, envir) {

  commands <- "miaViz::plotRowTree(se)"

  eval(parse(text=commands), envir=envir)

  list(commands=commands, contents=envir$tab)
})

# visualising panel
tree_plot <- RowTreePlot(PanelWidth=8L, DataBoxOpen=TRUE)

