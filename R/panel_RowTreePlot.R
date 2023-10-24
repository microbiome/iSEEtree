setClass("RowTreePlot", contains="Panel",
         slots=c(tree_name="character", add_legend="logical", layout="character",
                 edge_colour_by="character", tip_colour_by="character",
                 by_exprs_values="character"))

setMethod("initialize", "RowTreePlot", function(.Object, ...) {
  extra_args <- list(...)
  extra_args <- .emptyDefault(extra_args, "tree_name", "phylo")
  extra_args <- .emptyDefault(extra_args, "add_legend", TRUE)
  extra_args <- .emptyDefault(extra_args, "layout", "circular")
  extra_args <- .emptyDefault(extra_args, "edge_colour_by", "none")
  extra_args <- .emptyDefault(extra_args, "tip_colour_by", "none")
  extra_args <- .emptyDefault(extra_args, "by_exprs_values", "counts")
  
  do.call(callNextMethod, c(list(.Object), extra_args))
})

RowTreePlot <- function(...) {
  new("RowTreePlot", ...)
}

setMethod(".defineDataInterface", "RowTreePlot", function(x, se, select_info) {
  tab_name <- .getEncodedName(x)
  collected <- list()
  
  collected[[1]] <- selectInput(
    paste0(tab_name, "_tree_name"), label="Tree",
    choices=rowTreeNames(se), selected=slot(x, "tree_name")
  )
  collected[[2]] <- checkboxInput(
    paste0(tab_name, "_add_legend"), label="add_legend", value=slot(x, "add_legend")
  )
  collected[[3]] <- selectInput(
    paste0(tab_name, "_layout"), label="Layout",
    choices=c("circular", "rectangular", "slanted", "fan", "inward_circular",
              "radial", "unrooted", "equal_angle", "daylight", "dendrogram",
              "ape", "ellipse", "roundrect"), selected=slot(x, "layout")
  )
  collected[[4]] <- selectInput(
    paste0(tab_name, "_edge_colour_by"), label="Color lines by",
    choices=names(rowData(se)), selected=slot(x, "edge_colour_by")
  )
  collected[[5]] <- selectInput(
    paste0(tab_name, "_tip_colour_by"), label="Color nodes by",
    choices=names(rowData(se)), selected=slot(x, "tip_colour_by")
  )
  collected[[6]] <- selectInput(
    paste0(tab_name, "_by_exprs_values"), label="Assay",
    choices=assayNames(se), selected=slot(x, "by_exprs_values")
  )
  
  do.call(tagList, collected)
})

setMethod(".createObservers", "RowTreePlot", function(x, se, input, session, pObjects, rObjects) {
  callNextMethod()
  
  panel_name <- .getEncodedName(x)

  .createProtectedParameterObservers(
    panel_name,
    c("tree_name", "add_legend", "layout", "edge_colour_by", "tip_colour_by", "by_exprs_values"),
    input=input, pObjects=pObjects, rObjects=rObjects
  )
  
  invisible(NULL)
})

setMethod(".fullName", "RowTreePlot", function(x) "Row tree plot")

setMethod(".panelColor", "RowTreePlot", function(x) "#8B5A2B")

setMethod(".defineOutput", "RowTreePlot", function(x) {
  plotOutput(.getEncodedName(x))
})

setMethod(".generateOutput", "RowTreePlot", function(x, se, all_memory, all_contents) {
  plot_env <- new.env()
  plot_env$se <- se
  
  selected <- .processMultiSelections(x, all_memory, all_contents, plot_env)
  
  # simplify this to plotRowTree
  fn_call <- "gg <- %s(se"
  
  extra_args <- list()
  extra_args[["tree_name"]] <- deparse(slot(x, "tree_name"))
  extra_args[["add_legend"]] <- deparse(slot(x, "add_legend"))
  extra_args[["layout"]] <- deparse(slot(x, "layout"))
  extra_args[["edge_colour_by"]] <- deparse(slot(x, "edge_colour_by"))
  extra_args[["tip_colour_by"]] <- deparse(slot(x, "tip_colour_by"))
  extra_args[["by_exprs_values"]] <- deparse(slot(x, "by_exprs_values"))

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

