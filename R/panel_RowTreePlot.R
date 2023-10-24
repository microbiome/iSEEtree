FUN <- function(se, rows, columns,
                layout = c("circular", "rectangular", "slanted", "fan",
                           "inward_circular", "radial", "unrooted",
                           "equal_angle", "daylight", "dendrogram", "ape",
                           "ellipse", "roundrect"),
                edge_colour = c(NULL, "Kingdom", "Phylum", "Class"),
                node_colour = c(NULL, "Kingdom", "Phylum", "Class"))
{
  
  kept <- se
  miaViz::plotRowTree(kept, layout = layout, edge_colour_by = edge_colour,
                      node_colour_by = node_colour, tip_colour_by = node_colour)
}
restrict <- NULL

default.args <- formals(FUN)
default.args <- default.args[-seq_len(3)]

# Prune out arguments that we can't support.
keepers <- list()
for (i in names(default.args)) {
  current <- eval(default.args[[i]])
  if ((is.character(current) && length(current)!=0L) ||
      (is.numeric(current) && length(current)==1L) ||
      (is.logical(current) && length(current)==1L))
  {
    keepers[[i]] <- current
  }
}

if (!is.null(restrict)) {
  keepers <- keepers[intersect(names(keepers), restrict)]
}

fn_args <- keepers

collated <- vapply(fn_args, class, "")
setClass("RowTreePlot", contains="Panel", slots=collated)

setMethod("initialize", "RowTreePlot", function(.Object, ...) {
  extra_args <- list(...)
  for (i in names(fn_args)) {
    extra_args <- .emptyDefault(extra_args, i, fn_args[[i]][1]) # select first element when multiple choice.
  }
  do.call(callNextMethod, c(list(.Object), extra_args))
})

RowTreePlot <- function(...) {
  new("RowTreePlot", ...)
}

setMethod(".defineDataInterface", "RowTreePlot", function(x, se, select_info) {
  tab_name <- .getEncodedName(x)
  collected <- list()
  
  for (i in names(fn_args)) {
    options <- fn_args[[i]]
    current <- slot(x, i)
    id <- paste0(tab_name, "_", i)
    
    collected[[i]] <- if (is.character(options) && length(options)==1L) {
      textInput(id, label=i, value=current)
    } else if (is.character(options) && length(options) >= 1L) {
      selectInput(id, label=i, choices=options, selected=current)
    } else if (is.numeric(options)) {
      numericInput(id, label=i, value=current)
    } else if (is.logical(options)) {
      checkboxInput(id, label=i, value=current)
    }
  }
  
  do.call(tagList, collected)
})

setMethod(".createObservers", "RowTreePlot", function(x, se, input, session, pObjects, rObjects) {
  callNextMethod()
  
  panel_name <- .getEncodedName(x)
  
  # Doesn't matter all that much whether they're protected or not,
  # given that custom panels cannot transmit.
  .createProtectedParameterObservers(panel_name, names(fn_args),
                                     input=input, pObjects=pObjects, rObjects=rObjects)
  
  invisible(NULL)
})

setMethod(".fullName", "RowTreePlot", function(x) "Row tree plot")

setMethod(".panelColor", "RowTreePlot", function(x) "#8B5A2B")

setMethod(".defineOutput", "RowTreePlot", function(x) {
  plotOutput(.getEncodedName(x))
})

fn_name <- "plotRowTree"

setMethod(".generateOutput", "RowTreePlot", function(x, se, all_memory, all_contents) {
  plot_env <- new.env()
  plot_env$se <- se
  
  selected <- .processMultiSelections(x, all_memory, all_contents, plot_env)
  
  # simplify this to plotRowTree
  fn_call <- "gg <- %s(se"
  
  if (exists("row_selected", plot_env, inherits=FALSE)) {
    fn_call <- paste(fn_call, ", row_selected")
  } else {
    fn_call <- paste(fn_call, ", NULL")
  }
  
  if (exists("col_selected", plot_env, inherits=FALSE)) {
    fn_call <- paste(fn_call, ", col_selected")
  } else {
    fn_call <- paste(fn_call, ", NULL")
  }
  
  extra_args <- list()
  for (i in names(fn_args)) {
    extra_args[[i]] <- deparse(slot(x, i))
  }
  extra_args <- paste(sprintf("%s=%s", names(extra_args), unlist(extra_args)), collapse=", ")
  
  if (!identical(extra_args, "")) {
    fn_call <- paste(fn_call, extra_args, sep = ", ")
  }
  
  fn_call <- paste0(fn_call, ")")
  fn_call <- paste(strwrap(fn_call, exdent=4), collapse="\n")
  
  # Not using 'fn_name' to assign to 'envir', to avoid potentially
  # overwriting important variables like 'se' with arbitrary user names.
  plot_env$.customFUN <- FUN
  tmp_call <- sprintf(fn_call, ".customFUN")
  .textEval(tmp_call, plot_env)
  
  commands <- sprintf(fn_call, fn_name)
  
  commands <- sub("^gg <- ", "", commands) # to avoid an unnecessary variable.
  list(contents=plot_env$gg, commands=list(select=selected, plot=commands))
})

setMethod(".renderOutput", "RowTreePlot", function(x, se, output, pObjects, rObjects) {
  plot_name <- .getEncodedName(x)
  force(se) # defensive programming to avoid difficult bugs due to delayed evaluation.
  
  # nocov start
  output[[plot_name]] <- renderPlot({
    .retrieveOutput(plot_name, se, pObjects, rObjects)$contents
  })
  # nocov end
  
})
