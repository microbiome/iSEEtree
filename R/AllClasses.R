tree_slots <- c(layout="character", add.legend="logical",
    edge.colour.by="character", tip.colour.by="character", order.tree="logical",
    tip.size.by="character", edge.size.by="character", tip.shape.by="character",
    node.size.by="character", node.shape.by="character", collapse="character",
    node.colour.by="character", add.node.lab="logical", add.tip.lab="logical",
    open.angle="numeric", rotate.angle="numeric", branch.length="logical",
    visual_parameters="character", size_parameters="character",
    shape_parameters="character", colour_parameters="character")

#' @rdname TreePlot
#' @export
setClass("TreePlot", contains=c("Panel", "VIRTUAL"), slots=tree_slots)

#' @rdname RowTreePlot
#' @export
setClass("RowTreePlot", contains="TreePlot")

#' @rdname ColumnTreePlot
#' @export
setClass("ColumnTreePlot", contains="TreePlot")

graph_slots <- c(name="character", assay.type="character", layout="character",
    edge.type="character", show.label="logical", add.legend="logical",
    edge.colour.by="character", edge.size.by="character",
    node.colour.by="character", node.shape.by="character",
    node.size.by="character", visual_parameters="character",
    size_parameters="character", shape_parameters="character",
    colour_parameters="character")

#' @rdname GraphPlot
#' @export
setClass("GraphPlot", contains=c("Panel", "VIRTUAL"), slots=graph_slots)

#' @rdname RowGraphPlot
#' @export
setClass("RowGraphPlot", contains="GraphPlot")

#' @rdname ColumnGraphPlot
#' @export
setClass("ColumnGraphPlot", contains="GraphPlot")

setClassUnion("charlog", c("character", "logical"))

#' @rdname RDAPlot
#' @export
setClass("RDAPlot", contains="Panel", slots=c(dimred="character",
    add.ellipse="charlog", colour_by="character", vec.text="logical",
    add.vectors="logical", ellipse.alpha="numeric", confidence.level="numeric",
    add.significance="logical", add.expl.var="logical", ellipse.linewidth="numeric",
    ellipse.linetype="numeric", vec.size="numeric", vec.colour="character",
    vec.linetype="numeric", arrow.size="numeric", label.colour="character",
    label.size="numeric", visual_parameters="character"))

#' @rdname LoadingPlot
#' @export
setClass("LoadingPlot", contains="Panel", slots=c(dimred="character",
    layout="character", ncomponents="numeric", add.tree="logical"))

#' @rdname ScreePlot
#' @export
setClass("ScreePlot", contains="Panel", slots=c(dimred="character",
    show.barplot="logical", show.points="logical", show.line="logical",
    show.labels="logical", add.proportion="logical", add.cumulative="logical",
    n="numeric", show.names="logical", eig.name="character"))

#' @rdname AbundancePlot
#' @export
setClass("AbundancePlot", contains="Panel",
    slots=c(rank="character", use_relative="logical", add_legend="logical",
    order_sample_by_row="character", order_sample="character", 
    decreasing="logical", order_sample_by_column="character"))

#' @rdname AbundanceDensityPlot
#' @export
setClass("AbundanceDensityPlot", contains="Panel", slots=c(layout="character",
    assay.type="character", n="numeric", dots_colour="character",
    dots_colour_by="character", add_legend="logical", flipped="logical",
    order_descending="logical", dots_shape="character",
    dots_shape_by="character"))

