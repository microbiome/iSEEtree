tree_slots <- c(layout="character", add_legend="logical",
    edge_colour_by="character", tip_colour_by="character", order_tree="logical",
    tip_size_by="character", edge_size_by="character", tip_shape_by="character",
    node_size_by="character", node_shape_by="character", collapse="character",
    node_colour_by="character", add.node.lab="logical", add.tip.lab="logical",
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
    order_descending="logical", dots_shape="character", dots_shape_by="character"))

