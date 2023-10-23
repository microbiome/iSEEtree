library(miaViz)
CUSTOM_TREE <- function(se, rows, columns,
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

TREE <- createCustomPlot(CUSTOM_TREE,
                         className = "RowTreePlot",
                         fullName = "Row tree plot")
TREE()

if (interactive()) {
  library(mia)
  data("GlobalPatterns", package = "mia")
  tse <- GlobalPatterns
  tse_genus <- mergeFeaturesByPrevalence(tse,
                                         rank = "Genus",
                                         prevalence = 50/100)
  tse_genus <- addTaxonomyTree(tse_genus)
  
  iSEE(tse_genus, initial=list(
    RowDataPlot(PanelId=1L),
    TREE()
  ))
}
