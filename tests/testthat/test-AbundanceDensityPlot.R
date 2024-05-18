test_that("AbundanceDensityPlot", {
  
  data("Tengeler2020", package = "mia")
  tse <- Tengeler2020
  panel <- AbundanceDensityPlot()
  
  expect_identical(.getEncodedName(panel), "AbundanceDensityPlotNA")
  expect_identical(.fullName(panel), "Abundance density plot")
  expect_identical(.panelColor(panel), "#8B5A2B")
  
  expect_s3_class(.defineOutput(panel), "shiny.tag.list")
  expect_match(.generateOutput(panel, tse)[["commands"]][["fun"]],
    'p <- miaViz::plotAbundanceDensity(se, layout="jitter", add_legend=TRUE,\n',
    'assay.type="counts", n=5)', fixed = TRUE)
  
  expect_true(.hideInterface(panel, "ColumnSelectionSource"))
  expect_false(.multiSelectionResponsive(panel, "column"))
  expect_true(.multiSelectionResponsive(panel, "row"))
  
  expect_contains(slotNames(panel),
                  c("layout", "assay.type", "n", "dots_colour",
                    "dots_colour_by", "add_legend"))
  
  expect_contains(.definePanelTour(panel)[[1]],
                  c("#AbundanceDensityPlotNA_SelectionBoxOpen"))
  
  expect_s3_class(.create_visual_box_for_abunddens_plot(panel, tse),
                  "shiny.tag.list")
  
})
