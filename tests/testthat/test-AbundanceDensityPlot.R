test_that("AbundanceDensityPlot", {
  
  output <- new.env()
  pObjects <- new.env()
  rObjects <- new.env()
  select_info <- list(single = list(feature = "---", sample = "---"),
                      multi = list(row = "---", column = "---"))
  
  data("Tengeler2020", package = "mia")
  tse <- Tengeler2020
  panel <- AbundanceDensityPlot()
  
  expect_identical(.getEncodedName(panel), "AbundanceDensityPlotNA")
  expect_identical(.fullName(panel), "Abundance density plot")
  expect_identical(.panelColor(panel), "#8B5A2B")
  
  expect_s3_class(.defineInterface(panel, tse, select_info)[[1]][[1]], "shiny.tag.list")
  expect_length(.defineDataInterface(panel, tse, select_info), 6)
  
  expect_s3_class(.defineOutput(panel), "shiny.tag.list")
  expect_match(.generateOutput(panel, tse)[["commands"]][["fun"]],
    'p <- miaViz::plotAbundanceDensity(se, layout="jitter", add_legend=TRUE,\n',
    'assay.type="counts", n=5)', fixed = TRUE)
  
  expect_true(.hideInterface(panel, "ColumnSelectionSource"))
  expect_false(.multiSelectionResponsive(panel, "column"))
  expect_true(.multiSelectionResponsive(panel, "row"))
  
  expect_contains(slotNames(panel),
                  c("layout", "assay.type", "n", "dots_colour",
                    "dots_colour_by", "add_legend", "flipped",
                    "order_descending", "dots_shape", "dots_shape_by"))
  
  expect_contains(.definePanelTour(panel)[[1]],
                  c("#AbundanceDensityPlotNA_SelectionBoxOpen"))
  
  expect_s3_class(.create_visual_box_for_abunddens_plot(panel, tse),
                  "shiny.tag.list")

  expect_null(.renderOutput(panel, tse, output = output, pObjects = pObjects, rObjects = rObjects))
  expect_s3_class(output$AbundanceDensityPlotNA, "shiny.render.function")
  expect_s3_class(output$AbundanceDensityPlotNA_INTERNAL_PanelMultiSelectInfo, "shiny.render.function")
  expect_s3_class(output$AbundanceDensityPlotNA_INTERNAL_PanelSelectLinkInfo, "shiny.render.function")

  expect_identical(.exportOutput(panel, tse), "AbundanceDensityPlotNA.pdf")
  
})


