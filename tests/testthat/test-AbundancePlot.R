test_that("AbundancePlot", {
  
  data("Tengeler2020", package = "mia")
  tse <- Tengeler2020
  panel <- AbundancePlot()
  
  panel[["rank"]] <- "Kingdom"
  
  expect_identical(.getEncodedName(panel), "AbundancePlotNA")
  expect_identical(.fullName(panel), "Abundance plot")
  expect_identical(.panelColor(panel), "#00E5EE")
  
  expect_s3_class(.defineOutput(panel), "shiny.tag.list")
  expect_match(.generateOutput(panel, tse)[["commands"]][["fun"]],
      'p <- miaViz::plotAbundance(se, rank="Kingdom", add_legend=TRUE,
    use_relative=TRUE)',
      fixed = TRUE)
  
  expect_true(.hideInterface(panel, "RowSelectionSource"))
  expect_false(.multiSelectionResponsive(panel, "row"))
  expect_true(.multiSelectionResponsive(panel, "column"))
  
  expect_contains(slotNames(panel), c("rank", "add_legend", "use_relative"))
  
  expect_contains(.definePanelTour(panel)[[1]],
                  c("#AbundancePlotNA_SelectionBoxOpen"))
  
  expect_s3_class(.create_visual_box_for_abund_plot(panel, tse),
                  "shiny.tag.list")
  
})