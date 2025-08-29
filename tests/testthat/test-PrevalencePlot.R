test_that("PrevalencePlot", {
  
  output <- new.env()
  pObjects <- new.env()
  rObjects <- new.env()
  select_info <- list(single = list(feature = "---", sample = "---"),
                      multi = list(row = "---", column = "---"))
  
  data("Tengeler2020", package = "mia")
  tse <- Tengeler2020
  
  panel <- PrevalencePlot()
  
  panel[["include.lowest"]] <- TRUE
  
  expect_identical(.getEncodedName(panel), "PrevalencePlotNA")
  expect_identical(.fullName(panel), "Prevalence plot")
  expect_identical(.panelColor(panel), "grey")
  
  expect_s3_class(.defineInterface(panel, tse, select_info)[[1]][[1]], "shiny.tag.list")
  expect_length(.defineDataInterface(panel, tse, select_info), 6)
  
  expect_s3_class(.defineOutput(panel), "shiny.tag.list")
  # expect_match(.generateOutput(panel, tse)[["commands"]][["fun"]],
  #     'p <- miaViz::plotRowGraph(se, name=\"graph\", assay.type=\"counts\",\n    show.label=FALSE, layout=\"kk\", edge.type=\"fan\", add.legend=TRUE)',
  #     fixed = TRUE)
  
  expect_true(.hideInterface(panel, "ColumnSelectionSource"))
  expect_false(.multiSelectionResponsive(panel, "column"))
  expect_true(.multiSelectionResponsive(panel, "row"))
  
  expect_contains(slotNames(panel), c("detection", "prevalence", "assay.type",
      "rank", "include.lowest", "show.rank"))
  
  expect_contains(.definePanelTour(panel)[[1]],
      c("#PrevalencePlotNA_DataBoxOpen", "#PrevalencePlotNA",
      "#PrevalencePlotNA_SelectionBoxOpen"))
  
  expect_s3_class(.create_visual_box_for_prev_plot(panel, tse), "shiny.tag.list")
  
  expect_null(.renderOutput(panel, tse, output = output, pObjects = pObjects, rObjects = rObjects))
  expect_s3_class(output$PrevalencePlotNA, "shiny.render.function")
  expect_s3_class(output$PrevalencePlotNA_INTERNAL_PanelMultiSelectInfo, "shiny.render.function")
  expect_s3_class(output$PrevalencePlotNA_INTERNAL_PanelSelectLinkInfo, "shiny.render.function")
  
  # expect_identical(.exportOutput(panel, tse), "PrevalencePlotNA.pdf")
  
})
