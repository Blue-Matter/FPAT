
# other plots
output$plot1 <- renderPlot({
  if (!is.null(Info$file)) {
    output_dim_scores(Info$Summary)
  }
})

output$plot3 <- renderPlot({
  if (!is.null(Info$file)) {
    output_scores_TBL(Info$Summary)
  }
})

#  Output by TBL
output$plot4 <- renderPlot({
  if (!is.null(Info$file)) {
    FSHEP(Info$Output_table)
  }
})
output$plot5 <- renderPlot({
  if (!is.null(Info$file)) {
    harvest(Info$Output_table)
  }
})
output$plot6 <- renderPlot({
  if (!is.null(Info$file)) {
    harvest_assets(Info$Output_table)
  }
})
output$plot7 <- renderPlot({
  if (!is.null(Info$file)) {
    risk(Info$Output_table)
  }
})
output$plot8 <- renderPlot({
  if (!is.null(Info$file)) {
    managerial_returns(Info$Output_table)
  }
})
output$plot9 <- renderPlot({
  if (!is.null(Info$file)) {
    trade(Info$Output_table)
  }
})
output$plot10 <- renderPlot({
  if (!is.null(Info$file)) {
    product_form(Info$Output_table)
  }
})
output$plot11 <- renderPlot({
  if (!is.null(Info$file)) {
    post_harvest_perf(Info$Output_table)
  }
})
output$plot12 <- renderPlot({
  if (!is.null(Info$file)) {
    labor_returns(Info$Output_table)
  }
})
output$plot13 <- renderPlot({
  if (!is.null(Info$file)) {
    health_sanit(Info$Output_table)
  }
})


# FPI Input Dimension Scores
output$FPIinput <- renderPlot({
  if (!is.null(Info$file)) {
    baseline_select <- BaseLineSelectServer('baseline2')
    input_dim_scores(Info$Summary, baseline_select, BaseLine)
  }
}, height=600, width=600)


# FPI Output by TBL
output$FPIoutputTBL <- renderPlot({
  if (!is.null(Info$file)) {
    baseline_select <- BaseLineSelectServer('baseline3')
    output_scores_TBL(Info$Summary, baseline_select, BaseLine)
  }
}, height=600, width=600)
