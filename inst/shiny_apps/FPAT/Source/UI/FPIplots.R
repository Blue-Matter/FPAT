
FPI_UI <- function(id, label="FPI") {

  ns <- NS(id)
  tagList(
    column(12,
           HTML('<br>'),
           htmlOutput(ns("Intro")),
           HTML('<br>'),
           tabsetPanel(
             tabPanel(h5("Summary",style = "color:black"),
                      fluidRow(
                        column(4,
                               h1('Output Dimension Scores'),
                               plotOutput(ns('plot1'))
                               ),
                        column(4,
                               h1('Input Dimension Scores'),
                               plotOutput(ns('plot2'))
                               ),
                        column(4,
                               h1('Output Scores by TBL'),
                               plotOutput(ns('plot3'))
                        )
                      ), value=1),
             tabPanel(h5("Output by TBL",style = "color:black"),
                      fluidRow(
                        column(4,
                               h1('Indicator 1: Ecology'),
                               plotOutput(ns('plot4'))
                        ),
                        column(4,
                               h1('Indicator 2: Economics'),
                               plotOutput(ns('plot5')),
                               plotOutput(ns('plot6')),
                               plotOutput(ns('plot7')),
                               plotOutput(ns('plot8')),
                               plotOutput(ns('plot9')),
                               plotOutput(ns('plot10'))
                        ),
                        column(4,
                               h1('Indicator 3: Community'),
                               plotOutput(ns('plot11')),
                               plotOutput(ns('plot12')),
                               plotOutput(ns('plot13'))
                        )
                      ),value=2),
             tabPanel(h5("Output by Sector",style = "color:black"), HTML("<br>"), value=2)
           )
    )
  )

}

FPI_Server <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 output$Intro <- renderText({
                   "This panel contains FPI plots"
                   })

                 # Summary plots
                 output$plot1 <- renderPlot({
                   if (!is.null(Info$file)) {
                     output_dim_scores(Info$Summary)
                   }
                 })
                 output$plot2 <- renderPlot({
                   if (!is.null(Info$file)) {
                     input_dim_scores(Info$Summary)
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
               }

  )
}
