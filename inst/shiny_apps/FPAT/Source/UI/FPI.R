
BaseLineSelectUI <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::selectInput(ns('baseline'), 'Baseline Comparision',
                       choices=BaseLineChoices,
                       multiple = TRUE)
  )
}

BaseLineSelectServer <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 input$baseline

               }
  )
}

FPI_UI <- function(id, label="Inputs") {

  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(12,
             HTML('<br>'),
             htmlOutput(ns("Intro")),
             HTML('<br>'),

             tabsetPanel(

               # Output Dimension Scores
               tabPanel(h5('Outputs', style='color:black;'),
                        value=1,
                        br(),
                        column(4,
                               h4('Fishery Performance Indicators: Outputs (Measuring Wealth)'),
                               plotOutput(ns('FPIoutput'), width='100%', height="100%")
                        ),
                        column(3,
                               BaseLineSelectUI(ns('baseline1'))
                        ),
                        column(5,
                               h4('Other explanatory text or tables can go here')
                        )
               ),

               # Input Dimension Scores
               tabPanel(h5('Inputs', style='color:black;'),
                        value=2,
                        br(),
                        column(4,
                               h4('Fishery Performance Indicators: Inputs (Enabling Wealth Creation)'),
                               plotOutput(ns('FPIinput'), width='100%', height="100%")
                        ),
                        column(3,
                               BaseLineSelectUI(ns('baseline2'))

                        ),
                        column(5,
                               h4('Other explanatory text or tables can go here')
                        )
               ),

               # Output Scores by TBL
               tabPanel(h5('Output by TBL', style='color:black;'),
                        value=3,
                        br(),
                        column(4,
                               h4('Fishery Performance Indicators: Outputs by TBL Indicator'),
                               plotOutput(ns('FPIoutputTBL'), width='100%', height="100%")
                        ),
                        column(3,
                               BaseLineSelectUI(ns('baseline3'))

                        ),
                        column(5,
                               h4('Other explanatory text or tables can go here')
                        )
               ),

               # other plots
               tabPanel(h5('Other FPI Plots', style='color:black;'),
                        value=4,
                        tabsetPanel(
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
      )

    )
  )
}






FPI_Server <- function(id, Info) {
  moduleServer(id,
     function(input, output, session) {

       output$Intro <- renderText({
         "This panel contains figures with summary FPI scores and option to add baseline comparisions"
       })

       # FPI Output Dimension Scores
       output$FPIoutput <- renderPlot({
         if (!is.null(Info$file)) {
           baseline_select <- BaseLineSelectServer('baseline1')
           output_dim_scores(Info$Summary, baseline_select, BaseLine)
         }
         }, height=600, width=600)

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

     }
  )
}
