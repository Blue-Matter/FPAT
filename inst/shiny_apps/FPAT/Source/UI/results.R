


Results_UI <- function(id, label="Results") {

  ns <- NS(id)
  tagList(
    column(12,style="height: 700px",
       HTML('<br>'),
       htmlOutput(ns("Intro")),
       HTML('<br>'),
       tabsetPanel(

         tabPanel(h5("Summary",style = "color:black"), HTML("<br>"),value=1,

                  fluidRow(
                    column(4,
                           h4('Output Dimension Scores'),
                           plotOutput(ns('plot1'))
                    ),

                    column(4,
                           h4('Output Scores by TBL'),
                           plotOutput(ns('plot3'))
                    )
                  )

                  ),
         tabPanel(h5("Management performance",style = "color:black"), HTML("<br>"),value=2,
                  h4("About MSE results..."),
                  tabsetPanel(

                    tabPanel(h5("Kobe",style = "color:black"), HTML("<br>"),value=1,
                             column(6,plotOutput(ns('Pplot'))),
                             column(6,plotOutput(ns('Kplot')))

                    ),
                    tabPanel(h5("Trade-offs",style = "color:black"), HTML("<br>"),value=2,
                             column(6,plotOutput(ns('Tradeplot')))

                    )

                  )

                  ),
         tabPanel(h5("Environmental outcomes",style = "color:black"), HTML("<br>"), value=3,
                  h4('Indicator 1: Ecology'),
                  plotOutput(ns('plot4'))

                  ),
         tabPanel(h5("Social outcomes",style = "color:black"), HTML("<br>"), value=4,

                  h4('Indicator 3: Community'),
                  fluidRow(
                    column(4,plotOutput(ns('plot11'))),
                    column(4,plotOutput(ns('plot12'))),
                    column(4,plotOutput(ns('plot13')))
                  )

                  ),
         tabPanel(h5("Economic outcomes",style = "color:black"), HTML("<br>"), value=5,

                  h4('Indicator 2: Economics'),
                  fluidRow(
                    column(4, plotOutput(ns('plot5')),
                              plotOutput(ns('plot6'))),
                    column(4, plotOutput(ns('plot7')),
                              plotOutput(ns('plot8'))),
                    column(4, plotOutput(ns('plot9')),
                              plotOutput(ns('plot10')))
                  )

                  ) # end of economic tabpanel

       ) # end of tabset panel
    ) # end of column 12
  ) # end of taglist

}

Results_Server <- function(id,Info) {
  moduleServer(id,
    function(input, output, session) {

      output$Intro <- renderText({
        "This panel contains the results of the combined FPI - openMSE analyses"
      })

      output$Pplot <- renderPlot({     if (!is.null(Info$file))    Pplot2(Info$MSEproj,traj='quant',quants=c(0.05,0.95)) })
      output$Kplot <- renderPlot({     if (!is.null(Info$file))    Kplot(Info$MSEproj) })

      output$Tradeplot <- renderPlot({     if (!is.null(Info$file))    TradePlot(Info$MSEproj) })


      output$plot1 <- renderPlot({      if (!is.null(Info$file))   output_dim_scores(Info$Summary)    })
      output$plot3 <- renderPlot({      if (!is.null(Info$file))   output_scores_TBL(Info$Summary)    })

      #  Output by TBL
      output$plot4 <- renderPlot({      if (!is.null(Info$file))   FSHEP(Info$Output_table)      })
      output$plot5 <- renderPlot({      if (!is.null(Info$file))   harvest(Info$Output_table)    })
      output$plot6 <- renderPlot({      if (!is.null(Info$file))   harvest_assets(Info$Output_table)  })
      output$plot7 <- renderPlot({      if (!is.null(Info$file))   risk(Info$Output_table)      })
      output$plot8 <- renderPlot({      if (!is.null(Info$file))   managerial_returns(Info$Output_table)      })
      output$plot9 <- renderPlot({      if (!is.null(Info$file))   trade(Info$Output_table)     })
      output$plot10 <- renderPlot({     if (!is.null(Info$file))   product_form(Info$Output_table)    })
      output$plot11 <- renderPlot({     if (!is.null(Info$file))   post_harvest_perf(Info$Output_table)     })
      output$plot12 <- renderPlot({     if (!is.null(Info$file))   labor_returns(Info$Output_table)      })
      output$plot13 <- renderPlot({     if (!is.null(Info$file))   health_sanit(Info$Output_table)    })

    }
  )
}

