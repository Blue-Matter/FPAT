

HistDynamics_UI <- function(id, label="Fishery Dynamics") {

  ns <- NS(id)
  fluidPage(
    column(12,
      HTML('<br>'),
      htmlOutput(ns("Intro")),
      HTML('<br>'),

      tabsetPanel(

        tabPanel(h5("Introduction",style = "color:black"), HTML("<br>"),value=1,
                 h5('Explanatory text describing info in these plots')
        ),

        tabPanel(h5("Exploitation",style = "color:black"), HTML("<br>"),value=1,
                 column(4,plotOutput(ns('Effort1'))),
                 column(4,plotOutput(ns('Effort2')))
        ),
        tabPanel(h5("Removals",style = "color:black"), HTML("<br>"),value=1,

                 column(12,plotOutput(ns('hist_exp')))
        ),
        tabPanel(h5("Abundance",style = "color:black"), HTML("<br>"),value=2,
                 plotOutput(ns('hist_bio'))
        ),
        tabPanel(h5("Growth ",style = "color:black"), HTML("<br>"),value=3,
                 column(4,plotOutput(ns('Growth1'))),
                 column(4,plotOutput(ns('Growth2'))),
                 column(4,plotOutput(ns('Growth3')))
        ),
        tabPanel(h5("Maturity",style = "color:black"), HTML("<br>"),value=4,
                 column(4,plotOutput(ns('Maturity1'))),
                 column(4,plotOutput(ns('Maturity2')))
        ),
        tabPanel(h5("Survival ",style = "color:black"), HTML("<br>"),value=5,
                 column(4,plotOutput(ns('M1'))),
                 column(4,plotOutput(ns('M2'))),
                 column(4,plotOutput(ns('M3')))
        ),
        tabPanel(h5("Recruitment",style = "color:black"), HTML("<br>"),value=6,
                 column(4,plotOutput(ns('Recruitment1'))),
                 column(4,plotOutput(ns('Recruitment2')))
        ),

        tabPanel(h5("Data",style = "color:black"), HTML("<br>"), value=7)
      )
    )
  )
}


HistDynamics_Server <- function(id, Info) {
  moduleServer(id,
     function(input, output, session) {

       output$Intro <- renderText({
         "This panel contains figures and tables fully documenting the operating model (ie what they have specfied in the FPAT xlsx)"
       })

       # Dynamics

       output$hist_exp <- renderPlot({   if (!is.null(Info$file)) hist_exp(Info)   })
       output$Effort1 <- renderPlot({   if (!is.null(Info$file)) plot('Effort',Info$OM,plot.num=1)   })
       output$Effort2 <- renderPlot({   if (!is.null(Info$file)) plot('Effort',Info$OM,plot.num=2)   })


       output$hist_bio <- renderPlot({   if (!is.null(Info$file)) hist_bio(Info)   })


       output$Growth1 <- renderPlot({     if (!is.null(Info$file)) plot('Growth',Info$OM,plot.num=1)     })
       output$Growth2 <- renderPlot({     if (!is.null(Info$file)) plot('Growth',Info$OM,plot.num=2)     })
       output$Growth3 <- renderPlot({     if (!is.null(Info$file)) plot('Growth',Info$OM,plot.num=3)     })

       output$Maturity1 <- renderPlot({   if (!is.null(Info$file)) plot('Maturity',Info$OM,plot.num=1)   })
       output$Maturity2 <- renderPlot({   if (!is.null(Info$file)) plot('Maturity',Info$OM,plot.num=2)   })

       output$M1 <- renderPlot({          if (!is.null(Info$file)) plot('M',Info$OM,plot.num=1)       })
       output$M2 <- renderPlot({          if (!is.null(Info$file)) plot('M',Info$OM,plot.num=2)       })
       output$M3 <- renderPlot({          if (!is.null(Info$file)) plot('M',Info$OM,plot.num=3)       })

       output$Recruitment1 <- renderPlot({   if (!is.null(Info$file)) plot('Recruitment',Info$OM,plot.num=1)   })
       output$Recruitment2 <- renderPlot({   if (!is.null(Info$file)) plot('Recruitment',Info$OM,plot.num=2)   })

     }
  )
}
