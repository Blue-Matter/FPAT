

Inputs_UI <- function(id, label="Inputs") {

  ns <- NS(id)
  tagList(
    column(12,style="height: 700px",
      HTML('<br>'),
      htmlOutput(ns("Intro")),
      HTML('<br>'),

      tabsetPanel(

        tabPanel(h5("FPI+ Inputs ",style = "color:black"), HTML("<br>"),value=1,

                 column(4,
                        h4('FPI+ Input Dimension Scores'),
                        plotOutput(ns('FPIin'))
                 ),

                 ),
        tabPanel(h5("Fishery Dynamics",style = "color:black"), HTML("<br>"),value=2,

         tabsetPanel(

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

       ),

       tabPanel(h5("Management Strategies",style = "color:black"), HTML("<br>"),value=3,
            #h5("Select Management Strategies for Evaluation"),
            checkboxGroupInput(ns("MPset"),label="Management Strategy Selections",choiceNames=c(
             "Status Quo Catch and Effort","Size limits","Length-based","Index-based"),
             choiceValues=c("SQ","Size","LB","IB"),selected="SQ"),
            actionButton(ns("runMSE"),label="Re-run MSE test",icon=icon('cogs'))

       )

      )
    )
  )

}

Inputs_Server <- function(id, Info) {
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



       # FPI
       output$FPIin <- renderPlot({       if (!is.null(Info$file))   input_dim_scores(Info$Summary)   })

       observeEvent(input$MPset,{

         MPs<-list(
           SQ=c("CurC","curEref"),
           Size=c("matlenlim","matlenlim2"),
           LB=c("Ltarget1","Ltarget2"),
           IB=c("Itarget1","Itarget2")
         )

         #input<-list(MPset=names(MPs)[2:3]) # debug
         MPsel<-NULL
         for(i in 1:length(MPs))if(names(MPs)[i] %in% input$MPset) MPsel <- c(MPsel,MPs[[i]])
         Info$MPsel<-MPsel
         AM(paste(c("Management strategies selected:", Info$MPsel),collapse= " "))

       })

       observeEvent(input$runMSE,{
         runProj(Info)
       })

     }
  )
}
