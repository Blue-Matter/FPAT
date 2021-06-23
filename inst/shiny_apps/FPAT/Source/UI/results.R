


Results_UI <- function(id, label="Results") {

  ns <- NS(id)
  fluidPage(
    column(12,
       br(),
       htmlOutput(ns("Intro")),
       br(),
       h4("About MSE results..."),
       tabsetPanel(
         tabPanel(h5("Kobe",style = "color:black"), HTML("<br>"),value=1,
                  column(6,plotOutput(ns('Pplot'))),
                  column(6,plotOutput(ns('Kplot')))

         ),
         tabPanel(h5("Trade-offs",style = "color:black"), HTML("<br>"),value=2,
                  column(6,plotOutput(ns('Tradeplot')))
         ),
         tabPanel(h5("Socio-Economic Outcomes",style = "color:black"), HTML("<br>"),value=2,
                  h5('Plots of FPI-related Performance Metrics')
         ),
         tabPanel(h5("Management Strategies",style = "color:black"), HTML("<br>"),value=3,
                  #h5("Select Management Strategies for Evaluation"),
                  checkboxGroupInput(ns("MPset"),label="Management Strategy Selections",
                                     choiceNames=c("Status Quo Catch and Effort","Size limits","Length-based","Index-based"),
                                     choiceValues=c("SQ","Size","LB","IB"),selected="SQ"),
                  actionButton(ns("runMSE"),label="Re-run MSE Projections",icon=icon('cogs')),
                  br()

         )
       )
    )
  )
}

Results_Server <- function(id,Info) {
  moduleServer(id,
    function(input, output, session) {

      output$Intro <- renderText({
        "This panel contains the results of the openMSE projections - with links to FPI performance metrics"
      })

      output$Pplot <- renderPlot({     if (!is.null(Info$file))    Pplot2(Info$MSEproj,traj='quant',quants=c(0.05,0.95)) })
      output$Kplot <- renderPlot({     if (!is.null(Info$file))    Kplot(Info$MSEproj) })

      output$Tradeplot <- renderPlot({     if (!is.null(Info$file))    TradePlot(Info$MSEproj) })

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

