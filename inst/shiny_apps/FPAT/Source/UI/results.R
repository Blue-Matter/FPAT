


Results_UI <- function(id, label="Results") {

  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(12,
             h3('MSE Projection Results'),
             htmlOutput(ns('checkloaded')),
             htmlOutput(ns('MSE_results'))
      )
    )
  )
}

Results_Server <- function(id,Info) {
  moduleServer(id,
               function(input, output, session) {

                 output$checkloaded <- CheckLoaded(Info)

                 output$MSE_results <- renderUI({
                   if(!is.null(Info$MSEhist)) {
                     ns <- NS(id)
                     tagList(
                       fluidRow(
                         box(width=4, status='primary', solidHeader = TRUE,
                             title='Management Strategy Selection',
                             column(12,
                                    h4(strong('Status Quo Catch and Effort')),
                                    p('Fishing in the projection years is fixed at the current catch and current effort.'),
                                    h4(strong('Size limits')),
                                    p('Length-at-retention is set to size-of-maturity and 10% higher than size-of-maturity.'),
                                    h4(strong('Length-based')),
                                    p('Two management procedures that adjust the annual catch limit based on the trend in mean
                           length in the catch.'),
                                    h4(strong('Index-based')),
                                    p('Two management procedures that adjust the annual catch limit based on the trend in the index of
                           abundance.'),
                                    checkboxGroupInput(ns("MPset"),label="Select Management Procedures",
                                                       choiceNames=c("Status Quo Catch and Effort","Size limits","Length-based",
                                                                     "Index-based"),
                                                       choiceValues=c("SQ","Size","LB","IB"),selected=c("SQ")),
                                    htmlOutput(ns('customMPs')),
                                    htmlOutput(ns('selectedMPs')),
                                    br(),
                                    p('Select the management procedures you wish to test and run the MSE projections.'),

                                    actionButton(ns("runMSE"),label="Run MSE Projections",icon=icon('cogs')),
                                    htmlOutput(ns('DownloadMSE'))
                             )
                         ),
                         box(width=8, status='primary', solidHeader = TRUE, height=1050,
                             title='MSE Results',
                             htmlOutput(ns('Projection_results'))

                         )
                       )
                     )

                   }
                 })

                 output$selectedMPs <- renderUI({
                   if (length(Info$MPsel)>0) {

                     mps <- Info$MPsel
                     # clunky renaming
                     mps[mps=='CurC'] <- 'Current Catch'
                     mps[mps=='curEref'] <- 'Current Effort'
                     mps[mps=='matlenlim'] <- 'Size Limit 1'
                     mps[mps=='matlenlim2'] <- 'Size Limit 2'
                     mps[mps=='Ltarget1'] <- 'Length Targeting 1'
                     mps[mps=='Ltarget2'] <- 'Length Targeting 2'
                     mps[mps=='Itarget1'] <- 'Index Targeting 1'
                     mps[mps=='Itarget2'] <- 'Index Targeting 2'
                     text <- paste0(mps, collapse = ', ')
                     tagList(
                       h5(strong('Selected MPs:')),
                       p(text)
                     )
                   }

                 })

                 output$customMPs <- renderUI({
                   if(!is.null(Info$MSEhist)) {
                     Linf <- max(Info$MSEhist@SampPars$Stock$Linf) %>% round(0)
                     step <- 1

                     ns <- NS(id)
                     tagList(
                       bsCollapse(id = ns("collapse"),
                                  bsCollapsePanel("Custom MPs (click to expand)", style = "primary",
                                                  tabsetPanel(
                                                    tabPanel(h5('Custom Size Limit'),
                                                             value=1,
                                                             br(),
                                                             textInput(ns('SLname'),
                                                                       'MP Name (no spaces)',
                                                                       placeholder = 'Custom_Size_Limit'),
                                                             sliderInput(ns('slval'),
                                                                         'Size Limit',
                                                                         min=0,
                                                                         max=Linf,
                                                                         value=0.5*Linf,
                                                                         step=step),
                                                             actionButton(ns('submitSL'),
                                                                          'Submit')
                                                    ),
                                                    tabPanel(h5('Custom Spatial Closure'),
                                                             br(),
                                                             p('Coming soon!')

                                                    ),
                                                    tabPanel(h5('Upload MP'),
                                                             br(),
                                                             p('Coming soon!')

                                                    )
                                                  )
                                  )


                       )
                     )
                   }
                 })

                 observeEvent(input$submitSL, {
                   mp <- makeSLMP(input$slval)
                   nm <- input$SLname
                   nm <- gsub(" ", "_", nm)
                   assign(nm, mp, .GlobalEnv)
                   Info$MPsel<-c(Info$MPsel, nm)
                   Info$MPsel <- unique(Info$MPsel)
                   AM(paste(c("Management strategies selected:", Info$MPsel),collapse= " "))
                 })

                 observeEvent(input$MPset,{
                   MPs<-list(
                     SQ=c("CurC","curEref"),
                     Size=c("matlenlim","matlenlim2"),
                     LB=c("Ltarget1","Ltarget2"),
                     IB=c("Itarget1","Itarget2")
                   )
                   MPsel<-NULL
                   for(i in 1:length(MPs))if(names(MPs)[i] %in% input$MPset) MPsel <- c(MPsel,MPs[[i]])
                   Info$MPsel<-MPsel
                   AM(paste(c("Management strategies selected:", Info$MPsel),collapse= " "))
                 })

                 observeEvent(input$runMSE,{
                   runProj(Info)
                 })



                 output$DownloadMSE<- renderUI({
                   if(!is.null(Info$MSEproj)) {
                     ns <- NS(id)
                     tagList(
                       h5(strong('Download MSE Object')),
                       p('For more detailed analysis, the MSE object can be downloaded',
                         'and used in the',
                         a(href='https://openmse.com/', 'openMSE', target="_blank"),
                         'framework.'),
                       downloadButton(ns('downloadMSE'), 'Download MSE')
                     )
                   }
                 })

                 output$Projection_results <- renderUI({
                   if(is.null(Info$MSEproj)) {
                     return(h4('Projections have not been run yet.', style = "color:red"))
                   } else {
                     ns <- NS(id)
                     tagList(
                       tabsetPanel(
                         tabPanel(h5('Projection Plots', style='color:black;'),
                                  value=1,
                                  br(),
                                  column(8,
                                         plotOutput(ns('Projection_plot'))
                                  ),
                                  column(4,
                                         h3('Projection Plot'),
                                         selectInput(ns('Proj_Var'),
                                                     label = 'Projection Variable',
                                                     choices = c('Spawning Biomass', 'Catch')),
                                         htmlOutput(ns('Proj_opts'))
                                  )
                         ),
                         tabPanel(h5('Trade-Off Plots', style='color:black;'),
                                  value=2,
                                  br(),
                                  column(8,
                                         plotOutput(ns('TradeOff_plot'))
                                  ),
                                  column(4,
                                         h3('Trade-Off Plot'),
                                         h3('X-Axis'),
                                         htmlOutput(ns('Xax')),
                                         htmlOutput(ns('Xopt')),

                                         h3('Y-Axis'),
                                         htmlOutput(ns('Yax')),
                                         htmlOutput(ns('Yopt'))
                                  )
                         )
                       )
                     )
                   }
                 })

                 output$Projection_plot <- renderPlot({
                   if (input$Proj_Var == 'Spawning Biomass') opt <- input$SBopts
                   if (input$Proj_Var == 'Catch') opt <- input$Copts
                   Projection_plot(Info$MSEproj, input$Proj_Var, opt)
                 },
                 width=function() {
                   dims <- window_dims()
                   dims[1]*0.3
                 },
                 height=function() {
                   dims <- window_dims()
                   dims[1]*0.3
                 })

                 output$Proj_opts <- renderUI({
                   ns <- NS(id)

                   if (input$Proj_Var == 'Spawning Biomass') {
                     out <- tagList(
                       radioButtons(ns('SBopts'), 'Relative to:',
                                    choices=c('SB0', 'SBMSY'))
                     )
                   }
                   if (input$Proj_Var == 'Catch') {
                     out <- tagList(
                       checkboxInput(ns('Copts'), 'Include removals?',
                                     value=FALSE)
                     )
                   }
                   return(out)
                 })

                 output$TradeOff_plot <- renderPlot({
                   Xaxis <- list()
                   Xaxis$Year <- input$x_year
                   Xaxis$Var <- input$x_var
                   Xaxis$Reference <- input$x_reference
                   Xaxis$Metric <- input$x_metric
                   IncEx <- input$x_err
                   if (length(Xaxis$Reference)<1) Xaxis$Reference <- 0
                   if(length(IncEx)<1) IncEx <- FALSE

                   Yaxis <- list()
                   Yaxis$Year <- input$y_year
                   Yaxis$Var <- input$y_var
                   Yaxis$Reference <- input$y_reference
                   Yaxis$Metric <- input$y_metric
                   IncEy <- input$y_err
                   if (length(Yaxis$Reference)<1) Yaxis$Reference <- 0
                   if(length(IncEy)<1) IncEy <- FALSE

                   TradeOff_plot(Info$MSEproj, Xaxis, Yaxis, IncEx, IncEy)
                 },
                 width=function() {
                   dims <- window_dims()
                   dims[1]*0.3
                 },
                 height=function() {
                   dims <- window_dims()
                   dims[1]*0.3
                 })


                 output$Xax <- renderUI({
                   ns <- NS(id)
                   if (class(Info$MSEproj) == 'MSE') {
                     Yr1 <- Current_Year + 1
                     Yr2 <- Current_Year + Info$MSEproj@proyears

                     tagList(
                       selectInput(ns('x_var'),
                                   label = 'Variable',
                                   choices = c('SB/SB0', 'SB/SBMSY', 'Catch')),
                       sliderInput(ns('x_year'),
                                   label = h4("Years"),
                                   min = Yr1,
                                   max = Yr2,
                                   value = c(Yr1, Yr2),
                                   step=1),
                       selectInput(ns('x_metric'),
                                   label = 'Metric',
                                   choices = c('Median', 'Probability'))
                     )
                   }
                 })
                 output$Xopt <- renderUI({
                   ns <- NS(id)
                   if (class(Info$MSEproj) == 'MSE') {
                     if (length(input$x_metric)>0) {
                       if (input$x_metric == 'Median') {
                         return(tagList(
                           checkboxInput(ns('x_err'), 'Include error bars?',
                                         value=FALSE)
                         ))
                       }
                       if(input$x_metric == 'Probability') {
                         return(tagList(
                           numericInput(ns('x_reference'), 'Reference Value',
                                        min=0,
                                        value=0.5,
                                        step=0.1)
                         ))
                       }
                     }
                   }
                 })

                 output$Yax <- renderUI({
                   ns <- NS(id)
                   if (class(Info$MSEproj) == 'MSE') {
                     Yr1 <- Current_Year + 1
                     Yr2 <- Current_Year + Info$MSEproj@proyears

                     tagList(
                       selectInput(ns('y_var'),
                                   label = 'Variable',
                                   choices = c('SB/SB0', 'SB/SBMSY', 'Catch'),
                                   selected='Catch'),
                       sliderInput(ns('y_year'),
                                   label = h4("Years"),
                                   min = Yr1,
                                   max = Yr2,
                                   value = c(Yr1, Yr2),
                                   step=1),
                       selectInput(ns('y_metric'),
                                   label = 'Metric',
                                   choices = c('Median', 'Probability'))
                     )
                   }
                 })

                 output$Yopt <- renderUI({
                   ns <- NS(id)
                   if (class(Info$MSEproj) == 'MSE') {
                     if (length(input$y_metric)>0) {
                       if (input$y_metric == 'Median') {
                         return(tagList(
                           checkboxInput(ns('y_err'), 'Include error bars?',
                                         value=FALSE)
                         ))
                       }
                       if(input$y_metric == 'Probability') {
                         return(tagList(
                           numericInput(ns('y_reference'), 'Reference Value',
                                        min=0,
                                        value=0.5,
                                        step=0.1)
                         ))
                       }
                     }
                   }
                 })


                 output$downloadMSE <- downloadHandler(
                   filename = function() {
                     paste("MSE", Sys.Date(), ".rda", sep="")
                   },
                   content = function(file) {
                     if(class(Info$MSEproj)=='MSE') {
                       saveRDS(Info$MSEproj, file)
                     }

                   }

                 )

               }
  )
}


makeSLMP <- function(SL) {
  mp <- function (x, Data, reps, ...)  {
    rec <- new("Rec")
    rec@LR5 <- SL*0.99
    rec@LFR <- SL
    rec
  }
  class(mp) <- 'MP'
  mp
}
