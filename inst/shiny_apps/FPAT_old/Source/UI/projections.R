


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

Results_Server <- function(id,Info, window_dims) {
  moduleServer(id,
               function(input, output, session) {

                 output$checkloaded <- CheckOMLoaded(Info)

                 output$MSE_results <- renderUI({
                   if(!is.null(Info$MSEhist)) {
                     ns <- NS(id)
                     tagList(
                         column(width=3,
                             h3('Management Strategy Selection'),
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
                                    h4(strong('Spatial Management')),
                                    p('Two management procedures that 1) open an existing spatial closure (if one exists) and 2) close the planned spatial closure (if any)'),
                                    checkboxGroupInput(ns("MPset"),label="Select Management Procedures",
                                                       choiceNames=c("Status Quo Catch and Effort","Size limits","Length-based",
                                                                     "Index-based", "Spatial Management"),
                                                       choiceValues=c("SQ","Size","LB","IB", "Spatial"),selected=c("SQ")),
                                    htmlOutput(ns('customMPs')),
                                    htmlOutput(ns('selectedMPs')),
                                    actionButton(ns('reset_defaults'), 'Reset Default MPs'),
                                    br(),
                                    br(),
                                    h5('Select the management procedures you wish to test and run the MSE projections.'),
                                    actionButton(ns("runMSE"),label="Run MSE Projections",icon=icon('cogs')),
                                    htmlOutput(ns('DownloadMSE')),
                                    br(),
                                    hr()
                             )
                         ),
                         column(width=9,
                             h3('MSE Results'),
                             htmlOutput(ns('Projection_results'))

                         )

                     )

                   }
                 })


                 output$selectedMPs <- renderUI({
                   if (length(Info$MPsel)>0) {
                     mps <- Info$MPsel
                     ns <- NS(id)
                     tagList(
                       selectInput(ns('MPselect'), 'Selected MPs:', choices=mps, selected=mps, multiple = TRUE)
                     )
                   }

                 })

                 observeEvent(input$reset_defaults, {
                   Info$MPsel <- c("Current_Catch","Current_Effort")
                   updateCheckboxGroupInput(session, inputId='MPset',
                                            label="Select Management Procedures 2 ",
                                            choiceNames=c("Status Quo Catch and Effort","Size limits","Length-based",
                                                          "Index-based", "Spatial Management"),
                                            choiceValues=c("SQ","Size","LB","IB", "Spatial"),selected=c("SQ"))
                   updateSelectInput(session, inputId='MPselect', selected=Info$MPsel)

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
                                                             p('Size limit is in the same units as length parameters specified in the FPAT input file.'),
                                                             actionButton(ns('submitSL'),
                                                                          'Submit')
                                                    ),
                                                    tabPanel(h5('Custom Constant Effort Limit'),
                                                             value=1,
                                                             br(),
                                                             textInput(ns('Effname'),
                                                                       'MP Name (no spaces)',
                                                                       placeholder = 'Custom_Effort_Limit'),
                                                             sliderInput(ns('eff'),
                                                                         "Effort Limit",
                                                                         min=0.05,
                                                                         max=3,
                                                                         value=0.5,
                                                                         step=0.05),
                                                             p('Effort limit is relative to the current fishing effort (e.g., a value of 0.5 means projected effort will be half the current level.)'),
                                                             actionButton(ns('submitcustomE'),
                                                                          'Submit')
                                                    ),
                                                    tabPanel(h5('Custom Constant Catch Limit'),
                                                             value=1,
                                                             br(),
                                                             textInput(ns('TACname'),
                                                                       'MP Name (no spaces)',
                                                                       placeholder = 'Custom_Catch_Limit'),
                                                             sliderInput(ns('catch'),
                                                                         "Catch Limit",
                                                                         min=0.05,
                                                                         max=3,
                                                                         value=0.5,
                                                                         step=0.05),
                                                             p('Catch limit is relative to the current catch (e.g., a value of 0.5 means projected catch will be half the current level.)'),
                                                             actionButton(ns('submitcustomC'),
                                                                          'Submit')
                                                    ),
                                                    tabPanel(h5('Advanced - Import MP'),
                                                             br(),
                                                             p('Import an MP (saved using saveRDS)'),
                                                             p("Once a file is uploaded, provide a name and click button to add the MP."),
                                                             fileInput(ns("ImportMP"),label=NULL, accept=c('.rdata', '.rds', '.rda')),
                                                             textInput(ns("MS_Import_Label"),label=NULL, value = "", placeholder = "Name of management procedure"),
                                                             actionButton(ns("loadMP"),"Add MP",style='color:red',icon=icon('cogs'))
                                                    )
                                                  )
                                  )


                       )
                     )
                   }
                 })

                 shinyjs::disable("loadMP")

                 observeEvent(input$ImportMP, {
                   filey <<- input$ImportMP

                   tryCatch({
                     MP_out <<- readRDS(file = filey$datapath)
                     stopifnot(typeof(MP_out) == "closure" && inherits(MP_out, "MP"))
                     shinyjs::enable("loadMP")

                   }, error = function(e) {
                     AM(paste0(e,"\n"))
                     shinyalert(paste0("No MP was found in file: ", filey$name), type = "error")
                     AM(paste0("No MP was found in file: ", filey$name))
                     return(0)
                   })
                 })

                 observeEvent(input$loadMP, {
                   if(!nchar(input$MS_Import_Label)) {
                     shinyalert("No name for the MP was provided.", type = "error")
                   }

                   filey <- input$ImportMP
                   tryCatch({
                     MP_out <- readRDS(file = filey$datapath)
                     stopifnot(typeof(MP_out) == "closure" && inherits(MP_out, "MP"))

                     if(input$MS_Import_Label %in% Info$MPsel) {
                       AM(paste0("Error: ", input$MS_Import_Label, " MP already selected. Choose another name."))
                     } else {
                       nm <- input$MS_Import_Label
                       nm <- gsub(" ", "_", nm)
                       assign(nm, MP_out, envir = .GlobalEnv)
                       Info$MPsel<-c(Info$MPsel, nm)
                       Info$MPsel <- unique(Info$MPsel)
                       AM(paste(c("Management strategies selected:", Info$MPsel),collapse= " "))
                     }
                   }, error = function(e) {
                     AM(paste0(e,"\n"))
                     shinyalert(paste0("No MP was found in file: ", filey$name), type = "error")
                     AM(paste0("No MP was found in file: ", filey$name))
                     return(0)
                   })
                 })

                 observeEvent(input$submitSL, {
                   mp <- makeSLMP(input$slval)
                   nm <- input$SLname
                   nm <- gsub(" ", "_", nm)
                   if (nchar(nm)>0) {
                     assign(nm, mp, .GlobalEnv)
                     Info$MPsel<-c(Info$MPsel, nm)
                     Info$MPsel <- unique(Info$MPsel)
                     AM(paste(c("Management strategies selected:", Info$MPsel),collapse= " "))
                   }

                 })

                 observeEvent(input$submitcustomE, {
                   mp <- makeEffortMP(input$eff)
                   nm <- input$Effname
                   nm <- gsub(" ", "_", nm)
                   if (nchar(nm)>0) {
                     assign(nm, mp, .GlobalEnv)
                     Info$MPsel<-c(Info$MPsel, nm)
                     Info$MPsel <- unique(Info$MPsel)
                     AM(paste(c("Management strategies selected:", Info$MPsel),collapse= " "))
                   }

                 })

                 observeEvent(input$submitcustomC, {
                   mp <- makeTACMP(input$catch)
                   nm <- input$TACname
                   nm <- gsub(" ", "_", nm)
                   if (nchar(nm)>0) {
                     assign(nm, mp, .GlobalEnv)
                     Info$MPsel<-c(Info$MPsel, nm)
                     Info$MPsel <- unique(Info$MPsel)
                     AM(paste(c("Management strategies selected:", Info$MPsel),collapse= " "))
                   }

                 })

                 observeEvent(input$MPset,{
                   MPs<-list(
                     SQ=c("Current_Catch","Current_Effort"),
                     Size=c("Size_Limit_1","Size_Limit_2"),
                     LB=c("Length_Targeting_1","Length_Targeting_2"),
                     IB=c("Index_Targeting_1","Index_Targeting_2"),
                     Spatial=c('Open_Existing', 'Close_Planned')
                   )
                   MPsel<-NULL
                   for(i in 1:length(MPs))if(names(MPs)[i] %in% input$MPset) MPsel <- c(MPsel,MPs[[i]])
                   Info$MPsel<-MPsel

                   AM(paste(c("Management strategies selected:", Info$MPsel),collapse= " "))


                 })

                 observeEvent(input$MPselect, {
                   selectMPs<-input$MPselect
                   Info$MPsel <- selectMPs
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

                 proj_height <- reactiveVal(400)

                 observe({
                   if(!is.null(Info$MSEproj2)) {
                     nMPs <- Info$MSEproj2@nMPs
                     if (nMPs > 3) {
                       plotheight <- 400 + (nMPs/3)*400
                     }else plotheight <- 400
                     proj_height(plotheight)
                   }
                 })


                 output$Projection_results <- renderUI({
                   if(is.null(Info$MSEproj)) {
                     return(h4('Projections have not been run yet.', style = "color:red"))
                   } else {
                     ns <- NS(id)
                     tagList(
                       column(10,

                       tabsetPanel(
                         tabPanel(h4('Projection Plots', style='color:black;'),
                                  value=1,
                                  tabsetPanel(
                                    tabPanel(h4('Spawning Biomass', style='color:black;'),
                                             column(8,
                                                    br(),
                                                    plotOutput(ns('Biomass_projection_plot'))
                                             ),
                                             column(4,
                                                    htmlOutput(ns('Biomass_text')),
                                                    htmlOutput(ns('Biomass_opts'))
                                             )
                                    ),
                                    tabPanel(h4('Catch', style='color:black;'),
                                             column(8,
                                                    br(),
                                                    plotOutput(ns('Catch_projection_plot'))
                                             ),
                                             column(4,
                                                    htmlOutput(ns('Catch_text')),
                                                    htmlOutput(ns('Catch_opts'))
                                             )
                                    ),
                                    tabPanel(h4('Recruitment', style='color:black;'),
                                             column(8,
                                                    br(),
                                                    plotOutput(ns('Rec_projection_plot'))
                                             ),
                                             column(4,
                                                    htmlOutput(ns('Rec_text')),
                                                    htmlOutput(ns('Rec_opts'))
                                             )
                                    )
                                  )
                         ),
                         tabPanel(h4('Trade-Off Plots', style='color:black;'),
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
                       ),
                       column(2,
                              checkboxGroupInput(ns('MP_select'),
                                                   'Filter MPs',
                                                   choices=Info$MSEproj@MPs,
                                                   selected=Info$MSEproj@MPs)

                              )

                     )
                   }
                 })

                 observeEvent(input$MP_select, {
                   Info$MSEproj2 <- Sub(Info$MSEproj, MPs=input$MP_select)
                 })


                 observe({
                 output$Biomass_projection_plot <- renderPlot({
                   if (!is.null(input$SB_quants))
                     Projection_plot(Info$MSEproj2, 'Spawning Biomass', input$SBopts, input$SB_quants)
                 },
                 height=proj_height())
                 })

                 observe({
                   output$Catch_projection_plot <- renderPlot({
                     if (!is.null(input$Catch_quants))
                       Projection_plot(Info$MSEproj2, 'Catch', input$Catchopts, input$Catch_quants)
                   },
                   height=proj_height())
                 })


                 observe({
                   output$Rec_projection_plot <- renderPlot({
                     if (!is.null(input$Rec_quants))
                       Projection_plot(Info$MSEproj2, 'Recruitment', 0, input$Rec_quants)
                   },
                   height=proj_height())
                 })


                 output$Biomass_text <-  renderUI({
                   ns <- NS(id)
                   nMPs <- Info$MSEproj2@nMPs
                   txt <- ''
                   if (nMPs>1) p <- 'plots'
                   if (nMPs==1) p <- 'plot'
                   if (nMPs>1) tt <- 'These'
                   if (nMPs==1) tt <- 'This'
                   quant1 <- input$SB_quants
                   quant2 <- 100-quant1

                     if (length(input$SBopts)>0) {
                       if (quant1>50) {
                         if (input$SBopts=='SB0') {
                           txt <- paste0(tt, ' projection ', p, ' show the median (line) and ', quant2, 'th and ', quant1, 'th percentiles (shading) of spawning biomass relative to average unfished spawning biomass (SB0) for each MP.')
                         } else {
                           txt <- paste0(tt, ' projection ', p, ' show the median (line) and ', quant2, 'th and ', quant1, 'th percentiles (shading) of spawning biomass relative to spawning biomass corresponding with maximum sustainable yield (SBMSY) for each MP.')
                         }
                       } else {
                         if (input$SBopts=='SB0') {
                           txt <- paste0(tt, ' projection ', p, ' show the median spawning biomass relative to average unfished spawning biomass (SB0) for each MP.')
                         } else {
                           txt <- paste0(tt, ' projection ', p, ' show the median spawning biomass relative to spawning biomass corresponding with maximum sustainable yield (SBMSY) for each MP.')
                         }
                       }

                     }
                   if (length(txt)>0)
                     return(tagList(
                       br(),
                       h4('Projection Plots: Spawning Biomass'),
                       br(),
                       p(txt),
                       br())
                     )
                 })

                 output$Catch_text <-  renderUI({
                   ns <- NS(id)
                   nMPs <- Info$MSEproj2@nMPs
                   txt <- ''
                   if (nMPs>1) p <- 'plots'
                   if (nMPs==1) p <- 'plot'
                   if (nMPs>1) tt <- 'These'
                   if (nMPs==1) tt <- 'This'
                   quant1 <- input$Catch_quants
                   quant2 <- 100-quant1

                   if (quant1>50) {
                     txt <- paste0(tt, ' projection ', p, ' showing the median (line) and ', quant2, 'th and ', quant1, 'th percentiles (shading) of projected catch relative to catch in the most recent year for each MP.')
                   } else {
                     txt <- paste0(tt, ' projection ', p, ' showing the median projected catch relative to catch in the most recent year for each MP.')
                   }
                   if (length(txt)>0)
                     return(tagList(
                       br(),
                       h4('Projection Plots: Catch'),
                       br(),
                       p(txt),
                       br())
                     )
                 })

                 output$Rec_text <-  renderUI({
                   ns <- NS(id)
                   nMPs <- Info$MSEproj2@nMPs
                   txt <- ''
                   if (nMPs>1) p <- 'plots'
                   if (nMPs==1) p <- 'plot'
                   if (nMPs>1) tt <- 'These'
                   if (nMPs==1) tt <- 'This'
                   quant1 <- input$Rec_quants
                   quant2 <- 100-quant1

                   if (quant1>50) {
                     txt <- paste0(tt, ' projection ', p, ' showing the median (line) and ', quant2, 'th and ', quant1, 'th percentiles (shading) of projected recruitment relative to the average unfished recruitment for each MP.')
                   } else {
                     txt <- paste0(tt, ' projection ', p, ' showing the median projected recruitment relative to the average unfished recruitment for each MP.')
                   }
                   if (length(txt)>0)
                     return(tagList(
                       br(),
                       h4('Projection Plots: Recruitment'),
                       br(),
                       p(txt),
                       br())
                     )
                 })


                 output$Biomass_opts <- renderUI({
                   ns <- NS(id)
                   tagList(
                     bsCollapse(id = ns("Biocollapse"),
                                bsCollapsePanel("Plot Controls (click to expand)", style = "primary",
                                                radioButtons(ns('SBopts'), 'Relative to:',
                                                             choices=c('SB0', 'SBMSY')),
                                                sliderInput(ns('SB_quants'),
                                                            'Percentiles',
                                                            min=50,
                                                            max=100,
                                                            value=50,
                                                            step=5
                                                            )
                                )
                     )

                   )
                 })

                 output$Catch_opts <- renderUI({
                   ns <- NS(id)
                   tagList(
                     bsCollapse(id = ns("Catchcollapse"),
                                bsCollapsePanel("Plot Controls (click to expand)", style = "primary",
                                                checkboxInput(ns('Catchopts'), 'Include removals?',
                                                              value=FALSE),
                                                sliderInput(ns('Catch_quants'),
                                                            'Percentiles',
                                                            min=50,
                                                            max=100,
                                                            value=50,
                                                            step=5
                                                )
                                )
                     )

                   )
                 })

                 output$Rec_opts <- renderUI({
                   ns <- NS(id)
                   tagList(
                     bsCollapse(id = ns("Reccollapse"),
                                bsCollapsePanel("Plot Controls (click to expand)", style = "primary",
                                                sliderInput(ns('Rec_quants'),
                                                            'Percentiles',
                                                            min=50,
                                                            max=100,
                                                            value=50,
                                                            step=5
                                                )
                                )
                     )

                   )
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

                   TradeOff_plot(Info$MSEproj2, Xaxis, Yaxis, IncEx, IncEy)
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
                                   choices = c('SB/SB0', 'SB/SBMSY', 'Catch', 'AAVY', 'AAVE')),
                       sliderInput(ns('x_year'),
                                   label = h4("Years"),
                                   min = Yr1,
                                   max = Yr2,
                                   value = c(Yr1, Yr2),
                                   step=1,
                                   sep=''),
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
                                   choices = c('SB/SB0', 'SB/SBMSY', 'Catch', 'AAVY', 'AAVE'),
                                   selected='Catch'),
                       sliderInput(ns('y_year'),
                                   label = h4("Years"),
                                   min = Yr1,
                                   max = Yr2,
                                   value = c(Yr1, Yr2),
                                   step=1,
                                   sep=''),
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

makeEffortMP <- function(eff) {
  mp <- function (x, Data, reps, ...)  {
    rec <- new("Rec")
    rec@Effort <- eff
    rec
  }
  class(mp) <- 'MP'
  mp
}

makeTACMP <- function(catch) {
  mp <- function (x, Data, reps, ...)  {
    rec <- new("Rec")
    yearind <- which(Data@Year == Data@LHYear)
    rec@TAC <- Data@Cat[x,yearind]
    rec
  }
  class(mp) <- 'MP'
  mp
}
