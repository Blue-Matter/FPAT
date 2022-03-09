

HistDynamics_UI <- function(id, label="Fishery Dynamics") {

  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(12,
             h3('Simulated Historical Fishery Dynamics'),
             htmlOutput(ns('checkloaded')),
             htmlOutput(ns('Hist_dynamics'))

      )
    )
  )
}


HistDynamics_Server <- function(id, Info, Toggles) {
  moduleServer(id,
     function(input, output, session) {

       output$checkloaded <- CheckOMLoaded(Info)

       output$Hist_dynamics <- renderUI({
         if(!is.null(Info$MSEhist)) {
           ns <- NS(id)
           tagList(
             fluidRow(
               box(width=9, status='primary', solidHeader = TRUE,
                   title='Historical Fishery Dynamics',
                   tabsetPanel(
                     tabPanel(h5('Spawning Biomass', style='color:black;'),
                              value=1,
                              br(),
                              column(7,
                                     plotOutput(ns('hist_SB'), width='100%', height="100%")
                              ),
                              column(5,
                                     h3('Relative Historical Spawning Biomass'),
                                     p("The median and 25th & 75th quantiles of the simulated spawning biomass for the historical years.",
                                       'The spawning biomass is shown relative to the average unfished spawning biomass (SB0).')
                              )
                     ),
                     tabPanel(h5('Catch', style='color:black;'),
                              value=2,
                              br(),
                              column(7,
                                     plotOutput(ns('hist_catch'), width='100%', height="100%")
                              ),
                              column(5,
                                     h3('Relative Historical Catches'),
                                     p("The median and 25th & 75th quantiles of the simulated catches for the historical years.",
                                       'The catches are shown relative to the catch in the most recent year.')
                              )
                     ),
                     tabPanel(h5('Recruitment', style='color:black;'),
                              value=2,
                              br(),
                              column(7,
                                     plotOutput(ns('hist_recruitment'), width='100%', height="100%")
                              ),
                              column(5,
                                     h3('Relative Historical Recruitment'),
                                     p("The median and 25th & 75th quantiles of the simulated recruitment for the historical years.",
                                       'The recruitment is shown relative to the average unfished recruitment (R0).')
                              )
                     )
                   )
               ),
               box(width=3,status='primary', solidHeader = TRUE,
                   title='Fishery Simulation Metadata',
                   tableOutput(ns('FisheryMetadata')),
                   htmlOutput(ns('Assumptions'))
               ),

               box(width=3,status='primary', solidHeader = TRUE,
                   title='Download OM Report',
                   p('An Operating Model Report with plots of all simulated fishery dynamics and
                                       parameters can be downloaded by clicking the button below.'),
                   # radioButtons(ns('filetype'), 'Report File Type',
                                # choices = list("HTML" = 'html', "PDF" = 'pdf'), inline=TRUE),
                   downloadButton(ns('downloadOMRep'), 'Download OM Report'),
                   br()
               ),
               box(width=3,status='primary', solidHeader = TRUE,
                   title='Advanced',
                   bsCollapse(id = "collapseExample",
                              bsCollapsePanel("Download OM",
                                              p('For more detailed analysis, the Operating Model object can be downloaded',
                                                'and used in the',
                                                a(href='https://openmse.com/', 'openMSE', target="_blank"),
                                                'framework.'),
                                              downloadButton(ns('downloadOM'), 'Download OM'),
                                              style = "info"),
                              bsCollapsePanel("Load an OM from file",
                                              p('An openMSE Operating Model object can be uploaded here.',
                                                'This will replace the operating model constructed from the FPAT data file.'),
                                              fileInput(ns("LoadOM"),label=NULL, accept=c('.OM', '.rda', '.rdata')),
                                              style = "info")
                   )
               )
             )
           )
         }
       })

       output$Assumptions <- renderUI({
         asslist <<- Info$OM@Misc$asslist
         if (length(asslist)>0) {
           ns <- NS(id)
           actionButton(ns("assumptions"), "Assumptions")

         }
       })

       observeEvent(input$assumptions, {
        asslist <- Info$OM@Misc$asslist
        if (length(asslist)>0) {
          shinyalert("The following assumptions were made when the OM was created",
                     paste('<li>', asslist, '</li>', collapse='\n\n'), type = "info", size="m", html=TRUE)
        }

       })

       output$FisheryMetadata <- renderTable({
         if (!is.null(Info$Data)) {
           makefisherymetadata(Info)
         }
       }, colnames = FALSE, sanitize.text.function=function(x){x})

       output$hist_SB <- renderPlot({
         hist_spawnbio(Info)
         },
         width=function() {
           dims <- window_dims()
           dims[1]*0.3
         },
         height=function() {
           dims <- window_dims()
           dims[1]*0.3
         })

       output$hist_catch <- renderPlot({
         hist_catch(Info)
       },
       width=function() {
         dims <- window_dims()
         dims[1]*0.3
       },
       height=function() {
         dims <- window_dims()
         dims[1]*0.3
       })

       output$hist_recruitment <- renderPlot({
         hist_recruitment(Info)
       },
       width=function() {
         dims <- window_dims()
         dims[1]*0.3
       },
       height=function() {
         dims <- window_dims()
         dims[1]*0.3
       })

       output$downloadOMRep <- downloadHandler(
         filename = function() {
           # if (input$filetype == 'html') {
           #   paste("OM_Report", Sys.Date(), ".html", sep="")
           # } else {
           #   paste("OM_Report", Sys.Date(), ".pdf", sep="")
           # }
           paste("OM_Report", Sys.Date(), ".html", sep="")
         },
         content = function(file) {
           if(class(Info$MSEhist)=='Hist') {
             output_format <- 'html_document'
             # if (input$filetype == 'html') {
             #   output_format <- 'html_document'
             # } else {
             #   output_format <- 'pdf_document'
             # }
             AM("------------- Generating OM Report --------------")
             # AM(paste0("File type: ", input$filetype))
             AM(paste0("output_format:C ", output_format))
             GenOMreport(Info$MSEhist, file, output_format)
           }
         }
       )

       output$downloadOM <- downloadHandler(
         filename = function() {
            paste("OM", Sys.Date(), ".rda", sep="")
         },
         content = function(file) {
           if(class(Info$OM)=='OM') {
             saveRDS(Info$OM, file)
           }

         }
       )

       observeEvent(input$LoadOM, {
         OM <- try(readRDS(input$LoadOM$datapath), silent=T)
         AM("--- Loading OM ----------------")
         AM(paste0('File: ', input$LoadOM$datapath))

         if(class(OM) =='OM') {
           Info$OM <- OM
           if(!is.null(Info$OM)){
             AM('Simulating fishery')
             withProgress(message = "Constructing operating model", value = 0, {
               MSEhist<-try(runMSE(Info$OM,Hist=T,extended=T), silent=T)
             })
             if (class(MSEhist)=='Hist') {
               AM('Simulating fishery complete')
               Info$MSEhist<-MSEhist
             } else {
               AM('Simulating fishery error')
               shinyalert("FPAT did not build", paste("Error:", MSEhist), type = "error")
                            }
             Toggles$Loaded<-as.integer(!is.null(Info$OM))
           }

         } else {
           shinyalert("Uploaded file was not valid openMSE OM Object", type = "error")
           AM('Error uploading OM Object')
         }
       })
     })
}



GenOMreport <- function(MSEhist, file, output_format, nsamp=3) {
  SampCpars <- list() # empty list

  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("Package \"knitr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Package \"rmarkdown\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  plotPars <- list(breaks=10, col="darkgray", axes=FALSE,
                   cex.main=1, lwd=2)

  nsim <- MSEhist@OM@nsim
  nyears <- length(MSEhist@Data@Year)
  proyears <- MSEhist@OM@proyears
  title <- "Historical Simulations"

  Pars <- list(Stock=MSEhist@SampPars$Stock,
               Fleet=MSEhist@SampPars$Fleet,
               Obs=MSEhist@SampPars$Obs,
               Imp=MSEhist@SampPars$Imp)
  Pars$Hist <- MSEhist
  Pars$CurrentYr <- Current_Year
  Pars$Name <- 'Operating Model'
  Pars$MPA <- MSEhist@OM@MPA

  Params <- list(
    title = title,
    Pars = Pars,
    plotPars=plotPars,
    tabs = TRUE,
    its = sample(1:nsim, nsamp),
    nyears=nyears,
    proyears=proyears,
    date=NULL
  )

  RMD <- "OM.Rmd"
  Class <- 'OM'
  input <- file.path(system.file(package = 'MSEtool'),'Rmd', Class, RMD)

  knitr::knit_meta(class=NULL, clean = TRUE)
  withProgress(message = "Generating OM Report", value = 0, {
    rmarkdown::render(input, params=Params,
                      output_file=file,
                      quiet=TRUE, output_format=output_format)
  })
}

makefisherymetadata <- function(Info) {
  data <- Info$Data
  if (class(Info$MSEhist) == 'Hist') {
    MSEhist <- Info$MSEhist
    SB <- apply(MSEhist@TSdata$SBiomass,1:2,sum)
    nsim <- dim(SB)[1]
    nyears <- dim(SB)[2]
    lhyear <- max(c(data@LHYear, data@Year), na.rm=TRUE)
    yrs <- (lhyear-nyears+1):lhyear
    yrs <- paste0(yrs[1], "-", yrs[length(yrs)])


    return(  c(paste0('<strong>Name: </strong>',data@Name),
               paste0('<strong>Species: </strong>', '<i>', data@Species, '</i>'),
               paste0('<strong>Common Name: </strong>', data@Common_Name),
               paste0('<strong>Region: </strong>', data@Region),
               paste0('<strong>Historical Years: </strong>', yrs)))
  }
}

