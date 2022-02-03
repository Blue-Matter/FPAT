

FPI_UI <- function(id, label="Inputs") {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(12,
             h3('Fishery Performance Indicators'),
             htmlOutput(ns('checkloaded')),
             htmlOutput(ns('FPI_results'))
      )

    )
  )
}

FPI_Server <- function(id, Info, FPI_2) {
  moduleServer(id,
     function(input, output, session) {

       output$checkloaded <- CheckLoaded(Info)

       output$FPI_results <- renderUI({
         if(!is.null(Info$MSEhist)) {
           ns <- NS(id)
           tagList(
             fluidRow(
               box(width=9, status='primary', solidHeader = TRUE,
                   title='FPI Scores',
                   tabsetPanel(
                     # Output Sector Scores
                     tabPanel(h5('Outputs: Sector', style='color:black;'),
                              value=1,
                              br(),
                              column(7,
                                     plotOutput(ns('FPIoutput_sector'), width='100%', height="100%")
                              ),
                              column(5,
                                     h3('Outputs: Sector'),
                                     htmlOutput(ns('FPI_output_sector_text')),
                                     p('Select the desired benchmark from the drop down menu or load another FPI database to compare scores to identify dimensions where other fisheries have found ways to perform better.')
                              )
                     ),
                     tabPanel(h5('Outputs: Triple Bottom Line', style='color:black;'),
                              value=2,
                              br(),
                              column(7,
                                     plotOutput(ns('FPIoutput_tbl'), width='100%', height="100%")
                              ),
                              column(5,
                                     h3('Outputs: Triple Bottom Line'),
                                     htmlOutput(ns('FPI_output_tpl_text')),
                                     p('Select the desired benchmark from the drop down menu or load another FPI database to
                                     compare scores to identify dimensions where other fisheries have found ways to perform better.')
                              )
                     ),
                     tabPanel(h5('Inputs: Enabling Conditions', style='color:black;'),
                              value=3,
                              br(),
                              column(7,
                                     plotOutput(ns('FPIinput'), width='100%', height="100%")
                              ),
                              column(5,
                                     h3('Inputs: Enabling Conditions'),
                                     htmlOutput(ns('FPI_input_enabling')),
                                     p('Select the desired benchmark from the drop down menu, and compare scores to identify dimensions where the fishery has different levels of enabling conditions than typical fisheries of the same category.')
                              )
                     )
                   )
               ),
               box(width=3,status='primary', solidHeader = TRUE,
                   title='FPI Metadata',
                   tableOutput(ns('FPImetadata'))
               ),
               box(width=3,status='primary', solidHeader = TRUE,
                   title='FPI Comparisons',
                   shiny::selectInput(ns('baseline'), 'Baseline Comparison',
                                      choices=BaseLineChoices,
                                      multiple = TRUE),
                   h5(strong('Another FPI data file')),
                       column(8, fileInput(ns("Load2"),label=NULL)),
                       column(4, actionButton(ns('remove'), 'Remove'))

               ),
               box(width=3,status='primary', solidHeader = TRUE,
                   title='Download FPI Report',
                   p('The FPI plots can be downloaded in a FPI Report by clicking the button below.'),
                   radioButtons(ns('filetype'), 'Report File Type',
                                choices = list("HTML" = 'html', "PDF" = 'pdf'), inline=TRUE),
                   downloadButton(ns('downloadFPIRep'), 'Download FPI Report')


               )
             )
           )
           }
         })

       output$FPI_output_sector_text <- renderUI({
         rawText <- readLines('Data/FPI_output_sector.txt')
         splitText <- stringi::stri_split(str = rawText, regex = '\\n')
         lapply(splitText, p)
       })

       output$FPI_output_tpl_text <- renderUI({
         rawText <- readLines('Data/FPI_output_tpl.txt')
         splitText <- stringi::stri_split(str = rawText, regex = '\\n')
         lapply(splitText, p)
       })

       output$FPI_input_enabling <- renderUI({
         rawText <- readLines('Data/FPI_input_enabling.txt')
         splitText <- stringi::stri_split(str = rawText, regex = '\\n')
         lapply(splitText, p)
       })

       output$FPImetadata <- renderTable({
         if (!is.null(Info$FPI.Cover)) {
           metadata <- Info$FPI.Cover
           makemetadata(metadata)
         }
       }, colnames = FALSE, sanitize.text.function=function(x){x})

       output$FPIoutput_sector <- renderPlot({
         if (!is.null(Info$file)) {
           output_dim_scores(Info$Summary, input$baseline, BaseLine, FPI_2$Summary)
         }
       }, width=function() {
         dims <- window_dims()
         dims[1]*0.3
       }, height=function() {
         dims <- window_dims()
         dims[1]*0.3
       })

       output$FPIoutput_tbl <- renderPlot({
         if (!is.null(Info$file)) {
           output_scores_TBL(Info$Summary, input$baseline, BaseLine, FPI_2$Summary)
         }
       }, width=function() {
         dims <- window_dims()
         dims[1]*0.3
       }, height=function() {
         dims <- window_dims()
         dims[1]*0.3
       })

       output$FPIinput <- renderPlot({
         if (!is.null(Info$file)) {
           input_dim_scores(Info$Summary, input$baseline, BaseLine, FPI_2$Summary)
         }
       }, width=function() {
         dims <- window_dims()
         dims[1]*0.3
       }, height=function() {
         dims <- window_dims()
         dims[1]*0.3
       })

       observeEvent(input$Load2, {
         FPI_2$file <- input$Load2
         FPI_2$sheets <- readxl::excel_sheets(FPI_2$file$datapath)
         if (!'4. Summary' %in% FPI_2$sheets) {
           FPI_2$Summary <- NULL
         } else {
           FPI_2$Summary <- readxl::read_excel(FPI_2$file$datapath, sheet='4. Summary', .name_repair = 'minimal')
           FPI_2$Output_table <- readxl::read_excel(FPI_2$file$datapath, sheet='5. Output-table', .name_repair = 'minimal')
           FPI_2$Data <- XL2Data(name=FPI_2$file$datapath, sheet='12. Fishery Data')
           FPI_2$openMSE.Qs <- readxl::read_excel(FPI_2$file$datapath, sheet='13. openMSE Questions', .name_repair = 'minimal')
           FPI_2$FPI.Inputs <- readxl::read_excel(FPI_2$file$datapath, sheet='6. Input-table', .name_repair = 'minimal')
           FPI_2$FPI.Cover <- readxl::read_excel(FPI_2$file$datapath, sheet='3. Cover Page', .name_repair = 'minimal')
         }
       })

       observeEvent(input$remove, {
         FPI_2$Summary <- NULL
       })


       output$downloadFPIRep <- downloadHandler(
         filename = function() {
           if (input$filetype == 'html') {
             paste("FPI_Report", Sys.Date(), ".html", sep="")
           } else {
             paste("FPI_Report", Sys.Date(), ".pdf", sep="")
           }

         },
         content = function(file) {

           if (input$filetype == 'html') {
             output_format <- 'html_document'
           } else {
             output_format <- 'pdf_document'
           }
           AM("------------- Generating FPI Report --------------")
           AM(paste0("File type: ", input$filetype))
           AM(paste0("output_format: ", output_format))

           GenFPIreport(Info, input$baseline, BaseLine, FPI_2$Summary, file, output_format)

         }
       )




     }
  )
}

GenFPIreport <- function(Info,input_baseline, BaseLine, FPI_2_Summary, file, output_format) {
  FPIReport <- "RMD/FPIReport.rmd" #file.path(tempdir(), "report.Rmd")
  # file.copy("RMD/FPIReport.rmd", FPIReport, overwrite = TRUE)

  params <- list(Info=Info,
                 input_baseline=input_baseline,
                 BaseLine=BaseLine,
                 FPI_2_Summary=FPI_2_Summary)

  rmarkdown::render(FPIReport, output_file = file,
                    output_format = output_format,
                    params = params,
                    envir = new.env(parent = globalenv()))
}


makemetadata <- function(metadata) {
  # drop empty columns
  ind <- apply(apply(metadata, 2, is.na), 2, all)
  metadata <- metadata[,!ind]

  # drop empty rows
  ind <- apply(apply(metadata, 2, is.na), 1, all)
  metadata <- metadata[!ind,]

  names <- c('A. Country',
             'B. Location',
             'C. Fishery',
             'D. Single or Multi-species',
             'E. Species',
             'F. Date',
             'G. Reference',
             'H. Author',
             'I. Author')

  # get info - could be after : or presumably on the next line(s)
  get_info <- function(i=1, metadata, names) {
    text <- unlist(metadata[,1])
    ind <- which(grepl(names[i], text))

    # is it after :?
    title <- strsplit(text[ind], ":")[[1]][1]
    txt <- strsplit(text[ind], ":")[[1]][2]
    if (is.na(txt)) txt <- ''
    if (nchar(txt)<1) txt <- ''
    # more on next line
    y <- 0
    more <- TRUE
    while (more) {
      y <- y+1
      chk <- grepl(names[i+y], text[ind+y])
      if (is.na(chk) || chk) more <- FALSE
      txt <- paste(txt, sep=", ")
    }
    c(title, txt)
  }

  DF <- t(sapply(1:length(names), get_info, metadata=metadata, names=names))
  DF[,1] <- paste0('<strong>', DF[,1], '</strong>')
  DF
}



