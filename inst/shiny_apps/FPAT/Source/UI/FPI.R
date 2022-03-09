options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))

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

       output$checkloaded <- CheckFPILoaded(Info)

       output$FPI_results <- renderUI({
         if(!is.null(Info$FPI.Cover)) {
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
                              fluidRow(
                                column(7,
                                       plotOutput(ns('FPIoutput_sector'), width='100%', height="100%")
                                ),
                                column(5,
                                       h3('Outputs: Sector'),
                                       htmlOutput(ns('FPI_output_sector_text')),
                                       p('Select the desired benchmark from the drop down menu or load another FPI database to compare scores to identify dimensions where other fisheries have found ways to perform better.')
                                )
                              ),
                              fluidRow(
                                column(6,
                                       h3('Individual FPI Scores'),
                                       htmlOutput(ns('output_tableui'))
                                       ),
                                column(6,
                                       h3('Comments'),
                                       textAreaInput(ns("output_sector_comment"), "Discussion Text",
                                                      placeholder='Copy/paste discussion text here to be included in report.',
                                                      rows=10),
                                       )
                              )
                     ),
                     tabPanel(h5('Outputs: Triple Bottom Line', style='color:black;'),
                              value=2,
                              br(),
                              fluidRow(
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
                              fluidRow(
                                column(6,
                                       h3('Individual FPI Scores'),
                                       htmlOutput(ns('output_tbl_tableui'))
                                ),
                                column(6,
                                       h3('Comments'),
                                       textAreaInput(ns("output_tbl_comment"), "Discussion Text",
                                                     placeholder='Copy/paste discussion text here to be included in report.',
                                                     rows=10),
                                )
                              )

                     ),
                     tabPanel(h5('Inputs: Enabling Conditions', style='color:black;'),
                              value=3,
                              br(),
                              fluidRow(
                                column(7,
                                       plotOutput(ns('FPIinput'), width='100%', height="100%")
                                ),
                                column(5,
                                       h3('Inputs: Enabling Conditions'),
                                       htmlOutput(ns('FPI_input_enabling')),
                                       p('Select the desired benchmark from the drop down menu, and compare scores to identify dimensions where the fishery has different levels of enabling conditions than typical fisheries of the same category.')
                                )
                              ),
                              fluidRow(
                                column(6,
                                       h3('Individual FPI Scores'),
                                       htmlOutput(ns('input_tableui'))
                                ),
                                column(6,
                                       h3('Comments'),
                                       textAreaInput(ns("input_comment"), "Discussion Text",
                                                     placeholder='Copy/paste discussion text here to be included in report.',
                                                     rows=10),
                                )
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
                       column(8, fileInput(ns("Load2"),label=NULL, accept=c('.xls', '.xlsx'))),
                       column(4, actionButton(ns('remove'), 'Remove'))

               ),
               box(width=3,status='primary', solidHeader = TRUE,
                   title='Download FPI Report',
                   p('The FPI plots can be downloaded in a FPI Report by clicking the button below.'),
                   # radioButtons(ns('filetype'), 'Report File Type',
                   #              choices = list("HTML" = 'html', "PDF" = 'pdf'), inline=TRUE),
                   downloadButton(ns('downloadFPIRep'), 'Download FPI Report')
               )
             )
           )
           }
         })

       output$output_tableui <- renderUI({
         DF <- makeoutputDF(Info$Summary, Info$Output_table)
         choices <- unique(DF$Dimension)
         ns <- NS(id)
         tagList(
           selectInput(ns('output_dimension'), 'Dimension', choices=choices,
                       selected=choices[1],
                       multiple = FALSE),
           DT::dataTableOutput(ns("output_table"))
         )
       })

       output$output_table <- DT::renderDataTable({
         DF <- makeoutputDF(Info$Summary, Info$Output_table)
         DF[DF$Dimension%in% input$output_dimension,1:2]

       }, escape=FALSE, options = list(dom = 't', pageLength=40), rownames= FALSE)


       output$output_tbl_tableui <- renderUI({
         DF <- makeoutput_tblDF(Info$Summary, Info$Output_table)
         choices <- unique(DF$Dimension)
         ns <- NS(id)
         tagList(
           selectInput(ns('output_tbl_dimension'), 'Dimension', choices=choices,
                       selected=choices[1],
                       multiple = FALSE),
           DT::dataTableOutput(ns("output_tbl_table"))
         )
       })

       output$output_tbl_table <- DT::renderDataTable({
         DF <- makeoutput_tblDF(Info$Summary, Info$Output_table)
         DF[DF$Dimension%in% input$output_tbl_dimension,1:2]

       }, escape=FALSE, options = list(dom = 't', pageLength=40), rownames= FALSE)

       output$input_tableui <- renderUI({
         DF <- makeinputDF(Info$Summary, Info$FPI.Inputs)
         choices <- unique(DF$Dimension)
         ns <- NS(id)
         tagList(
           selectInput(ns('input_dimension'), 'Dimension', choices=choices,
                       selected=choices[1],
                       multiple = FALSE),
           DT::dataTableOutput(ns("input_table"))
         )
       })

       output$input_table <- DT::renderDataTable({
         DF <- makeinputDF(Info$Summary, Info$FPI.Inputs)
         DF[DF$Dimension%in% input$input_dimension,1:2]

       }, escape=FALSE, options = list(dom = 't', pageLength=40), rownames= FALSE)


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
         load_sheets <- try(readxl::excel_sheets(FPI_2$file$datapath), silent=TRUE)
         if (class(load_sheets)!='try-error') {
           FPI_2$sheets <- load_sheets
           if (!'4. Summary' %in% FPI_2$sheets) {
             FPI_2$Summary <- NULL
             AM("------------- Loaded file is not a valid FPI data file --------------")
             AM(paste0("File: ",   FPI_2$file))
             shinyalert("Loaded file is not a valid FPI data file", type = "error")
           } else {
             FPI_2$Summary <- readxl::read_excel(FPI_2$file$datapath, sheet='4. Summary', .name_repair = 'minimal')
             FPI_2$Output_table <- readxl::read_excel(FPI_2$file$datapath, sheet='5. Output-table', .name_repair = 'minimal')
             FPI_2$Data <- XL2Data(name=FPI_2$file$datapath, sheet='12. Fishery Data')
             FPI_2$openMSE.Qs <- readxl::read_excel(FPI_2$file$datapath, sheet='13. openMSE Questions', .name_repair = 'minimal')
             FPI_2$FPI.Inputs <- readxl::read_excel(FPI_2$file$datapath, sheet='6. Input-table', .name_repair = 'minimal')
             FPI_2$FPI.Cover <- readxl::read_excel(FPI_2$file$datapath, sheet='3. Cover Page', .name_repair = 'minimal')
             AM("------------- Comparision FPI loaded --------------")
             AM(paste0("File: ",   FPI_2$file))
           }
         } else {
           AM("------------- Loaded file is not a valid FPI data file --------------")
           AM(paste0("File: ",   FPI_2$file))
           shinyalert("Loaded file is not a valid FPI data file", type = "error")
         }
       })

       observeEvent(input$remove, {
         FPI_2$Summary <- NULL
       })

       output$downloadFPIRep <- downloadHandler(
         filename = function() {
           # if (input$filetype == 'html') {
           #   paste("FPI_Report", Sys.Date(), ".html", sep="")
           # } else {
           #   paste("FPI_Report", Sys.Date(), ".pdf", sep="")
           # }
           paste("FPI_Report", Sys.Date(), ".html", sep="")

         },
         content = function(file) {

           # if (input$filetype == 'html') {
           #   output_format <- 'html_document'
           # } else {
           #   output_format <- 'pdf_document'
           # }
           output_format <- 'html_document'
           AM("------------- Generating FPI Report --------------")
           # AM(paste0("File type: ", input$filetype))
           AM(paste0("output_format: ", output_format))

           comment_text <- list()
           comment_text$output_sector <- input$output_sector_comment
           comment_text$output_tbl <- input$output_tbl_comment
           comment_text$input <- input$input_comment

           desc_text <- list()
           desc_text$output <- readLines('Data/FPI_output_sector.txt')
           desc_text$output_tpl <- readLines('Data/FPI_output_tpl.txt')
           desc_text$input <- readLines('Data/FPI_input_enabling.txt')


           GenFPIreport(Info, input$baseline, BaseLine, FPI_2$Summary, file, output_format,
                        comment_text, desc_text)

         }
       )




     }
  )
}

GenFPIreport <- function(Info,input_baseline, BaseLine, FPI_2_Summary, file, output_format, comment_text, desc_text) {


  FPIReport <- file.path(tempdir(), "FPIReport.rmd")
  file.copy("RMD/FPIReport.rmd", FPIReport, overwrite = TRUE)

  params <- list(Info=Info,
                 input_baseline=input_baseline,
                 BaseLine=BaseLine,
                 FPI_2_Summary=FPI_2_Summary,
                 comment_text=comment_text,
                 desc_text=desc_text)

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


removeNAs <- function(DF, col=1) {
  for (i in 1:nrow(DF)) {
    if (is.na(DF[i,col])) DF[i,col] <- DF[i-1,col]
  }
  DF
}
add.dimension <- function(DF, Dimensions) {
  DF <- removeNAs(DF)
  tab <- table(DF)
  tab <- tab[match(unique(DF[,1]), names(tab))]
  tab <- as.numeric(tab)
  rep(Dimensions, tab)
}

add_average <- function(df) {
  df$Score <- suppressWarnings(as.numeric(df$Score))
  mean <- round(mean(df$Score, na.rm=T),2)
  bind_rows(df, data.frame(Measure='<b>Mean</b>',
                           Score=mean,
                           Dimension=unique(df$Dimension)))
}

makeoutputDF <- function(FPI.Summary, Output_table) {
  FPI.Summary <<- FPI.Summary
  Output_table <<- Output_table
  # Output Dimension Scores

  # Average Scores
  Avg.Scores <- FPI.Summary[7:17,1:3] %>% data.frame()
  Avg.Scores[,3] <- as.numeric(Avg.Scores[,3])
  colnames(Avg.Scores) <- c('Indicator', 'Dimension', 'Average')
  Avg.Scores <- removeNAs(Avg.Scores)

  # Individual Component Scores
  # come from Summary and Output-table sheets
  Dimensions <- Avg.Scores$Dimension

  List <- list()

  df <- data.frame(Output_table[2:15,c(4,7)])
  colnames(df) <- c('Measure', 'Score')
  df$Dimension <- 'Ecologically Sustainable Fisheries'
  List[[1]] <- df

  df <- data.frame(Output_table[16:23,c(4,7)])
  colnames(df) <- c('Measure', 'Score')
  df$Dimension <- 'Harvest Performance'
  List[[2]] <- df

  df <- data.frame(Output_table[24:29,c(4,7)])
  colnames(df) <- c('Measure', 'Score')
  df$Dimension <- 'Harvest Asset Performance'
  List[[3]] <- df

  df <- data.frame(Output_table[30:36,c(4,7)])
  colnames(df) <- c('Measure', 'Score')
  df$Dimension <- 'Risks'
  List[[4]] <- df

  df <- data.frame(Output_table[37:42,c(4,7)])
  colnames(df) <- c('Measure', 'Score')
  df$Dimension <- 'Owners, Permit Holders & Captains'
  List[[5]] <- df

  df <- data.frame(Output_table[43:50,c(4,7)])
  colnames(df) <- c('Measure', 'Score')
  df$Dimension <- 'Crew'
  List[[6]] <- df

  df <- data.frame(Output_table[51:57,c(4,7)])
  colnames(df) <- c('Measure', 'Score')
  df$Dimension <- 'Market Performance'
  List[[7]] <- df

  df <- data.frame(Output_table[58:63,c(4,7)])
  colnames(df) <- c('Measure', 'Score')
  df$Dimension <- 'Post-harvest Industry Performance'
  List[[8]] <- df

  df <- data.frame(Output_table[64:66,c(4,7)])
  colnames(df) <- c('Measure', 'Score')
  df$Dimension <- 'Post-Harvest Asset Performance'
  List[[9]] <- df

  df <- data.frame(Output_table[67:72,c(4,7)])
  colnames(df) <- c('Measure', 'Score')
  df$Dimension <- 'Processing Owners & Managers'
  List[[10]] <- df

  df <- data.frame(Output_table[73:79,c(4,7)])
  colnames(df) <- c('Measure', 'Score')
  df$Dimension <- 'Processing Workers'
  List[[11]] <- df

  List <- lapply(List, add_average)

  do.call('rbind', List)

}




makeoutput_tblDF <- function(FPI.Summary, Output_table) {
  FPI.Summary <<- FPI.Summary
  Output_table <<- Output_table
  # Output TBL Dimension Scores

  # Average Scores
  Avg.Scores <- FPI.Summary[42:55,1:3] %>% data.frame()
  Avg.Scores[,3] <- as.numeric(Avg.Scores[,3])
  colnames(Avg.Scores) <- c('Indicator', 'Dimension', 'Average')
  Avg.Scores <- removeNAs(Avg.Scores)

  # Individual Component Scores
  # come from Summary and Output-table sheets
  Dimensions <- Avg.Scores$Dimension

  List <- list()

  # Stock Health

  df <- data.frame(Output_table[2:15,c(4,7)])
  colnames(df) <- c('Measure', 'Score')
  df$Dimension <- 'Stock Health'
  List[[1]] <- df

  # Harvest
  df <- data.frame(FPI.Summary[9:11,15:16])
  colnames(df) <- c('Measure', 'Score')
  df$Dimension <- 'Harvest'
  List[[2]] <- df

  # Harvest Assets
  df <- data.frame(FPI.Summary[13:18,15:16])
  colnames(df) <- c('Measure', 'Score')
  df$Dimension <- 'Harvest Assets'
  List[[3]] <- df

  # Risk
  df <- data.frame(FPI.Summary[19:24,15:16])
  colnames(df) <- c('Measure', 'Score')
  df$Dimension <- 'Risk'
  List[[4]] <- df

  # Trade
  df <- data.frame(FPI.Summary[42:45,15:16])
  colnames(df) <- c('Measure', 'Score')
  df$Dimension <- 'Trade'
  List[[5]] <- df

  # Product Form
  df <- data.frame(FPI.Summary[c(47:50, 41,46),15:16])
  colnames(df) <- c('Measure', 'Score')
  df$Dimension <- 'Product Form'
  List[[6]] <- df

  # Post-Harvest Asset Performance
  df <- data.frame(FPI.Summary[53:55,15:16])
  colnames(df) <- c('Measure', 'Score')
  df$Dimension <- 'Post-Harvest Asset Performance'
  List[[7]] <- df

  # Managerial Returns
  df <- data.frame(FPI.Summary[c(26:27, 30, 56:57, 60),15:16])
  colnames(df) <- c('Measure', 'Score')
  df$Dimension <- 'Managerial Returns'
  List[[8]] <- df

  # Labor Returns
  df <- data.frame(FPI.Summary[c(32:33, 36, 62:64),15:16])
  colnames(df) <- c('Measure', 'Score')
  df$Dimension <- 'Labor Returns'
  List[[9]] <- df

  # Health & Sanitation
  df <- data.frame(FPI.Summary[c(12, 29,25,59,66,51),15:16])
  colnames(df) <- c('Measure', 'Score')
  df$Dimension <- 'Health & Sanitation'
  List[[10]] <- df

  # Community Services
  df <- data.frame(FPI.Summary[c(52, 25, 28,34,58,65),15:16])
  colnames(df) <- c('Measure', 'Score')
  df$Dimension <- 'Community Services'
  List[[11]] <- df

  # Local Ownership
  df <- data.frame(FPI.Summary[c(31,61),15:16])
  colnames(df) <- c('Measure', 'Score')
  df$Dimension <- 'Local Ownership'
  List[[12]] <- df

  # Local Labor
  df <- data.frame(FPI.Summary[c(37,67),15:16])
  colnames(df) <- c('Measure', 'Score')
  df$Dimension <- 'Local Labor'
  List[[13]] <- df

  # Career
  df <- data.frame(FPI.Summary[c(38:39, 68),15:16])
  colnames(df) <- c('Measure', 'Score')
  df$Dimension <- 'Career'
  List[[14]] <- df

  List <- lapply(List, add_average)

  do.call('rbind', List)

}

makeinputDF <- function(FPI.Summary, FPI.Inputs) {

  FPI.Summary <<- FPI.Summary
  FPI.Inputs <<- FPI.Inputs

  # Input Dimension Scores

  # Average Scores
  Avg.Scores <- FPI.Summary[23:37,1:3] %>% data.frame()
  Avg.Scores[,3] <- as.numeric(Avg.Scores[,3])
  colnames(Avg.Scores) <- c('Indicator', 'Dimension', 'Average')
  Avg.Scores <- removeNAs(Avg.Scores)

  # Individual Component Scores
  # come from Summary and Input-table sheets
  Dimensions <- Avg.Scores$Dimension

  List <- list()

  df <- data.frame(FPI.Inputs[2,c(3,6)])
  colnames(df) <- c('Measure', 'Score')
  df$Dimension <- 'National Environmental Performance'
  List[[1]] <- df

  df <- data.frame(FPI.Inputs[3:7,c(3,6)])
  colnames(df) <- c('Measure', 'Score')
  df$Dimension <- 'Environmental Risk'
  List[[2]] <- df

  df <- data.frame(FPI.Inputs[8:9,c(3,6)])
  colnames(df) <- c('Measure', 'Score')
  df$Dimension <- 'National Governance'
  List[[3]] <- df

  df <- data.frame(FPI.Inputs[10:11,c(3,6)])
  colnames(df) <- c('Measure', 'Score')
  df$Dimension <- 'National Economics'
  List[[4]] <- df

  df <- data.frame(FPI.Inputs[12:17,c(3,6)])
  colnames(df) <- c('Measure', 'Score')
  df$Dimension <- 'Fishing Access Rights'
  List[[5]] <- df

  df <- data.frame(FPI.Inputs[18:23,c(3,6)])
  colnames(df) <- c('Measure', 'Score')
  df$Dimension <- 'Harvest Rights'
  List[[6]] <- df

  df <- data.frame(FPI.Inputs[24:26,c(3,6)])
  colnames(df) <- c('Measure', 'Score')
  df$Dimension <- 'Collective Action'
  List[[7]] <- df

  df <- data.frame(FPI.Inputs[27:28,c(3,6)])
  colnames(df) <- c('Measure', 'Score')
  df$Dimension <- 'Participation & Support'
  List[[8]] <- df

  df <- data.frame(FPI.Inputs[29:30,c(3,6)])
  colnames(df) <- c('Measure', 'Score')
  df$Dimension <- 'Leadership & Cohesion'
  List[[9]] <- df

  df <- data.frame(FPI.Inputs[31:34,c(3,6)])
  colnames(df) <- c('Measure', 'Score')
  df$Dimension <- 'Gender'
  List[[10]] <- df

  df <- data.frame(FPI.Inputs[35:38,c(3,6)])
  colnames(df) <- c('Measure', 'Score')
  df$Dimension <- 'Management Capacity'
  List[[11]] <- df

  df <- data.frame(FPI.Inputs[39:40,c(3,6)])
  colnames(df) <- c('Measure', 'Score')
  df$Dimension <- 'Data'
  List[[12]] <- df

  df <- data.frame(FPI.Inputs[41:43,c(3,6)])
  colnames(df) <- c('Measure', 'Score')
  df$Dimension <- 'Management Methods'
  List[[13]] <- df

  df <- data.frame(FPI.Inputs[44:49,c(3,6)])
  colnames(df) <- c('Measure', 'Score')
  df$Dimension <- 'Markets & Market Institutions'
  List[[14]] <- df

  df <- data.frame(FPI.Inputs[50:55,c(3,6)])
  colnames(df) <- c('Measure', 'Score')
  df$Dimension <- 'Infrastructure'
  List[[14]] <- df

  List <- lapply(List, add_average)
  do.call('rbind', List)
}


