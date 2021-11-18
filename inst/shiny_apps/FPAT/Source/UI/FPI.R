

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
                                     p("FPI output scores measure the fishery's performance based on where wealth
                                     is accumulating in the fishery.  Higher scores are better, reflecting that
                                     more wealth is being generated in the stock resource, among fishermen and
                                     the harvest sector, or in the processing sector."),
                                     p('This graph is used to identify the dimensions where the fishery is performing well,
                                     and to target dimensions for improvement.  Different fisheries may have different,
                                     locally identified performance priorities.'),
                                     p('As a general rule, scoring levels have been chosen so that scores below 3 reflect
                                     the need for improvement.  The fishery may also be compared to benchmark scores for
                                     select categories of fisheries, average scores for those fisheries in the FPI database.'),
                                     p('Select the desired benchmark from the drop down menu or load another FPI database to
                                     compare scores to identify dimensions where other fisheries have found ways to perform better.')
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
                                     p("FPI output scores measure the fishery's performance on the pillars
                                       of the triple bottom line.  Higher scores are better, reflecting that
                                       the fishery is attaining more success on ecological, economic or community pillars."),
                                     p('This graph is used to identify the dimensions where the fishery is performing well, and to target dimensions for improvement.
                                       Different fisheries may have different, locally identified performance priorities.  '),
                                     p('As a general rule, scoring levels have been chosen so that scores below 3 reflect
                                     the need for improvement.  The fishery may also be compared to benchmark scores for
                                     select categories of fisheries, average scores for those fisheries in the FPI database.'),
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
                                     p("FPI input scores measure the level of enabling conditions which support fishery performance.
                                       Higher scores reflect more of the enabling condition, though whether or how each input affects
                                       fishery performance is an empirical question.  In some cases, these relationships can be complex,
                                       and depend on the presence of several enabling conditions at once."),
                                     p("One way to evaluate the fishery's enabling conditions is to compare them to benchmark scores for
                                       select categories of fisheries, the average scores for those fisheries in the FPI database.
                                       Select the desired benchmark from the drop down menu, and compare scores to identify dimensions
                                       where the fishery has different levels of enabling conditions than typical fisheries of the same category."),
                                     p('Another use of enabling condition data is to select the enabling conditions that will be altered in hopes
                                       of improving the target performance dimension.  Data from FPI case studies with different levels of
                                       that enabling condition, and other sources, can then be used to evaluate whether changes in that
                                       enabling condition are associated with better performance.'),
                                     p('Select the desired benchmark from the drop down menu or load another FPI database to
                                     compare scores to identify dimensions where other fisheries have found ways to perform better.')
                              )
                     )
                   )
               ),
               box(width=3,status='primary', solidHeader = TRUE,
                   title='FPI Comparisons',
                   shiny::selectInput(ns('baseline'), 'Baseline Comparison',
                                      choices=BaseLineChoices,
                                      multiple = TRUE),
                   h5(strong('Another FPI data file')),
                       column(8, fileInput(ns("Load2"),label=NULL)),
                       column(4, actionButton(ns('remove'), 'Remove'))

               )
             )
           )
           }
         })

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
         FPI_2$file <- iinpnput$Load2
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

     }
  )
}



