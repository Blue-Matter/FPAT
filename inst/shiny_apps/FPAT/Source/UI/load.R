

Load_UI <- function(id, label="Load") {

  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
             br(),
             h3('Load an FPAT Data File'),
             p('The FPAT data file is a specially formatted Excel Workbook that contains the FPI scores and any available fishery data.',
             'You can load your FPAT spreadsheet or select an existing FPAT case study.'),
             p('Once the data file is loaded, FPAT will build an operating model (OM) and simulate the historical fishing dynamics.
               This may take a few minutes to complete.')
      )
      ),
      fluidRow(
      box(width=4, height = 100, status='primary', solidHeader = TRUE,
          title="Load an FPAT Data File (.xlsx)",
          tipify(
            fileInput(ns("Load"),label=NULL),
            title="An FPAT spreadsheet contains FPI input and output scores, fishery data and additional questions for specifying an operating model")
      ),
      box(width=4, height = 100, status='primary', solidHeader = TRUE,
          title="Select an existing FPAT case study",
          div(style="display: inline-block;vertical-align:top; width: 250px;",
              column(6,
                     tipify(placement ='top',
                       selectInput(ns("Select"),choices=c("Demo_1","Demo_2"),label=NULL),
                       title='Select an existing FPAT case study')
              ),
              column(6,
                     actionButton(ns("LoadSelected"),label="Load case study",icon=icon("cloud-upload"))
              )
          )
      )
    )
  )
}

Load_Server <- function(id, Info, Toggles) {
  moduleServer(id,
    function(input, output, session) {

      output$Intro <- renderText({
        "Load an FPAT"
      })

      observeEvent(input$LoadSelected,{
        if(input$Select=="Demo_1")  Info$file <- list(datapath = "./Data/Demo_1.xlsx")
        if(input$Select=="Demo_2")  Info$file <- list(datapath = "./Data/Demo_2.xlsx")

        AM(Info$file)
        fetchOM(Info, Toggles, session)
      })


      observeEvent(input$Load, {
        Info$file <- input$Load
        fetchOM(Info, Toggles, session)
      }) # end of observe load

    }
  )
}

