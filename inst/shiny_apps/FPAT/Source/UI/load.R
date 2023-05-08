

Load_UI <- function(id, label="Load") {

  ns <- NS(id)
  tagList(
    column(12,
           h3('Load an FPAT Data File'),
           p('The FPAT data file is a specially formatted Excel Workbook that contains the FPI scores and any available fishery data.',
             'You can load your FPAT spreadsheet or select an existing FPAT case study.'),
           p('Once the data file is loaded, FPAT will build an operating model (OM) and simulate the historical fishing dynamics.
               This may take a few minutes to complete.')
    ),

    column(4,
           h3("Load an FPAT Data File (.xlsx)"),
           tipify(fileInput(ns("Load"),label=NULL, accept='.xlsx'),
                  title="An FPAT spreadsheet contains FPI input and output scores, fishery data and additional questions for specifying an operating model"
           )
    ),
    column(4,
           h3("Select an existing FPAT case study"),
           column(4,
                  tipify(placement ='top',
                         selectInput(ns("Select"),choices=c("Costa Rica - Multi-species"),label=NULL, width='250px'),
                         title='Select an existing FPAT case study')
           ),
           column(6,
                  actionButton(ns("LoadSelected"),label="Load case study",icon=icon("cloud-upload-alt"))
           )
    ),
    uiOutput(ns("metadata_box"))


    # column(4,
    #        box(height = 100, status='primary', solidHeader = TRUE,
    #            title="Load an FPAT Data File (.xlsx)",
    #            tipify(
    #              fileInput(ns("Load"),label=NULL, accept='.xlsx'),
    #              title="An FPAT spreadsheet contains FPI input and output scores, fishery data and additional questions for specifying an operating model")
    #        )
    #        ),
    # column(4,
    #        box(height = 100, status='primary', solidHeader = TRUE,
    #     title="Select an existing FPAT case study",
    #     fluidRow(
    #       column(4,
    #              tipify(placement ='top',
    #                     selectInput(ns("Select"),choices=c("Costa Rica - Multi-species"),label=NULL, width='250px'),
    #                     title='Select an existing FPAT case study')
    #       ),
    #       column(6,
    #              actionButton(ns("LoadSelected"),label="Load case study",icon=icon("cloud-upload-alt"))
    #       )
    #     )
    # )
    # ),
    # uiOutput(ns("metadata_box"))
  )

}

Load_Server <- function(id, Info, Toggles) {
  moduleServer(id,
    function(input, output, session) {

      output$Intro <- renderText({
        "Load an FPAT"
      })

      observeEvent(input$LoadSelected,{
        if(input$Select=="Costa Rica - Multi-species")  Info$file <- list(datapath = "./Data/Casestudies/Demo_1.xlsx")

        AM(Info$file)
        fetchOM(Info, Toggles, session)

      })

      observeEvent(input$Load, {
        Info$file <- input$Load
        fetchOM(Info, Toggles, session)
      }) # end of observe load


      output$metadata_box <- renderUI({
        if (!is.null(Info$FPI.Cover)) {
          out <- tagList(
            column(4,
                   h3('Metadata'),
                   tableOutput(session$ns('FPImetadata')
                   )
            )
          )
        } else {
          out <- tagList(
            column(4,
                   h3('Metadata'),
                   p('FPAT file not loaded.')
                   )
            )
        }
        out
      })
      output$FPImetadata <- renderTable({
        if (!is.null(Info$FPI.Cover)) {
          metadata <- Info$FPI.Cover
          makemetadata(metadata)
        }
      }, colnames = FALSE, sanitize.text.function=function(x){x})

    }
  )
}

