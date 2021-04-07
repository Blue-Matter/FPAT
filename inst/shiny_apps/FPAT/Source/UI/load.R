


Load_UI <- function(id, label="Load") {

  ns <- NS(id)
  tagList(
    column(12,style="height: 700px",
       HTML('<br>'),
       htmlOutput(ns("Intro")),
       HTML('<br>'),
       h5("Load an FPAT spreadsheet (.xlsx)",style="color:#347ab6"),
       column(12,
              tipify(fileInput(ns("Load"),label=NULL),title="An FPAT spreadsheet contains FPI input and outputs, fishery data and MERA questions for specifying an operating model")),
       HTML('<br>'),
       h5("Select an existing FPAT case study",style="color:#347ab6"),
       column(12,
          selectInput("Select",choices=c("Indonesian blue swimmer crab","Mediterranean brown shrimp","Baja red snapper"),label=NULL))

    )
  )

}

Load_Server <- function(id) {
  moduleServer(id,
    function(input, output, session) {

      output$Intro <- renderText({
        "Load an FPAT"
      })

      observeEvent(input$Load, {
        # super vulnerable to changes in the FPI workbook...
        Info$file <- input$Load
        Info$Summary <- readxl::read_excel(Info$file$datapath, sheet='4. Summary', .name_repair = 'minimal')
        Info$Output_table <- readxl::read_excel(FPIfile, sheet='5. Output-table', .name_repair = 'minimal')

        # make the operating model
        OM<-makeOM(FPIfile=input$Load$datapath)
        Toggles$Loaded<-as.integer(!is.null(OM))

      })

    }
  )
}

