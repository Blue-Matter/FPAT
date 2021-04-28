


Load_UI <- function(id, label="Load") {

  ns <- NS(id)
  tagList(
    column(12,style="height: 700px",
       HTML('<br>'),
       htmlOutput(ns("Intro")),
              HTML('<br>'),
       h5("Select an existing FPAT case study",style="color:#347ab6"),
       column(12,
           div(style="display: inline-block;vertical-align:top; width: 250px;",selectInput(ns("Select"),choices=c("Demo_1","Demo_2"),label=NULL)),
           div(style="display: inline-block;vertical-align:top; width: 250px;",actionButton(ns("LoadSelected"),label="Load case study",icon=icon("cloud-upload")))
          ),
       HTML('<br>'),
       h5("Load an FPAT spreadsheet (.xlsx)",style="color:#347ab6"),
       column(12,
              tipify(fileInput(ns("Load"),label=NULL),title="An FPAT spreadsheet contains FPI input and outputs, fishery data and MERA questions for specifying an operating model")),

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

