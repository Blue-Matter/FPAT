


Load_UI <- function(id, label="Load") {

  ns <- NS(id)
  tagList(
    column(12,style="height: 700px",
       HTML('<br>'),
       htmlOutput(ns("Intro")),
       HTML('<br>'),
       h5("Load an FPAT spreadsheet (.xlsx)",style="color:#347ab6"),
       column(12,
              tipify(fileInput("Load",label=NULL),title="An FPAT spreadsheet contains FPI input and outputs, fishery data and MERA questions for specifying an operating model")),
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

    }
  )
}

