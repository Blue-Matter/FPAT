


Manage_UI <- function(id, label="Manage1") {

  ns <- NS(id)
  tagList(
    verbatimTextOutput(ns("Intro")),
    checkboxInput(ns("test"),"test")
  )

}

Manage_Server <- function(id) {
  moduleServer(id,
    function(input, output, session) {

      output$Intro <- renderText({
        "Manage stuff"
      })

    }
  )
}

