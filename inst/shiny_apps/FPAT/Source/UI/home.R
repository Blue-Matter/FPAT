


Home_UI <- function(id, label="Home") {

  ns <- NS(id)
  tagList(
    verbatimTextOutput(ns("Intro")),
    checkboxInput(ns("test"),"test")
  )

}

Home_Server <- function(id) {
  moduleServer(id,
    function(input, output, session) {

      output$Intro <- renderText({
        "Home text and loading / saving"
      })

    }
  )
}

