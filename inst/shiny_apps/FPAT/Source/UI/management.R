


Manage_UI <- function(id, label="Manage") {

  ns <- NS(id)
  tagList(
    column(12,style="height: 700px",
           HTML('<br>'),
           htmlOutput(ns("Intro")),

    )
  )

}

Manage_Server <- function(id) {
  moduleServer(id,
    function(input, output, session) {

      output$Intro <- renderText({
        "In this panel users select management procedures for testing"
      })

    }
  )
}

