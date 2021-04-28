


Home_UI <- function(id, label="Home") {

  ns <- NS(id)
  tagList(
    column(12,style="height: 700px",
           HTML('<br>'),
           h4("This page is a place holder - text to be finalized later but could cover:"),
           HTML('<br>'),
           htmlOutput(ns('Intro')),
           HTML('<br>'),
           htmlOutput(ns('Instruct')),
           HTML('<br>'),
           htmlOutput(ns('Help')),

           a("FPAT User Guide", href="https://blue-matter.github.io/FPAT/FPAT.html")
    #verbatimTextOutput(ns("Intro")),
    )
  )

}

Home_Server <- function(id) {
  moduleServer(id,
    function(input, output, session) {

      output$Intro <- renderText({
        "1. FPAT introduction"
      })

      output$Instruct <- renderText({
        "2. Instructions for using FPAT (1. Load > 2. Examine Inputs > 3. View Results)"
      })

      output$Help <- renderText({
        "3. Getting help using FPAT"

      })

    }
  )
}

