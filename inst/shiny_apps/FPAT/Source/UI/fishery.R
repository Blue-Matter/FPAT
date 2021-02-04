

Fishery_UI <- function(id, label="Fishery") {

  ns <- NS(id)
  tagList(
    verbatimTextOutput(ns("Intro1")),
    checkboxInput(ns("test1"),"test")
  )

}

Fishery_Server <- function(id) {
  moduleServer(id,
     function(input, output, session) {

       output$Intro1 <- renderText({
         "Fishery plots - OMs data etc"
       })

     }
  )
}
