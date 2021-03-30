

Fishery_UI <- function(id, label="Fishery") {

  ns <- NS(id)
  tagList(
    column(12,style="height: 700px",
      HTML('<br>'),
      htmlOutput(ns("Intro")),
      HTML('<br>'),
      tabsetPanel(

        tabPanel(h5("Stock ",style = "color:black"), HTML("<br>"),value=1),
        tabPanel(h5("Fishery",style = "color:black"), HTML("<br>"),value=2),
        tabPanel(h5("Data",style = "color:black"), HTML("<br>"), value=3),
        tabPanel(h5("Management",style = "color:black"), HTML("<br>"), value=4)

      )
    )
  )

}

Fishery_Server <- function(id) {
  moduleServer(id,
     function(input, output, session) {

       output$Intro <- renderText({
         "This panel contains figures and tables fully documenting the operating model (ie what they have specfied in the FPAT xlsx)"
       })

     }
  )
}
