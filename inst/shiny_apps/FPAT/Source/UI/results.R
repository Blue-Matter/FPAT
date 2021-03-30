


Results_UI <- function(id, label="Results") {

  ns <- NS(id)
  tagList(
    column(12,style="height: 700px",
       HTML('<br>'),
       htmlOutput(ns("Intro")),
       HTML('<br>'),
       tabsetPanel(

         tabPanel(h5("Management performance",style = "color:black"), HTML("<br>"),value=1),
         tabPanel(h5("Environmental outcomes",style = "color:black"), HTML("<br>"), value=2),
         tabPanel(h5("Social outcomes",style = "color:black"), HTML("<br>"), value=3),
         tabPanel(h5("Economic outcomes",style = "color:black"), HTML("<br>"), value=4)

       )
    )
  )

}

Results_Server <- function(id) {
  moduleServer(id,
    function(input, output, session) {

      output$Intro <- renderText({
        "This panel contains the results of the combined FPI - openMSE analyses"
      })

    }
  )
}

