

options(shiny.maxRequestSize=1000*1024^2)

server <- function(input, output, session) {

  # ---- Initialize Reactive Values -----
  Toggles <- reactiveValues(
    Loaded=FALSE, # FPAT loaded?
    Fit=FALSE,    # Model fitted?
    MSE=FALSE)    # MSE run?

  output$Loaded <- reactive({ Toggles$Loaded })
  outputOptions(output, "Loaded", suspendWhenHidden = FALSE)

  output$Fit <- reactive({ Toggles$Fit })
  outputOptions(output, "Fit", suspendWhenHidden = FALSE)

  output$MSE <- reactive({ Toggles$MSE })
  outputOptions(output, "MSE", suspendWhenHidden = FALSE)


  # Log ----------------------------------------------------------
  #output$Log<-renderText(Log_text$text)

  # splash page
  Home_Server('Home1')
  Fishery_Server('Fishery1')
  Manage_Server('Manage1')
  Help_Server('Help1')

  USERID<-Sys.getenv()[names(Sys.getenv())=="USERNAME"]
  SessionID<-paste0(USERID,"-",strsplit(as.character(Sys.time())," ")[[1]][1],"-",strsplit(as.character(Sys.time())," ")[[1]][2])
  output$SessionID<-renderText(SessionID)

}
