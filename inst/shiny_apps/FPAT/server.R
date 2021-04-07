


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

  Info <- reactiveValues(
    file=NULL, # input file
    Summary=NULL, # FPI summary tab
    Output_table=NULL, # FPI output-table
    Input_table=NULL#,
    #OM = NULL  # Operating models
  )

  # splash page
  Home_Server('Home1')
  Load_Server('Load1', Info=Info)
  Fishery_Server('Fishery1')
  Manage_Server('Manage1')
  Results_Server('Results1')
  FPI_Server("FPI1")

  USERID<-Sys.getenv()[names(Sys.getenv())=="USERNAME"]
  SessionID<-paste0(USERID,"-",strsplit(as.character(Sys.time())," ")[[1]][1],"-",strsplit(as.character(Sys.time())," ")[[1]][2])
  output$SessionID<-renderText(SessionID)

  # Log stuff
  Log_text <- reactiveValues(text=paste0("-------- Start of Session -------- \nSession ID: ",SessionID,"\nUser ID: ",USERID))
  output$Log <- renderText(Log_text$text)


}
