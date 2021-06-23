


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
    file=NULL,          # input file
    sheets=NULL,        # Sheet names
    Summary=NULL,       # FPI summary tab
    Output_table=NULL,  # FPI output-table
    Input_table=NULL,   # FPI input-table
    Data = NULL,        # Data
    MERA.Qs = NULL,     # M
    FPI.Inputs = NULL,  # FPI inputs table
    FPI.Cover = NULL,   # FPI cover sheet
    OM = NULL,          # Operating model
    MSEhist = NULL,     # Historical reconstruction
    MSEproj = NULL,     # MSE projection
    MPsel = c("CurC","curEref")        # Selected MPs for running the MSE
  )

  # splash page
  Home_Server('Home1')
  Load_Server('Load1', Info=Info, Toggles=Toggles)
  FPI_Server('FPI1',Info=Info)
  HistDynamics_Server('dynamics', Info=Info)
  Results_Server('Results1',Info=Info)


  USERID<-Sys.getenv()[names(Sys.getenv())=="USERNAME"]
  SessionID<-paste0(USERID,"-",strsplit(as.character(Sys.time())," ")[[1]][1],"-",strsplit(as.character(Sys.time())," ")[[1]][2])
  output$SessionID<-renderText(SessionID)

  # Log stuff
  AM<<-function(newtext)   Log_text$text<-paste(newtext, Log_text$text, sep = "\n")
  Log_text <- reactiveValues(text=paste0("======= Start of Session ======= \nSession ID: ",SessionID,"\nUser ID: ",USERID))
  output$Log <- renderText(Log_text$text)
  output$Download_Log <-downloadHandler(
    filename = function(){"FPAT_Log.txt"},
    content = function(file) {
      writeLines(paste(Log_text$text, collapse = ", "), file)
    }
  )


}
