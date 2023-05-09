options(shiny.maxRequestSize=1000*1024^2)


server <- function(input, output, session) {
  useShinyjs()

  # ---- Initialize Reactive Values -----
  Toggles <- reactiveValues(
    FPI_Loaded=FALSE, # FPI loaded?
    OM_Loaded=FALSE, # OM successfully generated?
    Fit=FALSE,    # Model fitted?
    MSE=FALSE)    # MSE run?

  output$FPI_Loaded <- reactive({ Toggles$FPI_Loaded })
  outputOptions(output, "FPI_Loaded", suspendWhenHidden = FALSE)

  output$OM_Loaded <- reactive({ Toggles$OM_Loaded })
  outputOptions(output, "OM_Loaded", suspendWhenHidden = FALSE)

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
    openMSE.Qs = NULL,     # M
    FPI.Inputs = NULL,  # FPI inputs table
    FPI.Cover = NULL,   # FPI cover sheet
    OM = NULL,          # Operating model
    MSEhist = NULL,     # Historical reconstruction
    MSEproj = NULL,     # MSE projection
    MPsel = c("Current_Catch","Current_Effort")        # Selected MPs for running the MSE
  )

  # FPI comparision
  FPI_2 <- reactiveValues(
    file=NULL,          # input file
    sheets=NULL,        # Sheet names
    Summary=NULL,       # FPI summary tab
    Output_table=NULL,  # FPI output-table
    Input_table=NULL,   # FPI input-table
    Data = NULL,        # Data
    MERA.Qs = NULL,     # M
    FPI.Inputs = NULL,  # FPI inputs table
    FPI.Cover = NULL   # FPI cover sheet
  )

  # dynamic plot dimensions
  window_dims <- reactive(input$dimension)

  # Server calls
  Home_Server('home')
  Load_Server('load', Info=Info, Toggles=Toggles)
  FPI_Server('FPI',Info=Info, FPI_2=FPI_2)
  Dynamics_Server('dynamics', Info=Info, Toggles=Toggles)
  Results_Server('results',Info=Info)

  USERID<-Sys.getenv()[names(Sys.getenv())=="USERNAME"]
  SessionID<-paste0(USERID,"-",strsplit(as.character(Sys.time())," ")[[1]][1],"-",strsplit(as.character(Sys.time())," ")[[1]][2])
  output$SessionID<-renderText(SessionID)

  output$package_versions <- renderUI({
    tagList(
      h6(
        paste("FPAT", packageVersion('FPAT'),
              "openMSE", packageVersion("openMSE"),
              "MSEtool", packageVersion("MSEtool"),
              "DLMtool", packageVersion("DLMtool"),
              "SAMtool", packageVersion("SAMtool"))

      )
    )
  })
}

