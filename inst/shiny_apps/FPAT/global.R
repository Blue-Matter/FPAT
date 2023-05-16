library(dplyr)
library(DT)
library(ggplot2)
library(ggrepel)
library(readxl)
library(shinyBS)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinyjs)
library(shinyalert)
library(openMSE)
library(cowplot)
library(tidyr)

source('home.r')
source('load.R')
source('FPI.R')
source('dynamics.R')
source('results.R')

for (fl in list.files("./Source")) source(file.path("./Source", fl))

CheckFPILoaded <- function(Info) {
  renderUI({
    if(is.null(Info$sheets))
      return(h4('FPAT data file not loaded. Please return to Load and load an FPAT data file', style = "color:red"))
  })
}

CheckOMLoaded <- function(Info) {
  renderUI({
    if(is.null(Info$file))
      return(h4('FPAT data file not loaded. Please return to Load and load an FPAT data file', style = "color:red"))
    if(class(Info$Data)!='Data')
      return(h4('FPAT data file not loaded. Please return to Load and load an FPAT data file', style = "color:red"))
    if(is.null(Info$MSEhist))
      return(h4('Operating model could not be built from this FPAT file.', style = "color:red"))
  })
}

# Shared variables

Current_Year<<-as.integer(substr(Sys.time(),start=1,stop=4))

# CurrentYr<<-2021 # as.integer(input$Lyear) #as.integer(substr(as.character(Sys.time()),1,4))
# Syear<<-1951
# Lyear<<-2018


baseline_file <- './Data/Baseline.xlsx'

BaseLine <<- readxl::read_excel(baseline_file) # baseline FPI scores

options <- BaseLine[1,2:ncol(BaseLine)]

categories_all <- colnames(options)
categories <- categories_all[!grepl('\\...', categories_all)]

BaseLineChoices <- list()
for (i in seq_along(categories)) {
  cat <- categories[i]
  fst <- which(categories_all ==categories[i])
  lst <- which(categories_all ==categories[i+1])-1
  if (length(lst)<1) lst <- length(options)
  opts <- options[fst:lst]

  BaseLineChoices[[cat]] <- as.character(opts[1,])
}

BaseLineChoices <<- BaseLineChoices

# Rename MPs
Current_Catch <- CurC
Current_Effort <- curEref
Size_Limit_1 <- matlenlim
Size_Limit_2 <- matlenlim2
Length_Targeting_1 <- Ltarget1
Length_Targeting_2 <- Ltarget2
Index_Targeting_1 <- Itarget1
Index_Targeting_2 <- Itarget2


# spatial MPs

Open_Existing <- function(x, Data, ...) {
  rec <- new("Rec")
  rec@Allocate <- 1
  rec@Spatial <- rep(1, 3)
  rec
}
class(Open_Existing) <- 'MP'


Close_Planned <- function(x, Data, ...) {
  rec <- new("Rec")
  rec@Allocate <- 1
  rec@Spatial <- c(0,1,0)
  rec
}
class(Close_Planned) <- 'MP'


Current_Catch <- CurC

Current_Effort <- curE


Check_Sheets <- function(Info) {

  # Check all required sheet names exist
  req_sheets <- c('Cover Page', 'Summary', 'Output-table',
                  'Input-table', 'Fishery Data', 'FPAT App Questions', 'Effort Dynamics')

  sheet_df <- data.frame(Sheet=req_sheets, Number=NA)
  for (i in 1:length(req_sheets)) {
    ind <- which(grepl(req_sheets[i], Info$sheets)==TRUE)
    if (length(ind)!=0) {
      sheet_df$Number[i] <- ind
    }
  }

  # Error on missing (Effort Dynamics is optional)
  sheet_df_chk <- sheet_df %>% dplyr::filter(Sheet !='Effort Dynamics')
  ind <- sheet_df_chk$Number %>% is.na() %>% which()

  missing_sheets <- numeric()
  if (length(ind)>0) {
    missing_sheets <- sheet_df_chk[ind,1]
  }

  if (length(missing_sheets)>0) {
    e <- paste('Loaded file is missing required sheet(s): ', paste(missing_sheets, collapse=", "))
    AM(paste0(e,"\n"))
    shinyalert("FPAT file did not import.", paste("Error:",e), type = "error")
    AM(paste0(e,"\n"))
    return(0)
  }

  sheet_df$Sheet_Name <- NA
  for (i in 1:nrow(sheet_df)) {
    sheet_df$Sheet_Name[i] <- paste0(sheet_df$Number[i], ". ", sheet_df$Sheet[i])
  }

  get_name <- function(sheet, sheet_df) {
    ind <- which(grepl(sheet, sheet_df$Sheet))
    sheet_df$Sheet_Name[ind]
  }

  # Get sheet numbers
  Cover_Page <- get_name('Cover Page', sheet_df)
  Summary <- get_name('Summary', sheet_df)
  Output_table <- get_name('Output-table', sheet_df)
  Input_table <- get_name('Input-table', sheet_df)
  FPAT_App_Questions <- get_name('FPAT App Questions', sheet_df)
  Fishery_Data <- get_name('Fishery Data', sheet_df)
  Effort_Dynamics<- get_name('Effort Dynamics', sheet_df)

  Info$Sheet_Names <- list()
  Info$Sheet_Names$Cover_Page <- Cover_Page
  Info$Sheet_Names$Summary <- Summary
  Info$Sheet_Names$Output_table <- Output_table
  Info$Sheet_Names$Input_table <- Input_table
  Info$Sheet_Names$FPAT_App_Questions <- FPAT_App_Questions
  Info$Sheet_Names$Fishery_Data <- Fishery_Data
  Info$Sheet_Names$Effort_Dynamics <- Effort_Dynamics


  # Load FPI sheets
  Info$FPI.Cover <- readxl::read_excel(Info$file$datapath, sheet=Cover_Page, .name_repair = 'minimal')
  Info$Summary <- readxl::read_excel(Info$file$datapath, sheet=Summary, .name_repair = 'minimal')
  Info$Output_table <- readxl::read_excel(Info$file$datapath, sheet=Output_table, .name_repair = 'minimal')
  Info$FPI.Inputs <- readxl::read_excel(Info$file$datapath, sheet=Input_table, .name_repair = 'minimal')
  if (!grepl('NA', Effort_Dynamics)) {
    Info$Effort_Dynamics <- readxl::read_excel(Info$file$datapath, sheet=Effort_Dynamics, .name_repair = 'minimal')
  }

  # Load openMSE sheets
  Info$openMSE.Qs <- readxl::read_excel(Info$file$datapath, sheet=FPAT_App_Questions, .name_repair = 'minimal')
  Info$Data <- try(XL2Data(name=Info$file$datapath, sheet=Fishery_Data), silent=TRUE)

  Info
}

# Load indicator descriptions
IndDesc <- read.csv('Data/Indicator_Descriptions.csv')
IndDesc$Metric <- trimws(IndDesc$Metric)
IndDesc <<- IndDesc
