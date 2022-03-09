packages <- c('dplyr', 'DT', 'ggplot2', 'ggrepel', 'shinyWidgets',
              'shiny', 'shinyBS', 'shinydashboard', 'shinyalert',
              'readxl', 'stringr', 'openMSE', 'shinyjs', 'Hmisc')

for (pkg in packages) {
  req <- require(pkg, character.only = TRUE)

  if (!req) {
    install.packages(pkg)
    require(pkg, character.only = TRUE)
  }
}

# library(dplyr)
# library(DT)
# library(ggplot2)
# library(ggrepel)
# library(shinyWidgets)
# library(shiny)
# library(shinyBS)
# library(shinydashboard)
# library(shinyalert)
# library(readxl)
# library(stringr)
# library(openMSE)


#source('../../../R/plotFPIs.R')
for (fl in list.files("./Source/UI")) source(file.path("./Source/UI", fl))
for (fl in list.files("./Source/OM")) source(file.path("./Source/OM", fl),local=T)
for (fl in list.files("./Source/Misc")) source(file.path("./Source/Misc", fl))


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



