library(dplyr)
library(DT)
library(ggplot2)
library(ggrepel)
library(shinyWidgets)
library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinyalert)
library(readxl)
library(stringr)
library(MSEtool)
library(DLMtool)

#source('../../../R/plotFPIs.R')
for (fl in list.files("./Source/UI")) source(file.path("./Source/UI", fl))
for (fl in list.files("./Source/OM")) source(file.path("./Source/OM", fl),local=T)
for (fl in list.files("./Source/Misc")) source(file.path("./Source/Misc", fl))

# Shared variables

Current_Year<<-as.integer(substr(Sys.time(),start=1,stop=4))
CurrentYr<<-2021 # as.integer(input$Lyear) #as.integer(substr(as.character(Sys.time()),1,4))
Syear<<-1951
Lyear<<-2018





