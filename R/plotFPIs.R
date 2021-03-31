

library(fmsb)

# ---- Functions ----

radar_plot <- function(DF, title, n=25) {
  for (i in 2:nrow(DF)) {
    if (is.na(DF[i,1]))
      DF[i,1] <- DF[i-1,1]
  }

  DF$c <- as.numeric(DF$c)
  # insert new line after n characters
  DF$b <- gsub(paste0('(.{1,',n,'})(\\s|$)'), '\\1\n', DF$b)

  DF2 <- tidyr::pivot_wider(DF, id_cols=3, names_from = 2, values_from=3)
  max <- DF2
  max[max!=5] <- 5
  min <- DF2
  min[min!=0] <- 0
  DF3 <- rbind(max, min, DF2)
  cols <- rev(2:ncol(DF3))
  DF3 <- DF3[, c(1, cols)]



  par(mfrow=c(1,1), oma=c(0,0,0,0), mar=c(2,2,2,2))
  fmsb::radarchart(DF3, axistype=1, centerzero=TRUE, caxislabels=0:5, seg=5,
                   pcol=rgb(0.2,0.5,0.5,0.9), pfcol=rgb(0.2,0.5,0.5,0.5),
                   plwd=2,cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8,
                   vlcex=0.7, title=title)

}

output_dim_scores <- function(FPI.Summary) {
  # this isn't very robust!!

  first <- which(FPI.Summary[,1] == "INDICATOR")[1] +1
  last <- which(FPI.Summary[,2]=='Processing Workers')

  DF <- FPI.Summary[first:last, 1:3]
  colnames(DF) <- c('a', 'b', 'c')
  radar_plot(DF, 'Output Dimension Scores')
}

input_dim_scores <- function(FPI.Summary) {
  first <- which(FPI.Summary[,1] == "COMPONENT")[1] +1
  last <- which(FPI.Summary[,2]=='Infrastructure')

  DF <- FPI.Summary[first:last, 1:3]
  colnames(DF) <- c('a', 'b', 'c')
  radar_plot(DF, 'Input Dimension Scores')
}

output_scores_TBL <- function(FPI.Summary) {
  first <- which(FPI.Summary[,1] == "INDICATOR")[2] +1
  last <- which(FPI.Summary[,2]=='Career')

  DF <- FPI.Summary[first:last, 1:3]
  colnames(DF) <- c('a', 'b', 'c')
  radar_plot(DF, 'Output Scores by TBL')
}

FSHEP <- function(Output.table) {

  first <- which(Output.table[,4] == "Percentage of Stocks Overfished")
  last <- which(Output.table[,4]=='Proportion of Harvest with a 3rd Party Certification')
  DF <- Output.table[first:last, c(3,4,7)]
  colnames(DF) <- c('a', 'b', 'c')
  radar_plot(DF, 'Fish Stock Health & Environmental Performance')
}

harvest <- function(Output.table) {
  first <- which(Output.table[,4] == "Landings Level")
  last <- which(Output.table[,4]=='Season Length')
  extra <- which(Output.table[,4]=='Ex-vessel Price Compared to Historic High')
  DF <- Output.table[c(first:last, extra), c(3,4,7)]
  colnames(DF) <- c('a', 'b', 'c')
  radar_plot(DF, 'Harvest')
}

harvest_assets <- function(Output.table) {
  first <- which(Output.table[,4] == "Ratio of Asset Value to Gross Earnings")
  last <- which(Output.table[,4]=='Functionality of Harvest Capital')
  DF <- Output.table[first:last, c(3,4,7)]
  colnames(DF) <- c('a', 'b', 'c')
  radar_plot(DF, 'Harvest Assets')
}

risk <- function(Output.table) {
  first <- which(Output.table[,4] == "Annual Total Revenue Volatility")
  last <- which(Output.table[,4]=='Spatial Price Volatility')
  DF <- Output.table[first:last, c(3,4,7)]
  colnames(DF) <- c('a', 'b', 'c')
  radar_plot(DF, 'Risk')
}

managerial_returns <- function(Output.table) {
  one <- which(Output.table[,4] == "Earnings Compared to Regional Average Earnings")[1]
  two <- which(Output.table[,4]=='Owner/Permit Holder/Captain Wages Compared to Non-fishery Wages')
  three <- which(Output.table[,4]=='Social Standing of Boat Owners and Permit Holders')
  four <- which(Output.table[,4]=='Earnings Compared to Regional Average Earnings')[2]
  five <- which(Output.table[,4]=='Manager Wages Compared to Non-fishery Wages')
  six <- which(Output.table[,4]=='Social Standing of Processing Managers')
  DF <- Output.table[c(one, two, three, four, five, six), c(3,4,7)]
  DF[1,2] <- paste(DF[1,2], '(owner/captain)')
  colnames(DF) <- c('a', 'b', 'c')
  radar_plot(DF, 'Managerial Returns', 40)

}
# ---- Test ----
FPIfile <- 'G:/Shared drives/BM shared/1. Projects/FPAT/FPAT examples/FPAT vBeta-Dive-based fishery CR.xlsx'

# Summary Tab
FPI.Summary <- readxl::read_excel(FPIfile, sheet='4. Summary', .name_repair = 'minimal')

output_dim_scores(FPI.Summary)
input_dim_scores(FPI.Summary)
output_scores_TBL(FPI.Summary)

# Output-graph by TLB Tab
Output.table <- readxl::read_excel(FPIfile, sheet='5. Output-table', .name_repair = 'minimal')

FSHEP(Output.table)
harvest(Output.table)
harvest_assets(Output.table)
risk(Output.table)
managerial_returns(Output.table)
