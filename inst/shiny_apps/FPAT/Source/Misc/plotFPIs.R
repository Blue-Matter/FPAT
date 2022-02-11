
chk <- suppressWarnings(require(fmsb, quietly = TRUE))
if (!chk) {
  install.packages('fmsb')
  library(fmsb)
}
library(dplyr)

chk <- suppressWarnings(require(RColorBrewer, quietly = TRUE))
if (!chk) {
  install.packages('RColorBrewer')
  library(RColorBrewer)
}


add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2,
        function(x)
          rgb(x[1], x[2], x[3], alpha=alpha))
}

# ---- Functions ----

radar_plot <- function(DF, title, n=25, vlcex=1, BaseLineDat=NULL, FPI_2DF=NULL) {
  for (i in 2:nrow(DF)) {
    if (is.na(DF[i,1]))
      DF[i,1] <- DF[i-1,1]
  }

  DF$c <- as.numeric(DF$c)
  # insert new line after n characters
  DF$b <- gsub(paste0('(.{1,',n,'})(\\s|$)'), '\\1\n', DF$b)
  DF2 <- tidyr::pivot_wider(DF[,2:3], names_from = 1, values_from=2)

  max <- DF2
  max[max!=5] <- 5
  min <- DF2
  min[min!=0] <- 0
  DF3 <- rbind(max, min, DF2)
  cols <- rev(2:ncol(DF3))
  DF3 <- DF3[, c(1, cols)]
  DF3 <- as.data.frame(DF3)
  rownames(DF3) <- c('Min', 'Max', 'Fishery')
  # add baseline
  if (!is.null(BaseLineDat)) {
    for (i in 1:ncol(BaseLineDat)) {
      bldf <- DF3[3,]
      bldf[1:ncol(bldf)] <- t(BaseLineDat[,i])
      rownames(bldf) <- colnames(BaseLineDat[,i] )
      DF3 <- rbind(DF3, bldf)
    }
  }

  # add comparison FPI
  if (!is.null(FPI_2DF)) {
    for (i in 2:nrow(FPI_2DF)) {
      if (is.na(FPI_2DF[i,1]))
        FPI_2DF[i,1] <- FPI_2DF[i-1,1]
    }
    FPI_2DF <- t(FPI_2DF)
    cols <- rev(2:ncol(FPI_2DF))
    FPI_2DF <- FPI_2DF[,c(1, cols)]
    DF3 <- rbind(DF3, as.numeric(FPI_2DF[3,]))
    rownames(DF3)[length(rownames(DF3))] <-  'Comparison FPI'

  }

  # colors
  cols <- RColorBrewer::brewer.pal(n=max(3,nrow(DF3)-2), 'Set1')

  pcol <- cols
  pfcol <- cols
  pfcol[1] <- add.alpha(cols[1], 0.5)
  pfcol[2:length(pfcol)] <- add.alpha(cols[2:length(pfcol)], 0.3)

  par(mfrow=c(1,1), oma=c(0,0,0,0), mar=c(2,2,2,2), xpd=TRUE)
  fmsb::radarchart(DF3, axistype=1, centerzero=TRUE, caxislabels=0:5, seg=5,
                   pcol=pcol, pfcol=pfcol,
                   plwd=2,cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8,
                   title=title, vlcex=vlcex)

  if (!is.null(BaseLineDat) | !is.null(FPI_2DF)) {
    legend(
      x = "bottom", legend = rownames(DF3[-c(1,2),]), horiz = TRUE,
      bty = "n", pch = 20 , col =pcol,
      text.col = "black", cex = 1.25, pt.cex = 2,
      inset=c(0,-0.05), xpd=NA
    )
  }

}

output_dim_scores <- function(FPI.Summary, baseline, BaseLine, FPI_2=NULL, n=25, vlcex=1) {

  # baseline
  BaseLineDat <- NULL
  if (!is.null(baseline)) {
    cols <- which(BaseLine[1,2:ncol(BaseLine)] %in% baseline) + 1
    rows <- 39:49
    BaseLineDat <- BaseLine[rows,cols]
    BaseLineDat <- BaseLineDat %>% mutate_all(., as.numeric)
    colnames(BaseLineDat) <- baseline
  }

  # FPI scores
  first <- which(FPI.Summary[,1] == "INDICATOR")[1] +1
  last <- which(FPI.Summary[,2]=='Processing Workers')
  DF <- FPI.Summary[first:last, 1:3]
  colnames(DF) <- c('a', 'b', 'c')

  FPI_2DF <- NULL
  if (!is.null(FPI_2)) {
    first <- which(FPI_2[,1] == "INDICATOR")[1] +1
    last <- which(FPI_2[,2]=='Processing Workers')
    FPI_2DF <- FPI_2[first:last, 1:3]
    colnames(FPI_2DF) <- c('a', 'b', 'c')
  }

  radar_plot(DF, title=NULL, n=n, vlcex=vlcex, BaseLineDat, FPI_2DF)
}


input_dim_scores <- function(FPI.Summary, baseline, BaseLine, FPI_2=NULL, n=25, vlcex=1) {
  # baseline
  BaseLineDat <- NULL
  if (!is.null(baseline)) {
    cols <- which(BaseLine[1,2:ncol(BaseLine)] %in% baseline) + 1
    rows <- 19:33
    BaseLineDat <- BaseLine[rows,cols]
    BaseLineDat <- BaseLineDat %>% mutate_all(., as.numeric)
    colnames(BaseLineDat) <- baseline
  }

  first <- which(FPI.Summary[,1] == "COMPONENT")[1] +1
  last <- which(FPI.Summary[,2]=='Infrastructure')
  DF <- FPI.Summary[first:last, 1:3]
  colnames(DF) <- c('a', 'b', 'c')

  FPI_2DF <- NULL
  if (!is.null(FPI_2)) {
    first <- which(FPI_2[,1] == "COMPONENT")[1] +1
    last <- which(FPI_2[,2]=='Infrastructure')
    FPI_2DF <- FPI_2[first:last, 1:3]
    colnames(FPI_2DF) <- c('a', 'b', 'c')
  }
  radar_plot(DF, title=NULL, n=n, vlcex=vlcex, BaseLineDat, FPI_2DF)
}

output_scores_TBL <- function(FPI.Summary, baseline, BaseLine, FPI_2=NULL, n=25, vlcex=1) {
  # baseline
  BaseLineDat <- NULL
  if (!is.null(baseline)) {
    cols <- which(BaseLine[1,2:ncol(BaseLine)] %in% baseline) + 1
    rows <- 2:15
    BaseLineDat <- BaseLine[rows,cols]
    BaseLineDat <- BaseLineDat %>% mutate_all(., as.numeric)
    colnames(BaseLineDat) <- baseline
  }

  first <- which(FPI.Summary[,1] == "INDICATOR")[2] +1
  last <- which(FPI.Summary[,2]=='Career')
  DF <- FPI.Summary[first:last, 1:3]
  colnames(DF) <- c('a', 'b', 'c')

  FPI_2DF <- NULL
  if (!is.null(FPI_2)) {
    first <- which(FPI_2[,1] == "INDICATOR")[2] +1
    last <- which(FPI_2[,2]=='Career')
    FPI_2DF <- FPI_2[first:last, 1:3]
    colnames(FPI_2DF) <- c('a', 'b', 'c')
  }

  radar_plot(DF, title=NULL, n=n, vlcex=vlcex, BaseLineDat, FPI_2DF)
}



FSHEP <- function(Output.table, n=25, vlcex=1) {
  first <- which(Output.table[,4] == "Percentage of Stocks Overfished")
  last <- which(Output.table[,4]=='Proportion of Harvest with a 3rd Party Certification')
  DF <- Output.table[first:last, c(3,4,7)]
  colnames(DF) <- c('a', 'b', 'c')
  radar_plot(DF, 'Fish Stock Health & Environmental Performance', n=n, vlcex=vlcex)
}

harvest <- function(Output.table, n=25, vlcex=1) {
  first <- which(Output.table[,4] == "Landings Level")
  last <- which(Output.table[,4]=='Season Length')
  extra <- which(Output.table[,4]=='Ex-vessel Price Compared to Historic High')
  DF <- Output.table[c(first:last, extra), c(3,4,7)]
  colnames(DF) <- c('a', 'b', 'c')

  metrics <- c("Landings Level",
               'Excess Capacity',
               'Season Length',
               'Ex-vessel Price Compared to Historic High')
  string <- Output.table[,4] %>% as.matrix() %>% as.vector()
  ind <- which(string %in% metrics)
  DF <- Output.table[ind, c(3,4,7)]
  DF <- DF[match(metrics, string[ind]),]
  colnames(DF) <- c('a', 'b', 'c')
  radar_plot(DF, 'Harvest', n=n, vlcex=vlcex)
}

harvest_assets <- function(Output.table, n=25, vlcex=1) {
  metrics <- c("Ratio of Asset Value to Gross Earnings",
               'Total Revenue Compared to Historic High',
               'Asset (Permit, Quota, etc...) Value Compared to Historic High',
               'Borrowing Rate Compared to Risk-free Rate',
               'Source of Capital',
               'Functionality of Harvest Capital')
  string <- Output.table[,4] %>% as.matrix() %>% as.vector()
  ind <- which(string %in% metrics)
  DF <- Output.table[ind, c(3,4,7)]
  DF <- DF[match(metrics, string[ind]),]
  colnames(DF) <- c('a', 'b', 'c')
  radar_plot(DF, 'Harvest Assets', n=n, vlcex=vlcex)
}

risk <- function(Output.table, n=25, vlcex=1) {
  metrics <- c("Annual Total Revenue Volatility",
               'Annual Landings Volatility',
               'Intra-annual Landings Volatility',
               'Annual Price Volatility',
               'Intra-annual Price Volatility',
               'Spatial Price Volatility')
  string <- Output.table[,4] %>% as.matrix() %>% as.vector()
  ind <- which(string %in% metrics)
  DF <- Output.table[ind, c(3,4,7)]
  DF <- DF[match(metrics, string[ind]),]
  colnames(DF) <- c('a', 'b', 'c')
  radar_plot(DF, 'Risk', vlcex=vlcex, n=n)
}

managerial_returns <- function(Output.table, n=40, vlcex=1) {
  # duplicate metrics
  one <- which(Output.table[,4] == "Earnings Compared to Regional Average Earnings")[1]
  two <- which(Output.table[,4]=='Owner/Permit Holder/Captain Wages Compared to Non-fishery Wages')
  three <- which(Output.table[,4]=='Social Standing of Boat Owners and Permit Holders')
  four <- which(Output.table[,4]=='Earnings Compared to Regional Average Earnings')[2]
  five <- which(Output.table[,4]=='Manager Wages Compared to Non-fishery Wages')
  six <- which(Output.table[,4]=='Social Standing of Processing Managers')
  DF <- Output.table[c(one, two, three, four, five, six), c(3,4,7)]
  DF[1,2] <- paste(DF[1,2], '(owner/captain)')
  colnames(DF) <- c('a', 'b', 'c')
  radar_plot(DF, 'Managerial Returns', n=n, vlcex=vlcex)

}

trade <- function(Output.table, n=25, vlcex=1) {
  metrics <- c('International Trade',
               'Final Market Wealth',
               'Wholesale Price Compared to Similar Products',
               'Capacity of Firms to Export to the US & EU')
  string <- Output.table[,4] %>% as.matrix() %>% as.vector()
  ind <- which(string %in% metrics)
  DF <- Output.table[ind, c(3,4,7)]
  DF <- DF[match(metrics, string[ind]),]
  colnames(DF) <- c('a', 'b', 'c')
  radar_plot(DF, 'Trade', n=n, vlcex=vlcex)
}

product_form <- function(Output.table, n=25, vlcex=1) {
  metrics <- c('Processing Yield',
               'Shrink',
               'Capacity Utilization Rate',
               'Product Improvement',
               'Final Market Use',
               'Ex-vessel to Wholesale Marketing Margins')
  string <- Output.table[,4] %>% as.matrix() %>% as.vector()
  ind <- which(string %in% metrics)
  DF <- Output.table[ind, c(3,4,7)]
  DF <- DF[match(metrics, string[ind]),]
  colnames(DF) <- c('a', 'b', 'c')
  radar_plot(DF, 'Product Form', n=n, vlcex=vlcex)
}

post_harvest_perf <- function(Output.table, n=25, vlcex=1) {
  metrics <- c('Borrowing Rate Compared to Risk-free Rate',
               'Source of Capital',
               'Age of Facilities')
  string <- Output.table[,4] %>% as.matrix() %>% as.vector()
  ind <- which(string %in% metrics)
  DF <- Output.table[ind, c(3,4,7)]
  DF <- DF[match(metrics, string[ind]),]
  colnames(DF) <- c('a', 'b', 'c')
  radar_plot(DF, 'Post-Harvest Asset Performance', n=n, vlcex=vlcex)
}

labor_returns <- function(Output.table, n=30, vlcex=1) {
  # duplicate metrics
  metrics <- c('Crew Wages Compared to Non-fishery Wages',
               'Social Standing of Crew',
               'Worker Wages Compared to Non-fishery Wages',
               'Social Standing of Processing Workers')

  ind2 <- which(Output.table[,4] == 'Earnings Compared to Regional Average Earnings')

  string <- Output.table[,4] %>% as.matrix() %>% as.vector()
  ind <- which(string %in% metrics)
  ind <- ind[match(metrics, string[ind])]
  ind3 <- rep(NA, length(6))
  ind3[c(2,3,5,6)] <- ind
  ind3[1] <- ind2[2]
  ind3[4] <- ind2[4]

  DF <- Output.table[ind3, c(3,4,7)]
  colnames(DF) <- c('a', 'b', 'c')
  DF$b[1] <- paste(DF$b[1], "(Crew)")
  radar_plot(DF, 'Labor Returns', n=n, vlcex=vlcex)
}

health_sanit <- function(Output.table, n=25, vlcex=1) {
  # duplicate metrics
  metrics <- c('Harvest Safety',
               'Sanitation')

  ind2 <- which(Output.table[,4] == 'Access to Health Care')
  string <- Output.table[,4] %>% as.matrix() %>% as.vector()
  ind <- which(string %in% metrics)

  ind3 <- rep(NA, length(6))
  ind3[c(1,6)] <- ind
  ind3[2:5] <- ind2[1:4]

  DF <- Output.table[ind3, c(3,4,7)]
  colnames(DF) <- c('a', 'b', 'c')
  DF$b[2:5] <- paste(DF$b[2:5], c("(Owners)", "(Crew)", "(Processing Owners)", "(Processing Workers)"))
  radar_plot(DF, 'Health & Sanitation', n=n, vlcex=vlcex)
}


# UNCOMMENT AND RUN TO TEST PLOTS
# # ---- Demo ----
# FPIfile <- 'G:/Shared drives/BM shared/1. Projects/FPAT/FPAT examples/FPAT vBeta-Dive-based fishery CR.xlsx'
#
# # --- Summary Tab ---
# FPI.Summary <- readxl::read_excel(FPIfile, sheet='4. Summary', .name_repair = 'minimal')
#
# output_dim_scores(FPI.Summary)
# input_dim_scores(FPI.Summary)
# output_scores_TBL(FPI.Summary)
#
# # --- Output-graph by TLB Tab ---
# Output.table <- readxl::read_excel(FPIfile, sheet='5. Output-table', .name_repair = 'minimal')
#
# # Ecology
# FSHEP(Output.table)
#
# # Economics
# harvest(Output.table)
# harvest_assets(Output.table)
# risk(Output.table)
# trade(Output.table)
# product_form(Output.table)
# post_harvest_perf(Output.table)
#
# # Community
# managerial_returns(Output.table)
# labor_returns(Output.table)
# health_sanit(Output.table)


# NOT DONE BELOW HERE

# community_service(Output.table)
# local_owner(Output.table)
# local_labor(Output.table)
# career(Output.table)

# --- Output-graph by Sector Tab ---
#
# # Stock Performance
# FSHEP(Output.table)
#
# # Harvest Sector Performance
# harvest_performance(Output.table)
# harvest_assets(Output.table)
# risk(Output.table)
# owners_captains(Output.table)
# crew(Output.table)
# market_perf(Output.table)
# PHPSPP(Output.table)
# post_harvest_assets(Output.table)
# managers_owners(Output.table)
# workers(Output.table)


