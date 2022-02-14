
runProj<-function(Info){
  withProgress(message="Running test of management options",value=0, {
    Info$MSEproj<-Project(Info$MSEhist,MPs=Info$MPsel, extended = T)
    # out <<- Info$MSEproj
  })
}


# --- Make Results DF ----
MakeDF <- function(MSE) {
  nMPs <- MSE@nMPs
  nsim <- MSE@nsim
  nyears <- MSE@nyears
  proyears <- MSE@proyears
  Years <- seq(Current_Year+1, Current_Year+proyears)

  SB0 <- MSE@RefPoint$ByYear$SSB0[,(nyears+1):(nyears+proyears)]
  SB0 <- replicate(nMPs, SB0) %>% aperm(c(1,3,2))

  SBMSY <- MSE@RefPoint$ByYear$SSBMSY[,(nyears+1):(nyears+proyears)]
  SBMSY <- replicate(nMPs, SBMSY) %>% aperm(c(1,3,2))

  HistCatch <- apply(MSE@Hist@TSdata$Landings, 1:2, sum)
  LastCatch <- replicate(nMPs,HistCatch[,nyears])
  LastCatch <- replicate(proyears, LastCatch)

  HistRemovals <- apply(MSE@Hist@TSdata$Removals, 1:2, sum)
  LastRemovals <- replicate(nMPs,HistRemovals[,nyears])
  LastRemovals <- replicate(proyears, LastRemovals)

  df <- data.frame(Sim=1:nsim,
                   MP=rep(MSE@MPs, each=nsim),
                   Year=rep(Years,each=nMPs*nsim),
                   SB=as.vector(MSE@SSB),
                   SB0=as.vector(SB0),
                   SBMSY=as.vector(SBMSY),
                   Catch=as.vector(MSE@Catch),
                   LastCatch=as.vector(LastCatch),
                   Removals=as.vector(MSE@Removals),
                   LastRemovals=as.vector(LastRemovals))
  df <- df %>%
    mutate(SB_SB0=SB/SB0,
           SB_SBMSY=SB/SBMSY,
           C_Cnow=Catch/LastCatch,
           R_Rnow=Removals/LastRemovals)

  # rename MPs
  df$MP[df$MP=='CurC'] <- 'Current Catch'
  df$MP[df$MP=='curEref'] <- 'Current Effort'
  df$MP[df$MP=='matlenlim'] <- 'Size Limit 1'
  df$MP[df$MP=='matlenlim2'] <- 'Size Limit 2'
  df$MP[df$MP=='Ltarget1'] <- 'Length Targeting 1'
  df$MP[df$MP=='Ltarget2'] <- 'Length Targeting 2'
  df$MP[df$MP=='Itarget1'] <- 'Index Targeting 1'
  df$MP[df$MP=='Itarget2'] <- 'Index Targeting 2'

  df
}

# --- Projection Plot ----
Projection_plot <- function(MSE, Var=NULL, opt=NULL) {

  MSE <<- MSE
  Var <<- Var
  opt <<- opt

  if (class(MSE) == 'MSE' & !is.null(Var) & !is.null(opt)) {
    df <- MakeDF(MSE)

    if (Var =='Spawning Biomass') {
      if (opt=='SB0') {
        var <- 'SB_SB0'
        ylab <- 'Spawning Biomass relative to Average Unfished (SB0)'
      }
      if (opt=='SBMSY') {
        var <- 'SB_SBMSY'
        ylab <- 'Spawning Biomass relative to SBMSY'
      }
      DF <- df %>% select(Sim, MP, Year, all_of(var)) %>%
        tidyr::pivot_longer(cols=4)
    }

    if (Var =='Catch') {
      if (opt) {
        var <- c('C_Cnow', 'R_Rnow')
        cols <- 4:5
        ylab <- 'Relative Catch & Removals'
      }
      if (!opt) {
        var <- 'C_Cnow'
        cols <- 4
        ylab <- 'Relative Catch'
      }

      DF <- df %>% select(Sim, MP, Year, all_of(var)) %>%
        tidyr::pivot_longer(cols=all_of(cols))
    }

    ggplot(DF, aes(x=Year, y=value, color=name, fill=name)) +
      facet_wrap(~MP, ncol=3) +
      geom_hline(yintercept = c(0.5, 1), linetype=2, color='gray') +
      geom_smooth(stat = 'summary', alpha = 0.2,
                  fun.data = median_hilow, fun.args = list(conf.int = 0.5)) +
      expand_limits(y=c(0,1)) +
      theme_classic() +
      scale_color_manual(values=c('blue', 'darkblue')) +
      scale_fill_manual(values=c('blue', 'darkblue')) +
      guides(color='none', fill='none') +
      labs(x='Projection Year', y=ylab, color='', fill='') +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=16),
            strip.text = element_text(size=12))

  }
}


# --- Trade-Off Plot ----

subsetdf <- function(df, axis) {
  quants <- c(0.25, 0.75)
  if (length(axis$Var)>0) {
    if (axis$Var == 'SB/SB0') Var <- 'SB_SB0'
    if (axis$Var == 'SB/SBMSY') Var <- 'SB_SBMSY'
    if (axis$Var == 'Catch') Var <- 'C_Cnow'

    DF <- df %>% filter(Year %in% axis$Year) %>%
      group_by(MP) %>%
      select(MP, Val=all_of(Var))

    DF <- DF %>% group_by(MP) %>%
      dplyr::summarize(Median=median(Val),
                Low=quantile(Val, quants[1]),
                Hi=quantile(Val, quants[2]),
                Prob=mean(Val>axis$Reference))

    DF$Metric <- axis$Metric
    if (axis$Metric=='Median') {
      DF$val <- DF$Median
      if (grepl('SB',axis$Var)) DF$lab <- 'Median Relative Spawning Biomass'
      if (grepl('Catch',axis$Var)) DF$lab <- 'Median Relative Catch'
      DF$error_bars <- TRUE
    }
    if (axis$Metric=='Probability') {
      DF$val <- DF$Prob
      if (grepl('SB',axis$Var)) DF$lab <- paste0('Prob. Relative Spawning Biomass > ', axis$Reference)
      if (grepl('Catch',axis$Var)) DF$lab <- paste0('Prob. Relative Catch > ', axis$Reference)
      DF$error_bars <- FALSE
    }
    DF
  }

}


TradeOff_plot <- function(MSE, Xaxis=NULL, Yaxis=NULL, incEx=TRUE, incEy=TRUE) {

  MSE <<- MSE
  Xaxis <<- Xaxis
  Yaxis <<- Yaxis
  incEx <<- incEx
  incEy <<- incEy

  if (class(MSE) == 'MSE' & !is.null(Xaxis) & !is.null(Yaxis)) {
    df <- MakeDF(MSE)

    if (length(Xaxis$Var)>0 & length(Yaxis$Var)>0) {
      # X-axis
      if (Xaxis$Var %in% c('AAVY', 'AAVE')) {
        Xax <- calcVar(MSE, Xaxis)
      } else {
        Xax <- subsetdf(df, Xaxis)
      }


      # Yaxis
      if (Yaxis$Var %in% c('AAVY', 'AAVE')) {
        Yax <- calcVar(MSE, Yaxis)
      } else {
        Yax <- subsetdf(df, Yaxis)
      }


      # make dataframe
      DF <- data.frame(MP=Xax$MP,
                       x=Xax$val, y=Yax$val,
                       xlow=Xax$Low, xhi=Xax$Hi,
                       ylow=Yax$Low, yhi=Yax$Hi,
                       xlab=Xax$lab[1],
                       ylab=Yax$lab[1],
                       xerror=Xax$error_bars[1],
                       yerror=Yax$error_bars[1])

      if (nrow(DF)>0) {
        if (!incEx) DF$xerror <- FALSE
        if (!incEy) DF$yerror <- FALSE

        p <- ggplot(DF, aes(x=x, y=y, color=MP)) +
          geom_point() +
          ggrepel::geom_text_repel(aes(label=MP),size=6)

        # add error_bars
        if (DF$yerror[1])
          p <- p + ggplot2::geom_errorbar(aes(ymin=ylow, ymax=yhi))
        if (DF$xerror[1])
          p <- p + ggplot2::geom_errorbarh(aes(xmin=xlow, xmax=xhi))

        p <-  p +
          theme_bw() +
          expand_limits(x=c(0,1), y=c(0,1)) +
          guides(color='none') +
          labs(x=DF$xlab[1],
               y=DF$ylab[1])

        p + theme(axis.text=element_text(size=12),
                  axis.title=element_text(size=16))
      }
    }


  }
}

makeVlab <- function(axis) {
  if (axis$Var=='AAVY') {
    if (axis$Metric=='Median') {
      lab <- 'Median Inter-Annual Variability in Catch'
    } else {
      lab <- paste0('Probability Inter-Annual Variability in Catch <', axis$Reference*100, '%')
    }

  }
  if (axis$Var=='AAVE') {
    if (axis$Metric=='Median') {
      lab <- 'Median Inter-Annual Variability in Effort'
    } else {
      lab <- paste0('Probability Inter-Annual Variability in Effort <', axis$Reference*100, '%')
    }
  }
  lab
}


calcVar <- function(MSE, axis) {
  Yrs <- seq(Current_Year+1, by=1, length.out=MSE@proyears)
  pm <- get(axis$Var)(MSE, Yrs= match(axis$Year, Yrs))
  median <- apply(pm@Stat, 2, median)
  low <- apply(pm@Stat, 2, quantile, 0.25)
  hi  <- apply(pm@Stat, 2, quantile, 0.75)
  Prob <- apply(pm@Stat<axis$Reference, 2, mean)
  metric <- axis$Metric

  if (axis$Metric=='Median') {
    val <- median
    error_bars <- TRUE
    lab <- makeVlab(axis)
  }
  if (axis$Metric=='Probability') {
    val <- Prob
    error_bars <- FALSE
    lab <- makeVlab(axis)
  }

  data.frame(MP=MSE@MPs,
             Median=median,
             Low=low,
             Hi=hi,
             Prob=Prob,
             Metric=metric,
             val=val,
             lab=lab,
             error_bars=error_bars)

}


