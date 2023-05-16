# Historical fishery dynamics plots


hist_spawnbio <- function(Info) {
  if (class(Info$MSEhist) == 'Hist') {

    MSEhist <- Info$MSEhist

    SB <- apply(MSEhist@TSdata$SBiomass,1:2,sum)
    nsim <- dim(SB)[1]
    nyears <- dim(SB)[2]
    SB <- SB/MSEhist@Ref$ByYear$SSB0[, 1:nyears]

    Current_Year <- max(c(Info$Data@LHYear, Info$Data@Year), na.rm=TRUE)

    yrs<-Current_Year-(nyears:1)

    df <- data.frame(Year=rep(yrs, each=nsim), Sim=1:nsim, SB=as.vector(SB))

    ggplot(df, aes(x=Year, y=SB)) +
      geom_smooth(stat = 'summary', alpha = 0.2, fill = 'blue', color = 'blue',
                  fun.data = median_hilow, fun.args = list(conf.int = 0.5)) +
      expand_limits(y=c(0,1)) +
      labs(x="Year", y='Relative Spawning Biomass') +
      theme_bw() +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=16))
  }
}

hist_catch <- function(Info) {
  if (class(Info$MSEhist) == 'Hist') {
    MSEhist <- Info$MSEhist
    Catch <- apply(MSEhist@TSdata$Removals,1:2,sum)
    nsim <- dim(Catch)[1]
    nyears <- dim(Catch)[2]
    Current_Year <- max(c(Info$Data@LHYear, Info$Data@Year), na.rm=TRUE)
    yrs<-Current_Year-(nyears:1)
    Catch <- Catch/Catch[,nyears]

    df <- data.frame(Year=rep(yrs, each=nsim), Sim=1:nsim, Catch=as.vector(Catch))

    ggplot(df, aes(x=Year, y=Catch)) +
      geom_smooth(stat = 'summary', alpha = 0.2, fill = 'blue', color = 'blue',
                  fun.data = median_hilow, fun.args = list(conf.int = 0.5)) +
      expand_limits(y=c(0,1)) +
      labs(x="Year", y='Relative Historical Catch') +
      theme_bw() +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=16))
  }
}

hist_recruitment <- function(Info) {
  if (class(Info$MSEhist) == 'Hist') {
    MSEhist <- Info$MSEhist
    Rec <- apply(MSEhist@AtAge$Number[,1,,],1:2,sum)
    nsim <- dim(Rec)[1]
    nyears <- dim(Rec)[2]
    Current_Year <- max(c(Info$Data@LHYear, Info$Data@Year), na.rm=TRUE)
    yrs<-Current_Year-(nyears:1)
    Rec <- Rec/MSEhist@SampPars$Stock$R0

    df <- data.frame(Year=rep(yrs, each=nsim), Sim=1:nsim, Rec=as.vector(Rec))

    ggplot(df, aes(x=Year, y=Rec)) +
      geom_smooth(stat = 'summary', alpha = 0.2, fill = 'blue', color = 'blue',
                  fun.data = median_hilow, fun.args = list(conf.int = 0.5)) +
      expand_limits(y=c(0,1)) +
      labs(x="Year", y='Relative Historical Recruitment') +
      theme_bw() +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=16))
  }
}


plotquant<-function(x,p=c(0.05,0.25,0.75,0.95), yrs, cols=list(colm="dark blue", col50='light blue', col90='#60859925'), addline=T, ablines=NA){
  ny<-length(yrs)
  x[x==Inf]<-NA
  qs<-apply(x,2,quantile,p=p[c(1,4)],na.rm=T,type=3)
  qsi<-apply(x,2,quantile,p=p[2:3],na.rm=T,type=3)
  polygon(c(yrs,yrs[ny:1]),c(qs[1,],qs[2,ny:1]),border=NA,col=cols$col90)

  polygon(c(yrs,yrs[ny:1]),c(qsi[1,],qsi[2,ny:1]),border=NA,col=cols$col50)
  if(!is.na(ablines[1]))abline(h=ablines,col='#99999980')

  if(addline)for(i in 1:2)lines(yrs,x[i,],col='black',lty=i)
  lines(yrs,apply(x,2,quantile,p=0.5,na.rm=T),lwd=2,col=cols$colm)
}

tsplot<-function(x,xlab="",ylab="",zeroyint=T,cols=list(colm="dark blue", col50='light blue', col90='#60859925')){
  yrs<-CurrentYr-((dim(x)[2]):1)
  ymin=0
  if(!zeroyint)ymin<-quantile(x,0.01)
  plot(range(yrs),c(ymin,quantile(x,0.99)),col="white",xlab=xlab,ylab=ylab,yaxs='i')
  abline(h=pretty(seq(from=ymin,to=max(x)*1.25,length.out=20)),col="light grey")
  plotquant(x,yrs=yrs,cols=cols)

}

hist_bio<-function(Info){

  MSEhist<-Info$MSEhist
  par(mfcol=c(2,3),mai=c(0.3,0.6,0.2,0.1),omi=c(0.6,0,0,0))
  tsplot(x=apply(MSEhist@TSdata$SBiomass,1:2,sum),xlab="Historical Year",ylab="Spawning biomass")
  tsplot(apply(MSEhist@TSdata$Biomass,1:2,sum),xlab="Historical Year",ylab="Biomass")
  tsplot(apply(MSEhist@TSdata$Number,1:2,sum),xlab="Historical Year",ylab="Numbers")
  tsplot(apply(MSEhist@TSdata$VBiomass,1:2,sum),xlab="Historical Year",ylab="Vulnerable Biomass")
  tsplot(x=log(MSEhist@TSdata$RecDev[,1:(MSEhist@OM@nyears+MSEhist@OM@maxage-1)]),xlab="Historical Year",ylab="Recruitment strength",zeroyint=F)
  tsplot(apply(MSEhist@AtAge$Number[,1,,],1:2,sum),xlab="Historical Year",ylab="Recruitment")

}

hist_exp<-function(Info){
  MSEhist<-Info$MSEhist
  par(mfcol=c(2,3),mai=c(0.3,0.6,0.2,0.1),omi=c(0.6,0,0,0))
  cols=list(colm="darkgreen",col50='lightgreen',col90='#40804025')
  tsplot(apply(MSEhist@TSdata$Landings,1:2,sum),xlab="Historical Year",ylab="Landings",cols=cols)
  tsplot(apply(MSEhist@TSdata$Discards,1:2,sum),xlab="Historical Year",ylab="Discards",cols=cols)

}


