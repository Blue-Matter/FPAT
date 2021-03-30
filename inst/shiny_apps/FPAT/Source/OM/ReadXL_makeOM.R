
FPIfile <- 'G:/Shared drives/BM shared/1. Projects/FPAT/FPAT examples/FPAT vBeta-Dive-based fishery CR.xlsx'
mera<-readRDS("G:/Shared drives/BM shared/1. Projects/MERA/MERA_TESTS_2021/TFH/Tiger_flathead.mera")


samp_par<-function(n,type="Truncated normal",LB,UB,trunc=90){


  if(LB==UB){
    out<-rep(LB,n)
  }else{
    if(type=="Uniform"){
      out<-runif(n,LB,UB)
    }else{
      mu<-mean(c(LB,UB))
      UBtemp<-qnorm(1-(100-trunc)/200,mu,1)
      sd<-(UB-mu)/(UBtemp-mu)
      out<-rnorm_T(n,mu,sd,trunc)
    }
  }
  out

}



rnorm_T<-function(n=1,mean=0,sd=1,trunc=90){

  incomplete=T
  ntrial<-n*10
  out<-rep(NA,n)
  fillind<-(1:n)[is.na(out)]
  nfill<-length(fillind)
  LB<-((100-trunc)/2)/100
  UB<-1-LB
  #i<-0

  while(incomplete){
    #i<-i+1
    #print(i)
    samps<-rnorm(ntrial,mean,sd)
    cond<-samps > qnorm(LB,mean,sd) & samps < qnorm(UB, mean, sd)
    canfill<-sum(cond)
    if(canfill >= nfill){
      out[fillind]<-samps[cond][1:nfill]
      incomplete=F
    }else{
      out[fillind[1:canfill]]<-samps[cond]
      fillind<-(1:n)[is.na(out)]
      nfill<-length(fillind)
    }

  }

  if(all(out > qnorm(LB,mean,sd) & out < qnorm(UB, mean, sd))){
    return(out)
  }else{
    message("An error in rnorm_T occurred")
    return(NULL)
  }

}


LowSlopes<-function(OMin, except = NULL) {

  nms <- slotNames(OMin)
  # exceptions
  if (is.null(except)) except <- "EVERYTHING"
  exclude <- unique(grep(paste(except, collapse = "|"), nms, value = FALSE))

  vars <- c("inc","sd")
  ind <- unique(grep(paste(vars, collapse = "|"), nms, value = FALSE))
  ind <- ind[(!(nms[ind] %in% exclude))]
  for (X in seq_along(ind)) {
    slot(OMin, nms[ind[X]]) <- c(0, 1e-10)
  }

  return(OMin)

}


importFPI <- function(FPIfile) {

  errlist<-list()

  sheets <- readxl::excel_sheets(FPIfile)
  if (!'12. Fishery Data' %in% sheets) errlist$Datasheet <- "FPI+ is missing '12. Fishery Data' sheet"
  if (!'13. MERA Questions' %in% sheets) errlist$MERAsheet <- "FPI+ is missing '13. MERA Questions' sheet"

  Data <- XL2Data(name=FPIfile, sheet='12. Fishery Data')
  MERA.Qs <- readxl::read_excel(FPIfile, sheet='13. MERA Questions', .name_repair = 'minimal')
  FPI.Inputs <- readxl::read_excel(FPIfile, sheet='6. Input-table', .name_repair = 'minimal')
  FPI.Cover <- readxl::read_excel(FPIfile, sheet='3. Cover Page', .name_repair = 'minimal')

  plusgroup<-40

  OM<-LowSlopes(MSEtool::testOM)
  OM@nsim<-24
  OM@interval<-4
  OM@proyears<-30
  OM@seed<-1

  OM@R0<-1e9
  OM@Linf<-c(100,100)
  OM@L50<-NaN
  OM@K<-NaN
  OM@isRel<-"FALSE"

  OM@Name<-Data@Common_Name
  OM@Species<-Data@Species
  OM@Region<-Data@Region
  OM@Agency<-""
  Nyears<-max(Data@Year)-min(Data@Year)+1
  OM@nyears<-Nyears

  authors <- FPI.Cover[18,2:ncol(FPI.Cover)]
  authors <- as.matrix(authors)
  authors <- authors[!is.na(authors)]
  authors <- paste(authors, collapse=", ")
  if (length(authors)<1) authors <- NA
  OM@Source<-authors

  ind <- which(FPI.Inputs[,3] == "Data Availability")
  DataQual <- FPI.Inputs[ind,6]
  if (DataQual < 3 ) temp<-new('OM',Albacore,Generic_Fleet,Imprecise_Biased,Perfect_Imp)
  if (DataQual == 3) MERA_Import$D4 <- temp<-new('OM',Albacore,Generic_Fleet,Generic_obs,Perfect_Imp)
  if (DataQual > 3) MERA_Import$D4 <- temp<-new('OM',Albacore,Generic_Fleet,Precise_Unbiased,Perfect_Imp)
  OM<-Replace(OM,temp,Sub="Obs")


  # ---- Fishery characteristics ---------------------------------------------------------------------------

  if(is.na(Data@Mort)){
    errlist$Mort<-"Natural mortality rate slot 'M' not specified in sheet '12. Fishery Data'"
  }else{
    if(is.na(Data@CV_Mort)){
      AM("Coefficient of variation in matural mortality rate slot 'CV M' not specified in sheet '12. Fishery Data' - assuming a CV value of 0.1")
    }else{
      OM@M<-rep(Data@Mort,2)
      OM@cpars<-list()
      OM@cpars$M <- trlnorm(nsim,Data@Mort,Data@CV_Mort)
    }
  }

  if(is.na(Data@L50)){
    errlist$maxage<-"Length at 50% maturity slot not specified in sheet '12. Fishery Data'"
  }else{
    if(is.na(Data@CV_L50)){
      AM("Coefficient of variation in length at 50% maturity slot not specified in sheet '12. Fishery Data' - assuming a CV value of 0.1")
      OM@cpars$L50 <- trlnorm(nsim,Data@L50,0.1)
    }else{
      OM@L50<-rep(Data@L50,2)
      OM@cpars$L50 <- trlnorm(nsim,Data@L50,Data@CV_L50)
    }
  }

  if(is.na(Data@vbLinf)){
    errlist$vbLinf<-"Asymptotic length 'Von Bertalantffy Linf parameter' not specified in sheet '12. Fishery Data'"
  }else{
    if(is.na(Data@CV_vbLinf)){
      AM("Coefficient of variation in asymptotic length 'CV von B. Linf parameter'not specified in sheet '12. Fishery Data' - assuming a CV value of 0.05")
      OM@cpars$Linf <- trlnorm(nsim,Data@vbLinf,0.05)
    }else{
      OM@Linf<-rep(Data@vbLinf,2)
      OM@cpars$Linf <- trlnorm(nsim,Data@vbLinf,Data@CV_vbLinf)
    }
  }

  if(is.na(Data@vbK)){
    errlist$vbK<-"Asymptotic length 'Von Bertalantffy K parameter' not specified in sheet '12. Fishery Data'"
  }else{
    if(is.na(Data@CV_vbK)){
      AM("Coefficient of variation in asymptotic length 'CV von B. K parameter' not specified in sheet '12. Fishery Data' - assuming a CV value of 0.1")
      OM@cpars$K <- trlnorm(nsim,Data@vbLinf,0.1)
    }else{
      OM@K<-rep(Data@vbK,2)
      OM@cpars$K <- trlnorm(nsim,Data@vbK,Data@CV_vbK)
    }
  }

  if(is.na(Data@vbt0)){
    AM("Theoretical age at length zero 'Von B. t0 parameter' not specified in sheet '12. Fishery Data' - assuming a t0 value of 0")
  }else{
    if(is.na(Data@CV_vbK)){
      AM("Coefficient of variation in theoretical age at length zero 'CV von B. t0 parameter' not specified in sheet '12. Fishery Data' - assuming a CV value of 0.1")
      OM@cpars$t0 <- trlnorm(nsim,Data@vbt0,0.1)
    }else{
      OM@t0<-rep(Data@vbt0,2)
      OM@cpars$t0 <- trlnorm(nsim,Data@vbt0,Data@CV_vbt0)
    }
  }

  if(is.na(Data@MaxAge)){
    errlist$maxage<-"Maximum age slot not specified in sheet '12. Fishery Data'"
  }else{
    OM@maxage<-Data@MaxAge
  }
  OM@maxage<-min(OM@maxage,plusgroup)

  if (is.na(Data@Dep)) {
    AM("Depletion slot 'Current stock depletion' not specified in sheet '12. Fishery Data', maximum uncertainty in stock depletion has been assumed (1% to 50% of unfished spawning stock biomass")
    OM@cpars$D<-runif(nsim,0.01,0.5)
  }else{
    if(is.na(Data@CV_Dep)){
      AM("Coefficient of variation in current spawning stock biomass relative to unfished 'CV current stock depletion' not specified in sheet '12. Fishery Data' - assuming a CV value of 0.25")
      OM@cpars$D <- trlnorm(nsim,Data@Dep,0.26)
    }else{
      OM@D<-rep(Data@Dep,2)
      OM@cpars$D <- trlnorm(nsim,Data@Dep,Data@CV_Dep)
    }
  }


  if (is.na(Data@steep)) {
    AM("Resilience slot 'Steepness' not specified in sheet '12. Fishery Data', values in the range 0.5-0.9 are assumed")
    OM@cpars$h<-runif(nsim,0.5,0.9)
  }else{
    if(is.na(Data@CV_steep)){
      AM("Coefficient of variation in resilience 'CV Steepness' not specified in sheet '12. Fishery Data' - assuming a CV value of 0.15")
      OM@cpars$h <- sample_steepness2(nsim,Data@steep,0.15)
    }else{
      OM@h<-rep(Data@steep,2)
      OM@cpars$h <- sample_steepness2(nsim,Data@steep,Data@CV_steep)
    }
  }


  # --- Life history imputation
  #OMtemp<-OM1
  #OMtemp@nsim<-1000
  #OMtemp<-LH2OM(OMtemp, dist='norm',plot=F,filterMK=T) # get sample
  #OM1@K<-quantile(OMtemp@cpars$K,c(0.1,0.9)) # 80th percentile from LH2OM
  #OM1<-LH2OM(OM1, dist='norm',plot=F,filterMK=T) # truncated sample

  OM1@L50<-quantile(OM1@cpars$L50,c(0.1,0.90))
  OM1@L50_95<-c(10,10)
  OM1@Linf<-c(100,100)
  OM1@D<-getminmax(1,"D",PanelState)                                                        # F3 -----------
  OM1@h<-getminmax(1,"h",PanelState)                                                        # F4 -----------

  # Ftrend and error
  # eff_values<-readRDS("C:/temp/eff_values.rda"); input<-list(ny=68); nyears=68; nsim=48; Esd_min=0.1; Esd_max=0.5 # F5 -----------
  trends<-effort_mat()
  trends<-trends/apply(trends,1,mean)
  nt<-dim(trends)[1]

  Esd<-getminmax(1,"F",PanelState)                                                         # F6 ----------
  Esd_max<-Esd[2]
  Esd_min<-Esd[1]
  Esdrand<-samp_par(nsim,type=type,Esd_min,Esd_max,trunc=trunc) #runif(nsim,Esd_min,Esd_max)
  Emu<-(-0.5*Esdrand^2)
  Esdarray<-array(exp(rnorm(nsim*Nyears,Emu,Esdrand)),c(nsim,Nyears))

  qhs<-getminmax(1,"qh",PanelState)
  qhssim<-samp_par(nsim,type=type,qhs[1],qhs[2],trunc=trunc) #(nsim,qhs[1],qhs[2])
  qssim<-1+qhssim/100                                                   # F7 ----------
  trendsamp<-ceiling(runif(nsim)*nt)

  Find<-array(NA,c(nsim,Nyears))
  for(i in 1:nsim)Find[i,]<-trends[trendsamp[i],]*Esdarray[i,]* qssim[i]^((1:Nyears)-(Nyears/2))

  # --- Future catchability ----------

  OM1@qinc<-getminmax(1,"q",PanelState)                                                     # F8 ----------

  # --- Selectivity -----------------------

  Sel50<-getminmax(1,"sel",PanelState)                                                     # F10 ----------
  Sel50sim<-samp_par(nsim,type=type,Sel50[1],Sel50[2],trunc=trunc) #runif(nsim,Sel50[1],Sel50[2])

  L5<-OM1@cpars$Linf*Sel50sim*0.8
  LFS<-OM1@cpars$Linf*Sel50sim*1.2
  cond<-LFS>0.95*OM1@cpars$Linf
  LFS[cond]<-0.95*OM1@cpars$Linf[cond]
  Linf<-rep(100,nsim)

  OM1@Vmaxlen<-getminmax(1,"dome",PanelState)                                               # F11 ----------

  # --- Discarding ------------------------

  OM1@DR<-getminmax(1,"DR",PanelState) # F12 ----------
  #DR<-matrix(samp_par(nsim,type=type,OM1@DR[1],OM1@DR[2],trunc=trunc),ncol=nsim,nrow=nyears+proyears,byrow=T)

  OM1@Fdisc<-getminmax(1,"PRM",PanelState)                                                  # F13 ----------

  # --- Recruitment deviations ------------

  OM1@Perr<-getminmax(1,"sigR",PanelState)                                                  # F14 ----------

  # --- MPAs ------------------------------

  nareas<-3

  Ahrng<-getminmax(1,"Ah",PanelState) # size / frac habitat area 3                         # F15 ----------
  Vhrng<-getminmax(1,"Vh",PanelState) # prob staying in area 3                             # F16 ----------
  Arng<-getminmax(1,"A",PanelState)   # size / frac habitat area 1                         # F17 ----------
  Vrng<-getminmax(1,"V",PanelState)   # prob staying in area 3                             # F18 ----------

  Ahsim<-samp_par(nsim,type=type,Ahrng[1],Ahrng[2],trunc=trunc) #runif(nsim,Ahrng[1],Ahrng[2])
  Vhsim<-samp_par(nsim,type=type,Vhrng[1],Vhrng[2],trunc=trunc) #runif(nsim,Vhrng[1],Vhrng[2])
  Asim<-samp_par(nsim,type=type,Arng[1],Arng[2],trunc=trunc) #runif(nsim,Arng[1],Arng[2])
  Vsim<-samp_par(nsim,type=type,Vrng[1],Vrng[2],trunc=trunc) #runif(nsim,Vrng[1],Vrng[2])

  ilogit<-function(x)log(x/(1-x))
  logit<-function(x)exp(x)/(1+exp(x))

  mov1<-mov2<-array(NA,c(nsim,2,2))

  for(i in 1:nsim){
    mov1[i,,]<-getmov2(i,Vsim,Asim)
    mov2[i,,]<-getmov2(i,Vhsim,Ahsim)
  }

  V2<-apply(cbind(mov1[,2,2], # staying in areas 2 and 3 minus staying in area 3
                  mov2[,2,2]), # staying in areas 2 and 3 minus staying in area 1
            1,mean) # a WRONG GUESS of the prob_staying in area 2 - need to do the linear equation modelling for this.

  Sz2<-1-(Ahsim+Asim)
  Asize<-cbind(Asim,Sz2,Ahsim) # area 1 is Asim as future MPs close area 1
  probs<-cbind(Vsim,V2,Vhsim)

  # plot(Ahsim,Vhsim,type='l',xlim=c(0,0.9)); lines(Asim,Vsim,col="grey"); lines(Sz2,V2,col="red")

  mov<-array(NA,c(nsim, maxage+1, nareas, nareas, Nyears+proyears))
  for(i in 1:nsim)mov[i,,,,]<-array(rep(makemov(fracs=Asize[i,], prob=probs[i,]),each=maxage+1),c(maxage+1,nareas,nareas,Nyears+proyears))

  # OM1@MPA<-matrix(c(1,1,1,0,                                            # year1, area1 open, area2 open, area3 shut
  #                 nyears-1,0,1,1),ncol=nareas+1,byrow=T)              # nyears-1, area1 shut, area2 open, area3 open

  OM1@cpars$MPA<-matrix(1,nrow=OM1@nyears+OM1@proyears,ncol=3)
  OM1@cpars$MPA[1:(Nyears-1),3]<-0
  OM1@cpars$MPA[Nyears:proyears,1]<-0

  # Initial depletion                                                                      # F19 ----------
  initDrng<-getminmax(1,"Dh",PanelState)
  #print(initDrng)
  initD<-samp_par(nsim,type=type,initDrng[1],initDrng[2],trunc=trunc) #runif(nsim,initDrng[1],initDrng[2])

  # ---- Management parameters -----------------------------------------------------------------------------------------------

  OM1@TACFrac<-getminmax(2,"IB",PanelState)                                                 # M2 -----------
  OM1@TACSD<-getminmax(2,"IV",PanelState)                                                   # M3 -----------

  OM1@TAEFrac<-getminmax(2,"IBE",PanelState)                                                # M4 -----------
  OM1@TAESD<-getminmax(2,"IVE",PanelState)                                                  # M5 -----------

  OM1@SizeLimFrac<-getminmax(2,"IBSL",PanelState)                                           # M6 -----------
  OM1@SizeLimSD<-getminmax(2,"IVSL",PanelState)                                             # M7 -----------


  # ---- Data parameters -----------------------------------------------------------------------------------------------------

  CB_rng<-getminmax(3,"CB",PanelState)                                                     # D2 -----------
  Cbias<-samp_par(nsim,type=type,CB_rng[1],CB_rng[2],trunc=trunc) #runif(nsim,CB_rng[1],CB_rng[2])

  OM1@beta<-getminmax(3,"Beta",PanelState)                                                  # D3 -----------


  # ---- Custom parameters ---------------------------------------------------------------------------------------------------

  slots2cpars<-c("D","h","Vmaxlen","Fdisc","Perr","TACFrac","TACSD",
                 "TAEFrac","TAESD","SizeLimFrac","SizeLimSD","beta") # all slots that need making into cpars vectors

  makevec<-function(i,OM1,slots2cpars,nsim,type,trunc){
    LB<-slot(OM1,slots2cpars[i])[1]
    UB<-slot(OM1,slots2cpars[i])[2]
    OM1@cpars[[slots2cpars[i]]]<-samp_par(nsim,type=type,LB,UB,trunc)
    OM1
  }

  for(i in 1:length(slots2cpars))OM1<-makevec(i,OM1,slots2cpars,nsim,type,trunc)

  OM1@cpars<-c(OM1@cpars,list(Find=Find,L5=L5,LFS=LFS,Asize=Asize,mov=mov,initD=initD,Cbias=Cbias,
                              control=list(progress=T,ntrials=1000,fracD=0.2)))#,DR=DR))

  SampList<<-data.frame(Esdrand,qhssim,Sel50sim,Ahsim,Vhsim,Asim,Vsim,initD,Cbias)

  # ---- Bioeconomic parameters ----------------------------------------------------------------------------------------------
  #AM("TEST BE")

  # if(input$EC_Model!="None"){

  #  OM1@cpars<-c(OM1@cpars,list(CostCurr=rep(input$CostCurr,OM1@nsim),
  #                           RevCurr=rep(input$RevCurr,OM1@nsim),
  #                            Response=rep(input$Response/100,OM1@nsim),
  #                           CostInc=rep(input$CostInc,OM1@nsim),
  #                          RevInc=rep(input$RevInc,OM1@nsim)))
  #AM("Using bioeconomic model parameters")

  #}

  # ---- Data overwriting ---------------------------------------------------------------------------------------------------
  #saveRDS(OM,"C:/temp/OMpost.rda") #
  #saveRDS(dat,"C:/temp/datpost.rda") #

  if(Data()==1){
    AM("Questionnaire growth and mortality overwritten by those specified in uploaded data")
    if(!is.na(dat@vbLinf[1])){
      ratio<-dat@vbLinf[1]/mean(OM1@cpars$Linf)
      OM1@Linf<-rep(dat@vbLinf,2)
      OM1@cpars$Linf<-OM1@cpars$Linf*ratio
      OM1@cpars$LFS<-OM1@cpars$LFS*ratio
      OM1@cpars$L5<-OM1@cpars$L5*ratio
      OM1@cpars$L50<-OM1@cpars$L50*ratio
    }

    if(!is.na(dat@wla))OM1@a<-dat@wla
    if(!is.na(dat@wlb))OM1@b<-dat@wlb
    if(!is.na(dat@vbt0[1])) OM1@t0<-rep(dat@vbt0[1],2)
    if(!is.na(dat@vbK[1])){ OM1@K<-rep(dat@vbK[1],2); OM1@cpars$K<-OM1@cpars$K*dat@vbK/mean(OM1@cpars$K)}
    if(!is.null(dat@Mort) & !is.na(dat@Mort)) OM1@cpars$M <- OM1@cpars$M * dat@Mort / mean(OM1@cpars$M)

    if(!is.na(dat@LFC))OM1@L5<-rep(dat@LFC,2)
    if(!is.na(dat@LFS))OM1@LFS<-rep(dat@LFS,2)
    if(!is.na(dat@Vmaxlen))OM1@Vmaxlen<-rep(dat@Vmaxlen,2)
    if(!is.na(dat@LenCV))OM1@LenCV<-rep(dat@LenCV,2)

    # depletion to cpars


  }

  # AM("Using questionnaire-based operating model")

  if(Data()==1&input$OM_C){

    code<-input$Cond_ops
    AM(paste0("Conditioning operating model using method ",code))

    #setup(cpus=ncpus)

    tryCatch({

      withProgress(message = "Conditioning Operating Model", value = 0, {
        incProgress(0.1)
        #saveRDS(OM1,"C:/temp/OM.rda")
        #saveRDS(dat,"C:/temp/dat.rda")
        dofit(OM1,dat)
        CFit<-Status$Fit[[1]] #GetDep(OM,dat,code=code,cores=4)
        if(sum(CFit@conv)==0)AM(paste0(code,": ",sum(CFit@conv), " of ",length(CFit@conv)," simulations converged"))
        incProgress(0.8)

      })
      #saveRDS(CFit,"C:/temp/CFit.rda")
      OM1<-SubCpars(CFit@OM,CFit@conv) # subset operating model by converged runs and final depletion below 150% unfished
      SampList<<-SampList[CFit@conv,]  # subset other question parameter samples
      updateNumericInput(session=session, "nsim", value=sum(CFit@conv)) # make sure OM nsim matches the text box

      CondOM(1)
      SD(1)
      AM("------------- New conditioned OM made --------------")
      MadeOM(1)
      redoSD()
      AM("Updating status determination outputs following OM rebuilding")

    },
    error = function(e){
      AM(paste0(e,sep="\n"))
      shinyalert("Computational error", "Operating model conditioning returned an error. Try using a different model for conditioning.", type = "info")
    }
    )

    #testing=F
    #if(testing){
    # MSEobj<-runMSE(OM,"DCAC")
    #  OM_reb<-OM
    #  OM_reb@proyears<-max(OM1@proyears,20+2) # only have to compute to this year
    #  Dep_reb<-runif(OM1@nsim,50,50)#input$Dep_reb[1],input$Dep_reb[2]) # is a %
    #  OM_reb@cpars$D<-(Dep_reb/100)*MSEobj@OM$SSBMSY_SSB0#apply(MSEobj@SSB_hist[,,MSEobj@nyears,],1, sum)/(MSEobj@OM$SSB0*2) # start from half BMSY
    #  MSEobj_reb<-runMSE(OM_reb,"DCAC")
    #  Bdeps<-MSEobj_reb@OM$D/MSEobj_reb@OM$SSBMSY_SSB0#MSEobj_reb@B_BMSY[,1,1]#
    #}

  } # end of OM conditioning
  AM("------------- New OM made --------------")
  MadeOM(1)
} # #end of loaded OM or not
# OM1 # OM




#
#
#
#   MERA_Import <- list()
#
#   # ---- Fishery Questions ----
#   # F1
#   MERA_Import$Des<-list()
#   MERA_Import$Des$Name <- Data@Common_Name
#   MERA_Import$Des$Species <- Data@Species
#   MERA_Import$Des$Region <- Data@Region
#   MERA_Import$Des$Agency <- NA # Not in currently in Data file
#   MERA_Import$Des$Syear <- max(Data@Year)
#   MERA_Import$Des$Lyear<- min(Data@Year)
#
#   authors <- FPI.Cover[18,2:ncol(FPI.Cover)]
#   authors <- as.matrix(authors)
#   authors <- authors[!is.na(authors)]
#   authors <- paste(authors, collapse=", ")
#   if (length(authors)<1) authors <- NA
#
#   MERA_Import$Des$Author <- authors # Not in currently in Data file
#
#   # F2
#   if (is.na(Data@Mort))  errlist$Mort<-"Natural mortality rate slot 'M' not specified in sheet '12. Fishery Data'"
#   if (is.na(Data@CV_Mort)) errlist$CV_Mort <- "Coefficient of Variation slot 'CV M' not specificed in sheet '12. Fishery Data'"
#
#   # F3
#   if (is.na(Data@Dep)) {
#     AM("Depletion slot 'Current stock depletion' not specified in sheet '12. Fishery Data' - maximum uncertainty in stock depletion has been assumed")
#   } else {
#     if (is.na(Data@CV_Dep)){
#       AM("Depletion coefficient of variation slot 'CV current stock depletion' not specified in sheet '12. Fishery Data'- maximum uncertainty in stock depletion has been assumed")
#     }
#   }
#
#   # F4
#   if (is.na(Data@steep)) {
#     # Default is last 3 options
#     AM("Resilience / steepness slot 'Steepness' not specified in sheet '12. Fishery Data' - values in the range of 0.5 to 1 are assumed")
#     PanelState$Fpanel[[3]][1:2] <- F
#   }
#
#   # F5
#   # TODO: Import from Data@Effort or answer in MERA
#   MERA_Import$eff_values<-list()
#   MeRA_Import$eff_values$df<-data.frame(x=Data@Year,y=Data@Effort,series=rep(1,length(Data@Year)))
#
#   # F6
#   F6in <- as.numeric(stringr::str_extract_all(MERA.Qs$Score[1], "[0-9]+")[[1]])
#   F6 <- rep(FALSE, 3)
#   if (1 %in% F6in) F6[1] <- TRUE
#   if (2 %in% F6in) F6[2] <- TRUE
#   if (3 %in% F6in) F6[3] <- TRUE
#   MERA_Import$F6 <- (1:3)[F6]
#
#   # F7
#   F7in <- as.numeric(stringr::str_extract_all(MERA.Qs$Score[2], "[0-9]+")[[1]])
#   F7 <- rep(FALSE, 5)
#   if (1 %in% F7in) F7[3] <- TRUE
#   if (2 %in% F7in) F7[4] <- TRUE
#   if (3 %in% F7in) F7[5] <- TRUE
#   if (4 %in% F7in) F7[2] <- TRUE
#   if (5 %in% F7in) F7[1] <- TRUE
#   MERA_Import$F7 <- (1:5)[F7]
#   if (length( MERA_Import$F7) ==0)  MERA_Import$F7 <- 3
#   MERA_Import$F7 <- min(MERA_Import$F7):max(MERA_Import$F7)
#
#   # F8
#   # Default to stable
#   MERA_Import$F8 <- 3
#
#   # F9
#   if(!is.na(Data@L50) & !is.na(Data@vbLinf)) {
#     if (is.na(Data@CV_L50)) stop("CV_L50 is NA")
#     if (is.na(Data@CV_vbLinf)) stop("CV_vbLinf is NA")
#
#     L50range <- qlnorm(quants, log(Data@L50), Data@CV_L50)
#     Linfrange <- qlnorm(quants, log(Data@vbLinf), Data@CV_vbLinf)
#     Lmrange <- (L50range/Linfrange) %>% sort()
#     group <- c(0, 0.5, 0.6, 0.7, 0.8, 1)
#     MERA_Import$F9 <- match(cut(Lmrange, group), levels(cut(Lmrange, group)))
#     MERA_Import$F9 <- min(MERA_Import$F9):max(MERA_Import$F9)
#
#   } else {
#     MERA_Import$F9 <- 1:5
#   }
#
#   # F10
#   if(!is.na(Data@LFC) & !is.na(Data@vbLinf)) {
#     if (is.na(Data@CV_LFC)) stop("CV_LFC is NA")
#     if (is.na(Data@CV_vbLinf)) stop("CV_vbLinf is NA")
#
#     LFCrange <- qlnorm(quants, log(Data@LFC), Data@CV_LFC)
#     Linfrange <- qlnorm(quants, log(Data@vbLinf), Data@CV_vbLinf)
#     Lcrange <- (LFCrange/Linfrange) %>% sort()
#     group <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
#     MERA_Import$F10 <- match(cut(Lcrange, group), levels(cut(Lcrange, group)))
#     MERA_Import$F10 <- min(MERA_Import$F10):max(MERA_Import$F10)
#
#   } else {
#     MERA_Import$F10 <- 1:5
#   }
#
#   # F11
#   if (is.na(Data@Vmaxlen)) {
#     MERA_Import$F11 <- 1
#   } else {
#     group <- c(0, 0.25, 0.75, 0.99, 2)
#     lev <- match(cut(Data@Vmaxlen, group), levels(cut(Data@Vmaxlen, group)))
#     MERA_Import$F11 <- -lev + 5
#   }
#
#   # F12
#   F12in <- as.numeric(stringr::str_extract_all(MERA.Qs$Score[3], "[0-9]+")[[1]])
#   F12 <- rep(FALSE, 5)
#   if (1 %in% F12in) F12[1] <- TRUE
#   if (2 %in% F12in) F12[2] <- TRUE
#   if (3 %in% F12in) F12[3] <- TRUE
#   if (4 %in% F12in) F12[4] <- TRUE
#   if (5 %in% F12in) F12[5] <- TRUE
#   MERA_Import$F12 <- (1:5)[F12]
#   MERA_Import$F12 <- min(MERA_Import$F12):max(MERA_Import$F12)
#
#   # F13
#   F13in <- as.numeric(stringr::str_extract_all(MERA.Qs$Score[4], "[0-9]+")[[1]])
#   F13 <- rep(FALSE, 6)
#   if (1 %in% F13in) F13[1] <- TRUE
#   if (2 %in% F13in) F13[2] <- TRUE
#   if (3 %in% F13in) F13[3] <- TRUE
#   if (4 %in% F13in) F13[4] <- TRUE
#   if (5 %in% F13in) F13[5] <- TRUE
#   if (6 %in% F13in) F13[6] <- TRUE
#   MERA_Import$F13 <- (1:6)[F13]
#   MERA_Import$F13 <- min(MERA_Import$F13):max(MERA_Import$F13)
#
#   # F14
#   # Default to moderate & high unless in Data
#   if (!is.na(Data@sigmaR)) {
#     if (is.na(Data@CV_sigmaR)) stop("CV_sigmaR is NA")
#     sigmaRrange <- qlnorm(quants, log(Data@sigmaR), Data@CV_sigmaR)
#     group <- c(0, 0.1, 0.3, 0.6, 0.9, 1E3)
#     MERA_Import$F14 <- match(cut(sigmaRrange, group), levels(cut(sigmaRrange, group)))
#   } else {
#     MERA_Import$F14 <- 3:4
#   }
#
#   # F15
#   ind <- which(FPI.Inputs[,3] == "MPAs and Sanctuaries")
#   MPAq <- FPI.Inputs[ind,6]
#
#   if (MPAq == 1) MERA_Import$F15 <- 1
#   if (MPAq == 2) MERA_Import$F15 <- 2
#   if (MPAq == 3) MERA_Import$F15 <- 3
#   if (MPAq == 4) MERA_Import$F15 <- 4
#   if (MPAq == 5) MERA_Import$F15 <- 5:7
#
#   # F16
#   F16in <- as.numeric(stringr::str_extract_all(MERA.Qs$Score[5], "[0-9]+")[[1]])
#   F16 <- rep(FALSE, 5)
#   if (1 %in% F16in) F16[1] <- TRUE
#   if (2 %in% F16in) F16[2] <- TRUE
#   if (3 %in% F16in) F16[3] <- TRUE
#   if (4 %in% F16in) F16[4] <- TRUE
#   if (5 %in% F16in) F16[5] <- TRUE
#   MERA_Import$F16 <- (1:5)[F16]
#   MERA_Import$F16 <- min(MERA_Import$F16):max(MERA_Import$F16)
#
#   # F17
#   F17in <- as.numeric(stringr::str_extract_all(MERA.Qs$Score[6], "[0-9]+")[[1]])
#   F17 <- rep(FALSE, 7)
#   if (1 %in% F17in) F17[1] <- TRUE
#   if (2 %in% F17in) F17[2] <- TRUE
#   if (3 %in% F17in) F17[3] <- TRUE
#   if (4 %in% F17in) F17[4] <- TRUE
#   if (5 %in% F17in) F17[5] <- TRUE
#   if (6 %in% F17in) F17[6] <- TRUE
#   if (7 %in% F17in) F17[7] <- TRUE
#   MERA_Import$F17 <- (1:7)[F17]
#   MERA_Import$F17 <- min(MERA_Import$F17):max(MERA_Import$F17)
#
#   # F18
#   F18in <- as.numeric(stringr::str_extract_all(MERA.Qs$Score[7], "[0-9]+")[[1]])
#   F18 <- rep(FALSE, 5)
#   if (1 %in% F18in) F18[1] <- TRUE
#   if (2 %in% F18in) F18[2] <- TRUE
#   if (3 %in% F18in) F18[3] <- TRUE
#   if (4 %in% F18in) F18[4] <- TRUE
#   if (5 %in% F18in) F18[5] <- TRUE
#   MERA_Import$F18 <- (1:5)[F18]
#   MERA_Import$F18 <- min(MERA_Import$F18):max(MERA_Import$F18)
#
#   # F19
#   # Default to unfished
#   MERA_Import$F19 <- 5
#
#   # ---- Management Questions ----
#   # M1
#   M1in <- as.numeric(stringr::str_extract_all(MERA.Qs$Score[8], "[0-9]+")[[1]])
#   M1 <- rep(FALSE, 4)
#   if (1 %in% M1in) M1[1] <- TRUE
#   if (2 %in% M1in) M1[2] <- TRUE
#   if (3 %in% M1in) M1[3] <- TRUE
#   if (4 %in% M1in) M1[4] <- TRUE
#   MERA_Import$M1 <- (1:4)[M1]
#   MERA_Import$M1 <- min(MERA_Import$M1):max(MERA_Import$M1)
#
#   # M2 & M3
#   ind <- which(FPI.Inputs[,3] == "Enforcement Capability")
#   Enforce <- FPI.Inputs[ind,6]
#
#   MERA_Import$M2 <- 4 # average taken exactly
#   # variability linked to enforcement
#   if (Enforce == 1) MERA_Import$M3 <- 5
#   if (Enforce == 2) MERA_Import$M3 <- 4:5
#   if (Enforce == 3) MERA_Import$M3 <- 3:4
#   if (Enforce == 4) MERA_Import$M3 <- 2:3
#   if (Enforce == 5) MERA_Import$M3 <- 1
#
#   # M4 - M7 - Mirror M2 & M3
#   MERA_Import$M4 <- MERA_Import$M6 <-   MERA_Import$M2
#   MERA_Import$M5 <- MERA_Import$M7 <-   MERA_Import$M3
#
#   # ---- Data Questions ----
#
#   # D1
#   MERA_Import$D1 <- NA # not required - Data already loaded in FPI+
#
#   # D2
#   D2in <- as.numeric(stringr::str_extract_all(MERA.Qs$Score[9], "[0-9]+")[[1]])
#   D2 <- rep(FALSE, 5)
#   if (1 %in% D2in) D2[1] <- TRUE
#   if (2 %in% D2in) D2[2] <- TRUE
#   if (3 %in% D2in) D2[3] <- TRUE
#   if (4 %in% D2in) D2[4] <- TRUE
#   if (5 %in% D2in) D2[5] <- TRUE
#   MERA_Import$D2 <- (1:5)[D2]
#   MERA_Import$D2 <- min(MERA_Import$D2):max(MERA_Import$D2)
#
#   # D3
#   D3in <- as.numeric(stringr::str_extract_all(MERA.Qs$Score[9], "[0-9]+")[[1]])
#   D3 <- rep(FALSE, 5)
#   if (1 %in% D3in) D3[1] <- TRUE
#   if (2 %in% D3in) D3[2] <- TRUE
#   if (3 %in% D3in) D3[3] <- TRUE
#   if (4 %in% D3in) D3[4] <- TRUE
#   if (5 %in% D3in) D3[5] <- TRUE
#   MERA_Import$D3 <- (1:5)[D3]
#   MERA_Import$D3 <- min(MERA_Import$D3):max(MERA_Import$D3)
#
#   # D4
#   ind <- which(FPI.Inputs[,3] == "Data Availability")
#   DataQual <- FPI.Inputs[ind,6]
#
#   if (DataQual == 1) MERA_Import$D4 <- 4
#   if (DataQual == 2) MERA_Import$D4 <- 4
#   if (DataQual == 3) MERA_Import$D4 <- 3
#   if (DataQual == 4) MERA_Import$D4 <- 2
#   if (DataQual == 5) MERA_Import$D4 <- 2
#
#   MERA_Import
# }
#





makeOM<-function(PanelState,nyears=NA,maxage=NA,proyears=NA,UseQonly=F){

  # ---- Misc OM building ------------------------------------------------------------------------------------
  nsim<-input$nsim

  if(input$OM_L & !UseQonly){
    OM1<-OM_L
    SampList<<-NULL
    AM("Using loaded operating model")

  }else{

    type<-input$Distribution # sampling distribution
    trunc<-input$IQRange     # inter quartile range (trunc, a % e.g. 90)

    OM1<-LowSlopes(MSEtool::testOM)
    if(!is.na(nsim)){
      OM1@nsim<-nsim
    }else{
      OM1<-trimOM(OM1,input$nsim)
    }

    if(input$use_seed)OM1@seed<-input$seed

    OM1@R0<-1e9
    OM1@Linf<-c(100,100)
    OM1@L50<-NaN
    OM1@K<-NaN
    OM1@isRel<-"FALSE"

    OM1@Name<-input$Name
    OM1@Species<-input$Species
    OM1@Region<-input$Region
    OM1@Agency<-input$Agency
    Nyears<-input$Lyear-input$Syear+1
    OM1@nyears<-Nyears

    OM1@Source<-input$Author
    OM1@interval<-input$interval
    if(is.na(proyears)){
      OM1@proyears<-proyears<-50 #input$proyears
    }else{
      OM1@proyears<-proyears #input$proyears
    }

    loc<-match("Err",inputnames[[3]])                                                        # D1 -----------
    cond<-as.vector(unlist(PanelState[[3]][loc]))
    Dquality<-as.vector(unlist(Err_list)[cond])

    if(Dquality=="Err_perf"){
      temp<-new('OM',Albacore,Generic_Fleet,Perfect_Info,Perfect_Imp)
    }else if(Dquality=="Err_good"){
      temp<-new('OM',Albacore,Generic_Fleet,Precise_Unbiased,Perfect_Imp)
    }else if(Dquality=="Err_mod"){
      temp<-new('OM',Albacore,Generic_Fleet,Generic_obs,Perfect_Imp)
    }else{
      temp<-new('OM',Albacore,Generic_Fleet,Imprecise_Biased,Perfect_Imp)
    }

    OM1<-Replace(OM1,temp,Sub="Obs")

    # ---- Fishery characteristics ---------------------------------------------------------------------------

    OM1@M<-getminmax(1,"M",PanelState)                                                        # F2 ----------
    # add M to cpars

    OM1@L50<-getminmax(1,"LM",PanelState)                                                     # F9 ----------

    if(is.na(maxage)){
      OM1@maxage<-maxage<-ceiling(-log(0.1)/min(OM1@M))
    }else{
      OM1@maxage=maxage
    }
    OM1@maxage<-maxage<-min(OM1@maxage,input$plusgroup)


    # --- Life history imputation
    OMtemp<-OM1
    OMtemp@nsim<-1000
    OMtemp<-LH2OM(OMtemp, dist='norm',plot=F,filterMK=T) # get sample
    OM1@K<-quantile(OMtemp@cpars$K,c(0.1,0.9)) # 80th percentile from LH2OM
    OM1<-LH2OM(OM1, dist='norm',plot=F,filterMK=T) # truncated sample

    OM1@L50<-quantile(OM1@cpars$L50,c(0.1,0.90))
    OM1@L50_95<-c(10,10)
    OM1@Linf<-c(100,100)
    OM1@D<-getminmax(1,"D",PanelState)                                                        # F3 -----------
    OM1@h<-getminmax(1,"h",PanelState)                                                        # F4 -----------

    # Ftrend and error
    # eff_values<-readRDS("C:/temp/eff_values.rda"); input<-list(ny=68); nyears=68; nsim=48; Esd_min=0.1; Esd_max=0.5 # F5 -----------
    trends<-effort_mat()
    trends<-trends/apply(trends,1,mean)
    nt<-dim(trends)[1]

    Esd<-getminmax(1,"F",PanelState)                                                         # F6 ----------
    Esd_max<-Esd[2]
    Esd_min<-Esd[1]
    Esdrand<-samp_par(nsim,type=type,Esd_min,Esd_max,trunc=trunc) #runif(nsim,Esd_min,Esd_max)
    Emu<-(-0.5*Esdrand^2)
    Esdarray<-array(exp(rnorm(nsim*Nyears,Emu,Esdrand)),c(nsim,Nyears))

    qhs<-getminmax(1,"qh",PanelState)
    qhssim<-samp_par(nsim,type=type,qhs[1],qhs[2],trunc=trunc) #(nsim,qhs[1],qhs[2])
    qssim<-1+qhssim/100                                                   # F7 ----------
    trendsamp<-ceiling(runif(nsim)*nt)

    Find<-array(NA,c(nsim,Nyears))
    for(i in 1:nsim)Find[i,]<-trends[trendsamp[i],]*Esdarray[i,]* qssim[i]^((1:Nyears)-(Nyears/2))

    # --- Future catchability ----------

    OM1@qinc<-getminmax(1,"q",PanelState)                                                     # F8 ----------

    # --- Selectivity -----------------------

    Sel50<-getminmax(1,"sel",PanelState)                                                     # F10 ----------
    Sel50sim<-samp_par(nsim,type=type,Sel50[1],Sel50[2],trunc=trunc) #runif(nsim,Sel50[1],Sel50[2])

    L5<-OM1@cpars$Linf*Sel50sim*0.8
    LFS<-OM1@cpars$Linf*Sel50sim*1.2
    cond<-LFS>0.95*OM1@cpars$Linf
    LFS[cond]<-0.95*OM1@cpars$Linf[cond]
    Linf<-rep(100,nsim)

    OM1@Vmaxlen<-getminmax(1,"dome",PanelState)                                               # F11 ----------

    # --- Discarding ------------------------

    OM1@DR<-getminmax(1,"DR",PanelState) # F12 ----------
    #DR<-matrix(samp_par(nsim,type=type,OM1@DR[1],OM1@DR[2],trunc=trunc),ncol=nsim,nrow=nyears+proyears,byrow=T)

    OM1@Fdisc<-getminmax(1,"PRM",PanelState)                                                  # F13 ----------

    # --- Recruitment deviations ------------

    OM1@Perr<-getminmax(1,"sigR",PanelState)                                                  # F14 ----------

    # --- MPAs ------------------------------

    nareas<-3

    Ahrng<-getminmax(1,"Ah",PanelState) # size / frac habitat area 3                         # F15 ----------
    Vhrng<-getminmax(1,"Vh",PanelState) # prob staying in area 3                             # F16 ----------
    Arng<-getminmax(1,"A",PanelState)   # size / frac habitat area 1                         # F17 ----------
    Vrng<-getminmax(1,"V",PanelState)   # prob staying in area 3                             # F18 ----------

    Ahsim<-samp_par(nsim,type=type,Ahrng[1],Ahrng[2],trunc=trunc) #runif(nsim,Ahrng[1],Ahrng[2])
    Vhsim<-samp_par(nsim,type=type,Vhrng[1],Vhrng[2],trunc=trunc) #runif(nsim,Vhrng[1],Vhrng[2])
    Asim<-samp_par(nsim,type=type,Arng[1],Arng[2],trunc=trunc) #runif(nsim,Arng[1],Arng[2])
    Vsim<-samp_par(nsim,type=type,Vrng[1],Vrng[2],trunc=trunc) #runif(nsim,Vrng[1],Vrng[2])

    ilogit<-function(x)log(x/(1-x))
    logit<-function(x)exp(x)/(1+exp(x))

    mov1<-mov2<-array(NA,c(nsim,2,2))

    for(i in 1:nsim){
      mov1[i,,]<-getmov2(i,Vsim,Asim)
      mov2[i,,]<-getmov2(i,Vhsim,Ahsim)
    }

    V2<-apply(cbind(mov1[,2,2], # staying in areas 2 and 3 minus staying in area 3
                    mov2[,2,2]), # staying in areas 2 and 3 minus staying in area 1
              1,mean) # a WRONG GUESS of the prob_staying in area 2 - need to do the linear equation modelling for this.

    Sz2<-1-(Ahsim+Asim)
    Asize<-cbind(Asim,Sz2,Ahsim) # area 1 is Asim as future MPs close area 1
    probs<-cbind(Vsim,V2,Vhsim)

    # plot(Ahsim,Vhsim,type='l',xlim=c(0,0.9)); lines(Asim,Vsim,col="grey"); lines(Sz2,V2,col="red")

    mov<-array(NA,c(nsim, maxage+1, nareas, nareas, Nyears+proyears))
    for(i in 1:nsim)mov[i,,,,]<-array(rep(makemov(fracs=Asize[i,], prob=probs[i,]),each=maxage+1),c(maxage+1,nareas,nareas,Nyears+proyears))

    # OM1@MPA<-matrix(c(1,1,1,0,                                            # year1, area1 open, area2 open, area3 shut
    #                 nyears-1,0,1,1),ncol=nareas+1,byrow=T)              # nyears-1, area1 shut, area2 open, area3 open

    OM1@cpars$MPA<-matrix(1,nrow=OM1@nyears+OM1@proyears,ncol=3)
    OM1@cpars$MPA[1:(Nyears-1),3]<-0
    OM1@cpars$MPA[Nyears:proyears,1]<-0

    # Initial depletion                                                                      # F19 ----------
    initDrng<-getminmax(1,"Dh",PanelState)
    #print(initDrng)
    initD<-samp_par(nsim,type=type,initDrng[1],initDrng[2],trunc=trunc) #runif(nsim,initDrng[1],initDrng[2])

    # ---- Management parameters -----------------------------------------------------------------------------------------------

    OM1@TACFrac<-getminmax(2,"IB",PanelState)                                                 # M2 -----------
    OM1@TACSD<-getminmax(2,"IV",PanelState)                                                   # M3 -----------

    OM1@TAEFrac<-getminmax(2,"IBE",PanelState)                                                # M4 -----------
    OM1@TAESD<-getminmax(2,"IVE",PanelState)                                                  # M5 -----------

    OM1@SizeLimFrac<-getminmax(2,"IBSL",PanelState)                                           # M6 -----------
    OM1@SizeLimSD<-getminmax(2,"IVSL",PanelState)                                             # M7 -----------


    # ---- Data parameters -----------------------------------------------------------------------------------------------------

    CB_rng<-getminmax(3,"CB",PanelState)                                                     # D2 -----------
    Cbias<-samp_par(nsim,type=type,CB_rng[1],CB_rng[2],trunc=trunc) #runif(nsim,CB_rng[1],CB_rng[2])

    OM1@beta<-getminmax(3,"Beta",PanelState)                                                  # D3 -----------


    # ---- Custom parameters ---------------------------------------------------------------------------------------------------

    slots2cpars<-c("D","h","Vmaxlen","Fdisc","Perr","TACFrac","TACSD",
                   "TAEFrac","TAESD","SizeLimFrac","SizeLimSD","beta") # all slots that need making into cpars vectors

    makevec<-function(i,OM1,slots2cpars,nsim,type,trunc){
      LB<-slot(OM1,slots2cpars[i])[1]
      UB<-slot(OM1,slots2cpars[i])[2]
      OM1@cpars[[slots2cpars[i]]]<-samp_par(nsim,type=type,LB,UB,trunc)
      OM1
    }

    for(i in 1:length(slots2cpars))OM1<-makevec(i,OM1,slots2cpars,nsim,type,trunc)

    OM1@cpars<-c(OM1@cpars,list(Find=Find,L5=L5,LFS=LFS,Asize=Asize,mov=mov,initD=initD,Cbias=Cbias,
                                control=list(progress=T,ntrials=1000,fracD=0.2)))#,DR=DR))

    SampList<<-data.frame(Esdrand,qhssim,Sel50sim,Ahsim,Vhsim,Asim,Vsim,initD,Cbias)

    # ---- Bioeconomic parameters ----------------------------------------------------------------------------------------------
    #AM("TEST BE")

    # if(input$EC_Model!="None"){

    #  OM1@cpars<-c(OM1@cpars,list(CostCurr=rep(input$CostCurr,OM1@nsim),
    #                           RevCurr=rep(input$RevCurr,OM1@nsim),
    #                            Response=rep(input$Response/100,OM1@nsim),
    #                           CostInc=rep(input$CostInc,OM1@nsim),
    #                          RevInc=rep(input$RevInc,OM1@nsim)))
    #AM("Using bioeconomic model parameters")

    #}

    # ---- Data overwriting ---------------------------------------------------------------------------------------------------
    #saveRDS(OM,"C:/temp/OMpost.rda") #
    #saveRDS(dat,"C:/temp/datpost.rda") #

    if(Data()==1){
      AM("Questionnaire growth and mortality overwritten by those specified in uploaded data")
      if(!is.na(dat@vbLinf[1])){
        ratio<-dat@vbLinf[1]/mean(OM1@cpars$Linf)
        OM1@Linf<-rep(dat@vbLinf,2)
        OM1@cpars$Linf<-OM1@cpars$Linf*ratio
        OM1@cpars$LFS<-OM1@cpars$LFS*ratio
        OM1@cpars$L5<-OM1@cpars$L5*ratio
        OM1@cpars$L50<-OM1@cpars$L50*ratio
      }

      if(!is.na(dat@wla))OM1@a<-dat@wla
      if(!is.na(dat@wlb))OM1@b<-dat@wlb
      if(!is.na(dat@vbt0[1])) OM1@t0<-rep(dat@vbt0[1],2)
      if(!is.na(dat@vbK[1])){ OM1@K<-rep(dat@vbK[1],2); OM1@cpars$K<-OM1@cpars$K*dat@vbK/mean(OM1@cpars$K)}
      if(!is.null(dat@Mort) & !is.na(dat@Mort)) OM1@cpars$M <- OM1@cpars$M * dat@Mort / mean(OM1@cpars$M)

      if(!is.na(dat@LFC))OM1@L5<-rep(dat@LFC,2)
      if(!is.na(dat@LFS))OM1@LFS<-rep(dat@LFS,2)
      if(!is.na(dat@Vmaxlen))OM1@Vmaxlen<-rep(dat@Vmaxlen,2)
      if(!is.na(dat@LenCV))OM1@LenCV<-rep(dat@LenCV,2)

      # depletion to cpars


    }

    # AM("Using questionnaire-based operating model")

    if(Data()==1&input$OM_C){

      code<-input$Cond_ops
      AM(paste0("Conditioning operating model using method ",code))

      #setup(cpus=ncpus)

      tryCatch({

        withProgress(message = "Conditioning Operating Model", value = 0, {
          incProgress(0.1)
          #saveRDS(OM1,"C:/temp/OM.rda")
          #saveRDS(dat,"C:/temp/dat.rda")
          dofit(OM1,dat)
          CFit<-Status$Fit[[1]] #GetDep(OM,dat,code=code,cores=4)
          if(sum(CFit@conv)==0)AM(paste0(code,": ",sum(CFit@conv), " of ",length(CFit@conv)," simulations converged"))
          incProgress(0.8)

        })
        #saveRDS(CFit,"C:/temp/CFit.rda")
        OM1<-SubCpars(CFit@OM,CFit@conv) # subset operating model by converged runs and final depletion below 150% unfished
        SampList<<-SampList[CFit@conv,]  # subset other question parameter samples
        updateNumericInput(session=session, "nsim", value=sum(CFit@conv)) # make sure OM nsim matches the text box

        CondOM(1)
        SD(1)
        AM("------------- New conditioned OM made --------------")
        MadeOM(1)
        redoSD()
        AM("Updating status determination outputs following OM rebuilding")

      },
      error = function(e){
        AM(paste0(e,sep="\n"))
        shinyalert("Computational error", "Operating model conditioning returned an error. Try using a different model for conditioning.", type = "info")
      }
      )

      #testing=F
      #if(testing){
      # MSEobj<-runMSE(OM,"DCAC")
      #  OM_reb<-OM
      #  OM_reb@proyears<-max(OM1@proyears,20+2) # only have to compute to this year
      #  Dep_reb<-runif(OM1@nsim,50,50)#input$Dep_reb[1],input$Dep_reb[2]) # is a %
      #  OM_reb@cpars$D<-(Dep_reb/100)*MSEobj@OM$SSBMSY_SSB0#apply(MSEobj@SSB_hist[,,MSEobj@nyears,],1, sum)/(MSEobj@OM$SSB0*2) # start from half BMSY
      #  MSEobj_reb<-runMSE(OM_reb,"DCAC")
      #  Bdeps<-MSEobj_reb@OM$D/MSEobj_reb@OM$SSBMSY_SSB0#MSEobj_reb@B_BMSY[,1,1]#
      #}

    } # end of OM conditioning
    AM("------------- New OM made --------------")
    MadeOM(1)
  } # #end of loaded OM or not
  OM1 # OM

}



trimOM<-function(OM1,newsim=8,silent=T){

  if(newsim > OM1@nsim)stop("You asked for more simulations than are available in the OM object")

  if(length(OM1@cpars)==0){

    if(!silent) message("There is no cpars slot in this OM object, only the nsim slot has been modified")
  }else{

    for(i in 1:length(OM1@cpars)){

      dims<-dim(OM1@cpars[[i]])
      ndim<-length(dims)

      if(ndim==0){
        OM1@cpars[[i]]<-OM1@cpars[[i]][1:newsim]
      }else if(ndim==2){
        OM1@cpars[[i]]<-matrix(OM1@cpars[[i]][1:newsim,],nrow=newsim)
      }else if(ndim==3){
        OM1@cpars[[i]]<-array(OM1@cpars[[i]][1:newsim,,],c(newsim,dims[2:3]))
      }else if(ndim==4){
        OM1@cpars[[i]]<-array(OM1@cpars[[i]][1:newsim,,,],c(newsim,dims[2:4]))
      }else if(ndim==5){
        OM1@cpars[[i]]<-array(OM1@cpars[[i]][1:newsim,,,,],c(newsim,dims[2:5]))
      }

    }

  }

  OM1@nsim<-newsim

  OM1
}
