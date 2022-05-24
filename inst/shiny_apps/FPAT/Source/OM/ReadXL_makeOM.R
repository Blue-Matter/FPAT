
#  FPIfile <- 'G:/Shared drives/BM shared/1. Projects/FPAT/FPAT examples/FPAT vBeta-Dive-based fishery CR.xlsx'
#  FPIfile <- 'G:/Shared drives/BM shared/1. Projects/FPAT/FPAT examples/demo.xlsx'
#  mera<-readRDS("G:/Shared drives/BM shared/1. Projects/MERA/MERA_TESTS_2021/TFH/Tiger_flathead.mera")

fetchOM<-function(Info, Toggles, session){
  # reset Info
  Info$sheets=NULL       # Sheet names
  Info$Summary=NULL       # FPI summary tab
  Info$Output_table=NULL  # FPI output-table
  Info$Input_table=NULL   # FPI input-table
  Info$Data = NULL        # Data
  Info$openMSE.Qs = NULL     # M
  Info$FPI.Inputs = NULL  # FPI inputs table
  Info$FPI.Cover = NULL   # FPI cover sheet
  Info$OM = NULL          # Operating model
  Info$MSEhist = NULL     # Historical reconstruction
  Info$MSEproj = NULL     # MSE projection
  Info$MPsel = c("CurC","curEref")        # Selected MPs for running the MSE

  # import FPAT data file with error checks

  # Check if loaded file is valid xlsx file
  is.xlsx <- readxl::format_from_signature(Info$file$datapath) == 'xlsx'
  if (is.na(is.xlsx)) is.xlsx <- FALSE
  # not a valid xlsx file
  if(!is.xlsx) {
    e <- 'Loaded file is not a valid xlsx file (invalid signature). File may be corrupted'
    AM(paste0(e,"\n"))
    shinyalert("FPAT file did not import.", paste("Error:",e), type = "error")
    AM(paste0(e,"\n"))
    return(0)
  }

  # Load the file
  Info$sheets <- readxl::excel_sheets(Info$file$datapath)

  Info <- Check_Sheets(Info)

  # Info$Summary
  # output_dim_scores(FPI.Summary, NULL)


  # FPI part loaded successfully
  Toggles$FPI_Loaded <- TRUE



  if (class(Info$Data)=='try-error') {
    e <- paste("Could not import fishery data from Sheet", Info$Sheet_Names$Fishery_Data,".", sep='\n', Info$Data)
    AM(paste0(e,"\n"))
    shinyalert("The FPI information was imported but the operating model could not be generated:", paste("Error:",e), type = "error")
    AM(paste0(e,"\n"))
    return(0)
  } else {
    # make the operating model
    Info$OM<-try(makeOM(Info), silent=TRUE)

    if (class(Info$OM)=='OM') {
      # OM successfully loaded
      # construct the operating model
      withProgress(message = "Constructing operating model", value = 0, {
        MSEhist<-try(runMSE(Info$OM,Hist=T,extended=T),  silent=TRUE)
      })

      if (class(MSEhist)=='try-error') {
        e <- MSEhist
        AM(paste0(e,"\n"))
        shinyalert("Operating Model could not be generated:", paste("Error:",e), type = "error")
        AM(paste0(e,"\n"))
        return(0)
      } else {
        Info$MSEhist <- MSEhist
        Toggles$OM_Loaded <- TRUE
      }

    }
  }
}


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


makeOM <- function(Info) {

  AM("--- Loading FPAT spreadsheet ----------------")
  errlist<-asslist<-list()

  sheets <- Info$sheets
  Data <- Info$Data
  openMSE.Qs <- Info$openMSE.Qs
  FPI.Inputs <- Info$FPI.Inputs
  FPI.Cover <- Info$FPI.Cover

  plusgroup<-40

  OM<-LowSlopes(MSEtool::testOM)
  OM@nsim<-nsim<- 24
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
  if(Nyears < 15)errlist$Nyears<-paste("You have specfied only",Nyears, "years of fishery effort data (FPAT requires a minimum of 15 years). This should represent a complete picture of the history of the fishery.")
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
  if (DataQual == 3) openMSE_Import$D4 <- temp<-new('OM',Albacore,Generic_Fleet,Generic_obs,Perfect_Imp)
  if (DataQual > 3) openMSE_Import$D4 <- temp<-new('OM',Albacore,Generic_Fleet,Precise_Unbiased,Perfect_Imp)
  OM<-Replace(OM,temp,Sub="Obs")

  OM@cpars<-list()

  # ---- Fishery characteristics ---------------------------------------------------------------------------

  # --- Biological --------------------

  if(is.na(Data@Mort)){
    errlist$Mort<-"Natural mortality rate slot 'M' not specified in sheet '12. Fishery Data'"
  }else{
    OM@M<-rep(Data@Mort,2)
    if(is.na(Data@CV_Mort)){
      asslist$Mort<-"Coefficient of variation in matural mortality rate slot 'CV M' not specified in sheet '12. Fishery Data'. A CV value of 0.1 was assumed."
      OM@cpars$M <- trlnorm(nsim,Data@Mort,0.1)
    }else{
      OM@cpars$M <- trlnorm(nsim,Data@Mort,Data@CV_Mort)
    }
  }

  if(is.na(Data@L50)){
    errlist$maxage<-"Length at 50% maturity slot not specified in sheet '12. Fishery Data'"
  }else{
    OM@L50<-rep(Data@L50,2)
    if(is.na(Data@CV_L50)){
      asslist$CV_L50<-"Coefficient of variation in length at 50% maturity slot not specified in sheet '12. Fishery Data'A CV value of 0.1 was assumed."
      OM@cpars$L50 <- trlnorm(nsim,Data@L50,0.1)
    }else{
      OM@cpars$L50 <- trlnorm(nsim,Data@L50,Data@CV_L50)
    }
  }

  if(is.na(Data@vbLinf)){
    errlist$vbLinf<-"'von Bertalanffy Linf parameter' not specified in sheet '12. Fishery Data'"
  }else{
    OM@Linf<-rep(Data@vbLinf,2)
    if(is.na(Data@CV_vbLinf)){
      asslist$CV_vbLinf<-"Coefficient of variation in asymptotic length 'CV von B. Linf parameter'not specified in sheet '12. Fishery Data'. A CV value of 0.05 was assumed"
      OM@cpars$Linf <- trlnorm(nsim,Data@vbLinf,0.05)
    }else{
      OM@cpars$Linf <- trlnorm(nsim,Data@vbLinf,Data@CV_vbLinf)
    }
  }

  if(is.na(Data@vbK)){
    errlist$vbK<-"'von Bertalanffy K parameter' not specified in sheet '12. Fishery Data'"
  }else{
    OM@K<-rep(Data@vbK,2)
    if(is.na(Data@CV_vbK)){
      asslist$CV_vbK<-"Coefficient of variation in asymptotic length 'CV von B. K parameter' not specified in sheet '12. Fishery Data'. A CV value of 0.1 was assumed."
      OM@cpars$K <- trlnorm(nsim,Data@vbLinf,0.1)
    }else{
      OM@cpars$K <- trlnorm(nsim,Data@vbK,Data@CV_vbK)
    }
  }

  if(is.na(Data@vbt0)){
    asslist$t0<-"Theoretical age at length zero 'von B. t0 parameter' not specified in sheet '12. Fishery Data'. A t0 value of 0 was assumed."
  }else{
    if(Data@vbt0>0)asslist$t0<-("Theoretical age at length zero 'von B. t0 parameter' is positive in sheet '12. Fishery Data. This generally means the growth model does not adequately describe mean length-at-age.'")
    OM@t0<-rep(Data@vbt0,2)
    if(is.na(Data@CV_vbt0)){
      asslist$CV_vbt0<-"Coefficient of variation in theoretical age at length zero 'CV von B. t0 parameter' not specified in sheet '12. Fishery Data'. A CV value of 0.1 was assumed."
      OM@cpars$t0 <- (-trlnorm(nsim,-Data@vbt0,0.1))
    }else{
      OM@cpars$t0 <- (-trlnorm(nsim,-Data@vbt0,Data@CV_vbt0))
    }
    #OM@cpars$t0[OM@cpars$t0>0]<-0
  }

  if(is.na(Data@MaxAge)){
    errlist$maxage<-"Maximum age slot not specified in sheet '12. Fishery Data'"
  }else{
    OM@maxage<-Data@MaxAge
  }
  OM@maxage<-min(OM@maxage,plusgroup)

  # --- Stock depletion -----------------------------

  if (is.na(Data@Dep)) {
    asslist$Dep<-"Depletion slot 'Current stock depletion' not specified in sheet '12. Fishery Data'. A wide range of stock depletion has been assumed (1% to 50% of unfished spawning stock biomass)."
    OM@cpars$D<-runif(nsim,0.01,0.5)
  }else{
    OM@D<-rep(Data@Dep,2)
    if(is.na(Data@CV_Dep)){
      asslist$CV_Dep<-"Coefficient of variation in current spawning stock biomass relative to unfished 'CV current stock depletion' not specified in sheet '12. Fishery Data'. A CV value of 0.25 has been assumed."
      OM@cpars$D <- trlnorm(nsim,Data@Dep,0.25)
    }else{
      OM@cpars$D <- trlnorm(nsim,Data@Dep,Data@CV_Dep)
    }
  }

  # --- Recruitment --------------------------------

  if (is.na(Data@steep)) {
    asslist$steep<-"Resilience slot 'Steepness' not specified in sheet '12. Fishery Data'. Values in the range 0.5-0.9 were assumed."
    OM@cpars$h<-runif(nsim,0.5,0.9)
  }else{
    OM@h<-rep(Data@steep,2)
    if(is.na(Data@CV_steep)){
      asslist$CV_steep<- "Coefficient of variation in resilience 'CV Steepness' not specified in sheet '12. Fishery Data'. A CV value of 0.15 has been assumed."
      OM@cpars$h <- sample_steepness2(nsim,Data@steep,0.15)
    }else{
      OM@cpars$h <- sample_steepness2(nsim,Data@steep,Data@CV_steep)
    }
  }

  if (is.na(Data@sigmaR)) {                                             # 14
    asslist$sigmaR<-"Recruitment variability slot 'sigmaR' not specified in sheet '12. Fishery Data'. Values in the range 0.3-0.9 were assumed."
    OM@Perr<-c(0.3,0.9)
    OM@cpars$Perr<-runif(nsim,0.3,0.9)
  }else{
    OM@Perr<-rep(Data@sigmaR,2)
    OM@cpars$Perr<-rep(Data@sigmaR,nsim)
  }

  if (any(is.na(Data@Effort))) {
    errlist$Effort<-"Missing values in Effort specified in sheet '12. Fishery Data'. Effort values (relative or absolute) required for all years."
  }

  trends<-matrix(Data@Effort,nrow=nsim,ncol=Nyears,byrow=T)
  trends<-trends/apply(trends,1,mean, na.rm=TRUE)
  nt<-dim(trends)[1]
  Esdmins<-c(0,0.2,0.5)
  Esdmaxes<-c(0.2,0.5,0.8)
  ind<- openMSE.Qs$Score[1]
  Esdrand<-runif(nsim,Esdmins[ind],Esdmaxes[ind]) #runif(nsim,Esd_min,Esd_max)
  Emu<-(-0.5*Esdrand^2)
  Esdarray<-array(exp(rnorm(nsim*Nyears,Emu,Esdrand)),c(nsim,Nyears))

  qmins<-c(-1,1,2,-2,-3)
  qmaxes<-c(1,2,3,-1,-2)
  ind<- openMSE.Qs$Score[2]
  qhssim<-runif(nsim,qmins[ind],qmaxes[ind]) #(nsim,qhs[1],qhs[2])
  qssim<-1+qhssim/100                                                   # F7 ----------
  trendsamp<-ceiling(runif(nsim)*nt)

  Find<-array(NA,c(nsim,Nyears))
  for(i in 1:nsim)Find[i,]<-trends[trendsamp[i],]*Esdarray[i,]* qssim[i]^((1:Nyears)-(Nyears/2))
  OM@cpars$Find<-Find

  # --- Future catchability ----------

  OM@qinc<-rep(0,2)                                                    # F8 ----------

  # --- Selectivity -----------------------

  if (is.na(Data@LFC)) {
    errlist$steep<-"Length at first capture not specified in sheet '12. Fishery Data'"
  }else{
    OM@L5<-rep(Data@LFC,2)
    if(is.na(Data@CV_LFC)){
      asslist$CV_LFC<- "Coefficient of variation in length at first capture not specified in sheet '12. Fishery Data'. A CV value of 0.15 was assumed."
      OM@cpars$L5 <- trlnorm(nsim,Data@LFC,0.15)
    }else{
      OM@cpars$L5 <- trlnorm(nsim,Data@LFC,Data@CV_LFC)
    }
  }
  if (is.na(Data@LFS) & !is.na(Data@LFC)) {
    asslist$LFS<-"Length at full selection not specified in sheet '12. Fishery Data. Assuming knife-edge selectivity at Data@LFC'"
    Data@LFS <- Data@LFC * 1.05
  } else if (is.na(Data@LFS) & is.na(Data@LFC)) {
    errlist$LFS<-"Length at full selection not specified in sheet '12. Fishery Data'"
  }else{
    OM@LFS<-rep(Data@LFS,2)
    if(is.na(Data@CV_LFS)){
      asslist$CV_LFS<- "Coefficient of variation in length at full selection not specified in sheet '12. Fishery Data'. A CV value of 0.15 was assumed."
      OM@cpars$LFS <- trlnorm(nsim,Data@LFS,0.15)
    }else{
      OM@cpars$LFS <- trlnorm(nsim,Data@LFS,Data@CV_LFS)
    }
  }

  if (is.na(Data@Vmaxlen)) {
    asslist$Vmaxlen<-"Vulnerability at asymptotic length not specified in sheet '12. Fishery Data'. A 'flat-topped' logistic selectivity was assumed. "
    OM@Vmaxlen<-rep(1,2)
    OM@cpars$Vmaxlen<-rep(1,nsim)
  }else{
    OM@Vmaxlen<-rep(Data@Vmaxlen,2)
    OM@cpars$Vmaxlen<-rep(Data@Vmaxlen,nsim)
  }


  # --- Discarding ------------------------

  ind <- as.numeric(stringr::str_extract_all(openMSE.Qs$Score[3], "[0-9]+")[[1]])
  DRmin<-c(0,0.05,0.25,0.5,0.75,0.95)
  DRmax<-c(0.05,0.25,0.5,0.75,0.95,1)
  OM@DR<-c(DRmin[ind],DRmax[ind])
  OM@cpars$DR<-runif(nsim,DRmin[ind],DRmax[ind])

  ind <- as.numeric(stringr::str_extract_all(openMSE.Qs$Score[4], "[0-9]+")[[1]])
  OM@Fdisc<-c(DRmin[ind],DRmax[ind])
  OM@cpars$Fdisc<-runif(nsim,DRmin[ind],DRmax[ind])


  # --- MPAs ------------------------------

  minA1 <- c(0,0,0.05,0.1,0.2)
  maxA1 <- c(0,0.05,0.1,0.2,0.5)
  minA <- c(0,0,0.05,0.1,0.2,0.3,0.4)
  maxA <- c(0,0.05,0.1,0.2,0.3,0.4,0.5)
  minmix<-c(0, 0.01, 0.05, 0.1, 0.2)
  maxmix<-c(0.01, 0.05, 0.1, 0.2, 0.5)

  ind <- which(FPI.Inputs[,3] == "MPAs and Sanctuaries")
  MPAq <- as.integer(FPI.Inputs[ind,6])
  Ahrng<-c(minA1[MPAq],maxA1[MPAq])
  Ahrng[Ahrng<=0] <- 0.001

  ind <- as.numeric(stringr::str_extract_all(openMSE.Qs$Score[5], "[0-9]+")[[1]])
  Vhrng<-c(minmix[ind],maxmix[ind])

  ind <- as.numeric(stringr::str_extract_all(openMSE.Qs$Score[6], "[0-9]+")[[1]])
  Arng<-c(minA[ind],maxA[ind])
  Arng[Arng<=0] <- 0.001

  ind <- as.numeric(stringr::str_extract_all(openMSE.Qs$Score[7], "[0-9]+")[[1]])
  Vrng<-c(minmix[ind],maxmix[ind])

  nareas<-3

  Ahsim<-runif(nsim,Ahrng[1],Ahrng[2])
  Vhsim<-runif(nsim,Vhrng[1],Vhrng[2])
  Asim<-runif(nsim,Arng[1],Arng[2])
  Vsim<-runif(nsim,Vrng[1],Vrng[2])

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

  mov<-array(NA,c(nsim, OM@maxage+1, nareas, nareas, Nyears+OM@proyears))
  for(i in 1:nsim)mov[i,,,,]<-array(rep(makemov(fracs=Asize[i,], prob=probs[i,]),each=OM@maxage+1),
                                    c(OM@maxage+1,nareas,nareas,Nyears+OM@proyears))
  OM@cpars$mov<-mov
  # potential future MPA - open unless closed by MP
  OM@cpars$MPA<-matrix(1,nrow=OM@nyears+OM@proyears,ncol=3)
  # existing MPA - default stays closed
  OM@cpars$MPA[,3]<-0

  OM@cpars$Asize <- Asize

  OM <<- OM

  # ! Initial depletion defaults to unfished !
  asslist$InitD <- "Initial historical depletion was assumed to be 1 (starting from unfished conditions)."

  # ---- Management parameters -----------------------------------------------------------------------------------------------

  # MP type feasible
  # M1in <- as.numeric(stringr::str_extract_all(openMSE.Qs$Score[9], "[0-9]+")[[1]])
  # TAC, TAE, Size, Time-area
  asslist$Manage_bias<-"TACs, TAEs and size limits are assumed to be taken without consistent overrages or underages."
  OM@TACFrac <- OM@TAEFrac <- OM@SizeLimFrac <- rep(1,2)

  Vmin <- c(0,    0.01, 0.05, 0.1, 0.2)
  Vmax <- c(0.01, 0.05, 0.1,  0.2, 0.4)
  ind <- which(FPI.Inputs[,3] == "Enforcement Capability")
  Enforce <- as.integer(FPI.Inputs[ind,6])
  OM@TACSD <- OM@TAESD <- OM@SizeLimSD <- c(Vmin[Enforce],Vmax[Enforce])


  # ---- Data parameters -----------------------------------------------------------------------------------------------------

  CobsB <- as.numeric(stringr::str_extract_all(openMSE.Qs$Score[9], "[0-9]+")[[1]])
  Bmin<-c(0.5, 0.7, 0.9,0.95, 1)   # for TAC and TAE
  Bmax<-c(0.7, 0.9, 1,  1.05, 1.1) # for TAC and TAE
  OM@cpars$Cbias<-runif(nsim, Bmin[CobsB],Bmax[CobsB])

  betamin<-c(2,1.25,0.8,0.5,0.33)
  betamax<-c(3,2,1.25,0.8,0.5)
  D3in <- as.numeric(stringr::str_extract_all(openMSE.Qs$Score[9], "[0-9]+")[[1]])
  OM@beta<-c(betamin[D3in],betamax[D3in])                                          # D3 -----------


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


  if(length(errlist)!=0){
    shinyalert(title="The FPI information was imported but the operating model could not be generated:", text=paste(unlist(errlist),collapse="\n\n"), type="error")
    AM("--- OM Build Errors ----------------")
    AM(paste(unlist(errlist),collapse="\n"))
    OM<-NULL
  }else{
    if(length(asslist)!=0){
      # shinyalert(title="When importing the FPAT spreadsheet the following assumptions were made:",
                 # text=paste(unlist(asslist),collapse="\n\n"), type="info", size='l')
      shinyalert(title="Note:",
                 text='The uploaded FPAT spreadsheet is missing some information needed to create the operating model. Assumed values were used. These assumptions are listed in the operating model report.',
                 type="info", size='s')
      AM("--- Loading assumptions ----------------")
      AM(paste(unlist(asslist),collapse="\n"))

      AM("=== FPI+ input file loaded correctly =========================")

    }
    OM@cpars$control=list(progress=T,ntrials=1000,fracD=0.2)
  }
  OM@Misc$asslist <- asslist
  OM <<- OM
  OM

} # end of loaded OM





