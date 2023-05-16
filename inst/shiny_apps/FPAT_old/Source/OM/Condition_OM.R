
OMCond<-function(){

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
