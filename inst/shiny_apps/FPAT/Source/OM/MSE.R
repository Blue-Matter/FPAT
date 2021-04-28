
runProj<-function(Info){
  withProgress(message="Running test of management options",value=0, {
    Info$MSEproj<-Project(Info$MSEhist,MPs=Info$MPsel, extended = T)
  })
}
