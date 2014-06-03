processWB <- function(hhsdata,plans) {
  #read.xlsx("hhsStateData.xls",sheetIndex=1) -> hhsdata
  #plans <- read.csv("out",colClasses = "character")
  #plans[,5] <- as.numeric(plans[,5])
  #plans[complete.cases(plans),] -> plans
  #statePops <- read.csv("./statepop.csv")
  start <- 120
  end <- nrow(hhsdata)
  as.numeric.factor <- function(x) {(as.numeric(levels(x)))[x]}
  states = character()
  types = character()
  df <- data.frame()
  while (start < end) {
    states <- c(states,as.character(hhsdata[start,2]))
    types <- c(types,as.character(hhsdata[start+4,7]))
    df <- rbind(df,c(as.numeric.factor(hhsdata[start+4,15]),as.numeric.factor(hhsdata[start+5,15]),
                     as.numeric.factor(hhsdata[start+10,15]),as.numeric.factor(hhsdata[start+11,15]),
                     as.numeric.factor(hhsdata[start+16,15]),as.numeric.factor(hhsdata[start+17,15]),
                     as.numeric.factor(hhsdata[start+18,15]),as.numeric.factor(hhsdata[start+19,15]),
                     as.numeric.factor(hhsdata[start+20,15]),
                     as.numeric.factor(hhsdata[start+10,8]),as.numeric.factor(hhsdata[start+11,8]),
                     as.numeric.factor(hhsdata[start+16,8]),as.numeric.factor(hhsdata[start+17,8]),
                     as.numeric.factor(hhsdata[start+18,8]),as.numeric.factor(hhsdata[start+19,8]),
                     as.numeric.factor(hhsdata[start+20,8]),as.numeric.factor(hhsdata[start+21,8]),
                     as.numeric.factor(hhsdata[start+22,8])
                     ))
    start <- start + 38
  }
  df <- cbind(df,types)
  df <- cbind(df,states)
  c(state.abb[1:8],"DC","FFM",state.abb[9:50]) -> statesAbb
  df <- df[order(df$state),]
  df <- cbind(df,statesAbb)

  names(df) <-  c("numplans","chgmedicaid","subsidy","nosubsidy","bronze","silver","gold","platinum","catastrophic",
                  "male","female","age<18","age 18-25","age 26-34","age 35-44","age 45-54","age 55-64",
                  "age > 65","exchangetype","state","stateabb")
  sapply(as.character(df[,"stateabb"]),function(x) {length(stateCarriers[[x]])} ) -> stateCarrierNums
  stateCarrierNums[stateCarrierNums == 0] <- NA
  df <- cbind(df,stateCarrierNums)
  df[,"nosubsidy"]/(df[,"subsidy"] + df[,"nosubsidy"]) -> df$nosubFraction
  df <- cbind(df,statePops)
  df
}