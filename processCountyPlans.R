# globals expected
# processCounties(counties) -> populousCounties
#processWB(hhsdata,plans) -> stateSummary
#counties <- read.csv("./CO-EST2013-Alldata.csv")

processCountyPlans <- function(ctyplans) {
 
  state = ctyplans[1,"State"]
  allCounties[[ctyplans[1,"State"]]] -> popCounties
  gsub(" COUNTY$","",popCounties[,"CTYNAME"]) -> popCounties[,"CTYNAME"]
  countyPop = popCounties[charmatch(ctyplans[,2],popCounties$CTYNAME),"CENSUS2010POP"][1]
  statePop = stateSummary[stateSummary$stateabb == state,"Pop"]
  bronzeFraction = stateSummary[stateSummary$stateabb == state,"bronze"]*countyPop/statePop
  silverFraction = stateSummary[stateSummary$stateabb == state,"silver"]*countyPop/statePop
  goldFraction = stateSummary[stateSummary$stateabb == state,"gold"]*countyPop/statePop
    
  
  ctybronze <- ctyplans[which(ctyplans$Metal.Level=="Bronze" & !is.na(charmatch(ctyplans[,2],popCounties$CTYNAME))),]
  ctysilver <- ctyplans[which(ctyplans$Metal.Level=="Silver" & !is.na(charmatch(ctyplans[,2],popCounties$CTYNAME))),]
  ctygold <- ctyplans[which(ctyplans$Metal.Level=="Gold" & !is.na(charmatch(ctyplans[,2],popCounties$CTYNAME))),]
  if (nrow(ctybronze) != 0) {
    
    
    ctybronze[order(ctybronze$Premium.Adult.Individual.Age.40),] -> ctybronze
    ctybronzeprc <- ctybronze[,"Premium.Adult.Individual.Age.40"]
    ctybronzenumplans <- length(ctybronze[,"Premium.Adult.Individual.Age.40"])
    numCarriers <- length(unique(ctybronze[,"Issuer.Name"]))
    competitiveCutoff <- min(ctybronzeprc) + ifelse(is.na(sd(ctybronzeprc)),0,sd(ctybronzeprc))
    competitiveCarriers <- ctybronze[which(ctybronze$Premium.Adult.Individual.Age.40<=competitiveCutoff),"Issuer.Name"]
  
    
    ctysilver[order(ctysilver$Premium.Adult.Individual.Age.40),] -> ctysilver
    ctysilverprc <- ctysilver[,"Premium.Adult.Individual.Age.40"]
    ctysilvernumplans <- length(ctysilver[,"Premium.Adult.Individual.Age.40"])
    numCarriersSilver <- length(unique(ctysilver[,"Issuer.Name"]))
    competitiveCutoffsilver <- min(ctysilverprc) + sd(ctysilverprc)
    competitiveCarriersSilver <- ctysilver[which(ctysilver$Premium.Adult.Individual.Age.40<=competitiveCutoffsilver),"Issuer.Name"]
    
    ctygold[order(ctygold$Premium.Adult.Individual.Age.40),] -> ctygold
    ctygoldprc <- ctygold[,"Premium.Adult.Individual.Age.40"]
    ctygoldnumplans <- length(ctygold[,"Premium.Adult.Individual.Age.40"])
    numCarriersgold <- length(unique(ctygold[,"Issuer.Name"]))
    competitiveCutoffgold <- min(ctygoldprc) + sd(ctygoldprc)
    competitiveCarriersgold <- ctygold[which(ctygold$Premium.Adult.Individual.Age.40<=competitiveCutoffgold),"Issuer.Name"]
    
    competitiveCarrier1 = as.character(NA)
    competitiveCarrier2 = as.character(NA) 
    competitiveCarrier3 = as.character(NA) 
    competitiveCarrier4 = as.character(NA)
    competitiveCarrier5 = as.character(NA)
    competitiveCarrier6 = as.character(NA) 
    
    i <- 1
    unique(competitiveCarriers) -> uniqCarriers
    if (length(uniqCarriers) > 6) {
      uniqCarriers <- uniqCarriers[1:6,]
    }
    for (cc in unique(competitiveCarriers)) {
      if (i ==1) {competitiveCarrier1 <- cc}
      if (i ==2) {competitiveCarrier2 <- cc}
      if (i ==3) {competitiveCarrier3 <- cc}
      if (i ==4) {competitiveCarrier4 <- cc}
      if (i ==5) {competitiveCarrier5 <- cc}
      if (i ==6) {competitiveCarrier6 <- cc}
      i <- i + 1
    }
    
    list(#nrow(ctybronze),
      #class =  class(ctyplans),
      state = state,
      bronzeFraction = bronzeFraction,
      numCarriersBronze = numCarriers,
      numCarriersSilver = numCarriersSilver,
      #competitiveCarriers = unique(competitiveCarriers),
      
      numCarriersgold = numCarriersgold,
      countyPop = popCounties[charmatch(ctyplans[,2],popCounties$CTYNAME),"CENSUS2010POP"][1],
      medianctybronzeprc = median(ctybronzeprc),
      meanctybronzeprc = mean(ctybronzeprc),
      sdctybronzeprc = sd(ctybronzeprc),
      maxctybronzeprc = max(ctybronzeprc),
      minctybronzeprc = min(ctybronzeprc),
      ctybronzenumplans = ctybronzenumplans,
      #ctybronzeprc = ctybronzeprc,
      competitiveBronze = length(unique(competitiveCarriers)),
      
      silverFraction = silverFraction,
      medianctysilverprc = median(ctysilverprc),
      meanctysilverprc = mean(ctysilverprc),
      sdctysilverprc = sd(ctysilverprc),
      maxctysilverprc = max(ctysilverprc),
      minctysilverprc = min(ctysilverprc),
      ctysilvernumplans = ctysilvernumplans,
      #ctysilverprc = ctysilverprc,
      competitiveSilver = length(unique(competitiveCarriersSilver)),
     
      goldFraction = goldFraction,
      medianctygoldprc = median(ctygoldprc),
      meanctygoldprc = mean(ctygoldprc),
      sdctygoldprc = sd(ctygoldprc),
      maxctygoldprc = max(ctygoldprc),
      minctygoldprc = min(ctygoldprc),
      ctygoldnumplans = ctygoldnumplans,
      #ctygoldprc = ctygoldprc,
      competitiveGold = length(unique(competitiveCarriersgold)),
      compCarrier1 = competitiveCarrier1,
      compCarrier2 = competitiveCarrier2,
      compCarrier3 = competitiveCarrier3,
      compCarrier4 = competitiveCarrier4,
      compCarrier5 = competitiveCarrier5,
      compCarrier6 = competitiveCarrier6
      
    )
  }
  
}

processStatePlans <- function(stateplans) {
  
  by(stateplans,stateplans$County,processCountyPlans) -> cty
  #lapply(cty, function(x) { if (!is.null(x[[1]])) x[[1]] } ) -> fff
  cty[!sapply(cty, is.null)] -> cty
  ldply (cty, data.frame) -> ctydf
  ctydf <- ctydf[is.na(ctydf$compCarrier3),]

}

marketSummaryStatePlans <-function(all = FALSE) {
  #plans <- read.csv("out",colClasses = "character")
  processCounties(counties,all) ->> allCounties
  by(plans,plans$State,processStatePlans) -> lll
  ldply (lll, data.frame) -> lll
  ddply(lll,.(state,compCarrier1,compCarrier2),summarize,
        marketSize = sum(bronzeFraction*meanctybronzeprc,na.rm = TRUE)
        + sum(silverFraction*meanctysilverprc,na.rm = TRUE) 
        + sum(goldFraction*meanctygoldprc,na.rm = TRUE),
        numPlans = sum(bronzeFraction) + sum(silverFraction) + sum(goldFraction)) -> mkt
        
  mkt[order(mkt$state,-mkt$marketSize),]
}

processAllStatePlans <- function(stateplans) {
  
  by(stateplans,stateplans$County,processCountyPlans) -> cty
  #lapply(cty, function(x) { if (!is.null(x[[1]])) x[[1]] } ) -> fff
  cty[!sapply(cty, is.null)] -> cty
  ldply (cty, data.frame) -> ctydf
  #ctydf <- ctydf[is.na(ctydf$compCarrier3),]
  
}

allStatePlansByCounty <-function() {
  #plans <- read.csv("out",colClasses = "character")
  processCounties(counties,TRUE) ->> allCounties
  by(plans,plans$State,processAllStatePlans) -> lll
  ldply (lll, data.frame)
}

summaryAllStatePlans <-function() {
  #plans <- read.csv("out",colClasses = "character")
  processCounties(counties,TRUE) ->> allCounties
  by(plans,plans$State,processAllStatePlans) -> lll
  ldply (lll, data.frame) -> lll
  ddply(lll,.(state),summarize,
        marketSize = sum(bronzeFraction*meanctybronzeprc,na.rm = TRUE)
        + sum(silverFraction*meanctysilverprc,na.rm = TRUE) 
        + sum(goldFraction*meanctygoldprc,na.rm = TRUE),
        numPlans = sum(bronzeFraction,na.rm = TRUE) + sum(silverFraction,na.rm = TRUE) + sum(goldFraction,na.rm = TRUE)) -> bests
  
  #mkt[order(mkt$state,-mkt$marketSize),]
  read.csv("./brokercomp.csv") -> brokerComps
  cbind(brokerComps,state.abb) -> brokerComps
  subset(brokerComps,select = c(ind,state.abb)) -> brokerComps
  
  names(brokerComps)[2] <- "state"
  merge(bests,brokerComps) ->bests
  bests[,"numPlans"]*bests[,"ind"] -> bests$relVal
  bests[order(-bests$relVal),]
  
}

processBestCarrierTargets <- function(df) {
  as.character(df[,"compCarrier1"])
  carriers <- c(as.character(df[,"compCarrier1"]),as.character(df[,"compCarrier2"]))
  
  carriers <- carriers[complete.cases(carriers)]
  length(carriers)
  uniqCarriers <- unique(carriers)
  length(uniqCarriers)
  #if (length(uniqCarriers)>=2) {
    uc <- matrix()
    if (length(uniqCarriers)>2) {
      combinations(length(uniqCarriers),2,uniqCarriers) -> uc
    } else if (length(uniqCarriers)==2) {
      uc = matrix(uniqCarriers,nrow=1)
    } else {
      uc = matrix(c(uniqCarriers,"NULL CARRIER"),nrow=1)
    }
    dfreturn = data.frame()
    ucMax <- character()
    max <- 0
    numPlans <- 0
    idMax <- numeric()
    for (i in 1:nrow(uc)) {
      localMax <- 0
      idLocal <- numeric(0)
      localNumPlans <- 0
      for (j in 1:nrow(df)) {
        if ( (df[j,"compCarrier1"] == uc[i,1] && is.na(df[j,"compCarrier2"])) ||
          (df[j,"compCarrier1"] == uc[i,2] && is.na(df[j,"compCarrier2"])) ||
          (df[j,"compCarrier1"] == uc[i,1] && df[j,"compCarrier2"] == uc[i,2]) ||
          (df[j,"compCarrier1"] == uc[i,2] && df[j,"compCarrier2"] == uc[i,1])) {
            localMax <- localMax + df[j,"marketSize"]
            localNumPlans <- localNumPlans + df[j,"numPlans"]
            idLocal <- c(idLocal,j)
        }
      }
      if (localMax > max) {
        max <- localMax
        numPlans <- localNumPlans
        idMax <- idLocal
        ucMax <- uc[i,]
      }
    }
    for (i in idMax) {
      dfreturn <- rbind(dfreturn,df[i,])
    }
    #dfreturn
    list(carrier1 = ucMax[1], carrier2 = ucMax[2], max = max, numPlans = numPlans) -> bests
    
  #} else {
   # 2
  #}
  
}

bestCarrierTargets <- function(mkt = NULL) {
  if (is.null(mkt)) {mkt <- marketSummaryStatePlans()}
  dlply(mkt,.(state),processBestCarrierTargets) -> bests
  ldply (bests, data.frame) -> bests
  read.csv("./brokercomp.csv") -> brokerComps
  cbind(brokerComps,state.abb) -> brokerComps
  subset(brokerComps,select = c(ind,state.abb)) -> brokerComps
  
  names(brokerComps)[2] <- "state"
  merge(bests,brokerComps) ->bests 
  bests[,"numPlans"]*bests[,"ind"] -> bests$relVal
  bests[order(-bests$relVal),]
}

