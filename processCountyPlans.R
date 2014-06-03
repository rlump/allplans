# globals expected
# processCounties(counties) -> populousCounties
#processWB(hhsdata,plans) -> stateSummary

processCountyPlans <- function(ctyplans) {
  populousCounties[[ctyplans[1,"State"]]] -> popCounties
  ctybronze <- ctyplans[which(ctyplans$Metal.Level=="Bronze" & !is.na(charmatch(ctyplans[,2],popCounties$CTYNAME))),]
  ctysilver <- ctyplans[which(ctyplans$Metal.Level=="Silver" & !is.na(charmatch(ctyplans[,2],popCounties$CTYNAME))),]
  ctygold <- ctyplans[which(ctyplans$Metal.Level=="Gold" & !is.na(charmatch(ctyplans[,2],popCounties$CTYNAME))),]
  if (nrow(ctybronze) != 0) {
    ctybronze[order(ctybronze$Premium.Adult.Individual.Age.40),] -> ctybronze
    ctybronzeprc <- ctybronze[,"Premium.Adult.Individual.Age.40"]
    ctybronzenumplans <- length(ctybronze[,"Premium.Adult.Individual.Age.40"])
    numCarriers <- length(unique(ctybronze[,"Issuer.Name"]))
    competitiveCutoff <- min(ctybronzeprc) + sd(ctybronzeprc)
    competitiveCarriers <- ctybronze[which(ctybronze$Premium.Adult.Individual.Age.40<competitiveCutoff),"Issuer.Name"]
    
    ctysilver[order(ctysilver$Premium.Adult.Individual.Age.40),] -> ctysilver
    ctysilverprc <- ctysilver[,"Premium.Adult.Individual.Age.40"]
    ctysilvernumplans <- length(ctysilver[,"Premium.Adult.Individual.Age.40"])
    numCarriersSilver <- length(unique(ctysilver[,"Issuer.Name"]))
    competitiveCutoffsilver <- min(ctysilverprc) + sd(ctysilverprc)
    competitiveCarriersSilver <- ctysilver[which(ctysilver$Premium.Adult.Individual.Age.40<competitiveCutoffsilver),"Issuer.Name"]
    
    ctygold[order(ctygold$Premium.Adult.Individual.Age.40),] -> ctygold
    ctygoldprc <- ctygold[,"Premium.Adult.Individual.Age.40"]
    ctygoldnumplans <- length(ctygold[,"Premium.Adult.Individual.Age.40"])
    numCarriersgold <- length(unique(ctygold[,"Issuer.Name"]))
    competitiveCutoffgold <- min(ctygoldprc) + sd(ctygoldprc)
    competitiveCarriersgold <- ctygold[which(ctygold$Premium.Adult.Individual.Age.40<competitiveCutoffgold),"Issuer.Name"]
    
    list(#nrow(ctybronze),
      numCarriersBronze = numCarriers,
      numCarriersSilver = numCarriersSilver,
      countyPop = popCounties[charmatch(ctyplans[,2],popCounties$CTYNAME),"CENSUS2010POP"][1],
      medianctybronzeprc = median(ctybronzeprc),
      meanctybronzeprc = mean(ctybronzeprc),
      sdctybronzeprc = sd(ctybronzeprc),
      maxctybronzeprc = max(ctybronzeprc),
      minctybronzeprc = min(ctybronzeprc),
      ctybronzenumplans = ctybronzenumplans,
      ctybronzeprc = ctybronzeprc,
      uniqCarriers = unique(competitiveCarriers),
      
      medianctysilverprc = median(ctysilverprc),
      meanctysilverprc = mean(ctysilverprc),
      sdctysilverprc = sd(ctysilverprc),
      maxctysilverprc = max(ctysilverprc),
      minctysilverprc = min(ctysilverprc),
      ctysilvernumplans = ctysilvernumplans,
      ctysilverprc = ctysilverprc,
      uniqCarriersSilver = unique(competitiveCarriersSilver),
     
      medianctygoldprc = median(ctygoldprc),
      meanctygoldprc = mean(ctygoldprc),
      sdctygoldprc = sd(ctygoldprc),
      maxctygoldprc = max(ctygoldprc),
      minctygoldprc = min(ctygoldprc),
      ctygoldnumplans = ctygoldnumplans,
      ctygoldprc = ctygoldprc,
      uniqCarriersgold = unique(competitiveCarriersgold)
      
    )
  }
  
}

processStatePlans <- function(stateplans) {
  
  by(stateplans,stateplans$County,processCountyPlans) -> cty
  #lapply(cty, function(x) { if (!is.null(x[[1]])) x[[1]] } ) -> fff
  cty[!sapply(cty, is.null)] -> cty
  #processWB(hhsdata,plans) -> stateSummary
  #lapply(cty, summaryCtyPlans)

}

summaryStatePlans <-function() {
  #plans <- read.csv("out",colClasses = "character")
  by(plans,plans$State,processStatePlans)
  
}