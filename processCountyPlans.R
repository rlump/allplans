
# processCounties(counties) -> populousCounties

processCountyPlans <- function(ctyplans) {
  populousCounties[[ctyplans[1,"State"]]] -> popCounties
  ctybronze <- ctyplans[which(ctyplans$Metal.Level=="Bronze" & !is.na(charmatch(ctyplans[10,2],popCounties$CTYNAME))),]
  ctybronze[order(ctybronze$Premium.Adult.Individual.Age.21),] -> ctybronze
  ctybrnzprc <- ctybronze[,"Premium.Adult.Individual.Age.21"]
  ctybrnznumplans <- length(ctybronze[,"Premium.Adult.Individual.Age.21"])
  numCarriers <- length(unique(ctybronze[,"Issuer.Name"]))
  competitiveCutoff <- min(ctybrnzprc) + sd(ctybrnzprc)
  competitiveCarriers <- ctybronze[which(ctybronze$Premium.Adult.Individual.Age.21<competitiveCutoff),"Issuer.Name"]
  c(numCarriers,
    median(ctybrnzprc),
    sd(ctybrnzprc),
    max(ctybrnzprc),
    min(ctybrnzprc),
    ctybrnznumplans,
    ctybrnzprc,
    unique(competitiveCarriers)
  )
}

processStatePlans <- function(stateplans) {
  
  by(stateplans,stateplans$County,processCountyPlans) -> cty
  cty
}

summaryStatePlans <-function() {
  #plans <- read.csv("out",colClasses = "character")
  by(plans,plans$State,processStatePlans)
  
}