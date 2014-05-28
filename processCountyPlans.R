
# processCounties(counties) -> populousCounties

processCountyPlans <- function(ctyplans) {
  populousCounties[[ctyplans[1,"State"]]] -> popCounties
  ctybronze <- ctyplans[which(ctyplans$Metal.Level=="Bronze" & !is.na(charmatch(ctyplans[,2],popCounties$CTYNAME))),]
  if (nrow(ctybronze) != 0) {
  #ctybronze <- ctyplans[which(ctyplans$Metal.Level=="Bronze"),]
  ctybronze[order(ctybronze$Premium.Adult.Individual.Age.21),] -> ctybronze
  ctybrnzprc <- ctybronze[,"Premium.Adult.Individual.Age.21"]
  ctybrnznumplans <- length(ctybronze[,"Premium.Adult.Individual.Age.21"])
  numCarriers <- length(unique(ctybronze[,"Issuer.Name"]))
  competitiveCutoff <- min(ctybrnzprc) + sd(ctybrnzprc)
  competitiveCarriers <- ctybronze[which(ctybronze$Premium.Adult.Individual.Age.21<competitiveCutoff),"Issuer.Name"]
  
  c(nrow(ctybronze),
    numCarriers,
    median(ctybrnzprc),
    sd(ctybrnzprc),
    max(ctybrnzprc),
    min(ctybrnzprc),
    ctybrnznumplans,
    ctybrnzprc,
    unique(competitiveCarriers)
  )
  }
  
}

processStatePlans <- function(stateplans) {
  
  by(stateplans,stateplans$County,processCountyPlans) -> cty
  lapply(cty, function(x) { if (!is.null(x[[1]])) x[[1]] } ) -> fff
  fff[!sapply(fff, is.null)]
}

summaryStatePlans <-function() {
  #plans <- read.csv("out",colClasses = "character")
  by(plans,plans$State,processStatePlans)
  
}