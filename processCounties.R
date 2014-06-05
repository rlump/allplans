processCounties <- function(counties) {
  #counties <- read.csv("./CO-EST2013-Alldata.csv")
  
  # remove bad county dona ana, nm
  counties <- counties[-1835,]
  
  # get state.abb in tables  
  counties[,7] <- toupper(as.character(counties[,7]))
  states <- counties[!duplicated(counties[,6],),][,6]
  states <- states[-which(as.character(states)=="District of Columbia")]
  stateabbmap <- data.frame(states,state.abb)
  counties <- cbind(counties,stateabbmap[match(counties[,6],stateabbmap[,1]),])
  
  #
  getPopulousCounties <- function(cty) {
    statepop <- cty[toupper(cty$STNAME) == cty$CTYNAME,][,8]
    cty <- cty[toupper(cty$STNAME) != cty$CTYNAME,]
    cty[order(cty$CENSUS2010POP,decreasing = TRUE),] -> cty
    #meanpop <- mean(cty[,cty$CENSUS2010POP])
    i <- 1
    while (sum(cty[1:i,8]) < statepop*0.8) {
      i <- i + 1
    }
    #cty[1:i,(c(6,7,8))]
    cty[,c(6,7,8)]
  }
  
  by(counties,counties$state.abb,getPopulousCounties)
  
  
}