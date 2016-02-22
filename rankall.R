rankall <- function(condition, num="best") {
  
colNum = 0
  colName = ""
  stateList = c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")
  if(state %in% stateList){
    if (condition=="heart attack") {
      colNum = 11
      colName = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    } else if (condition=="heart failure") {
      colNum = 17
      colName = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    } else if (condition=="pneumonia") {
      colNum = 23
      colName = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    } else {
      
      return(paste(c('Error in best(',state,",",condition,") : invalid outcome"), sep=" "))
    }
    
  } else {
    
    return(paste(c('Error in best(',state,",",condition,") : invalid state"), sep=" "))
  }
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcome[, colNum] <- as.numeric(outcome[, colNum])
  hospitals <- read.csv("hospital-data.csv", colClasses = "character")  
  combined <- merge(outcome, hospitals, "Provider.Number")
  hospitalOutcomes <- data.frame(combined$Hospital.Name.x, combined$State.x, combined[colName])
  colnames(hospitalOutcomes) = c("Hospital", "State", "Outcome")
  hospitalOutcomes <- subset(hospitalOutcomes, State==state & !is.na(Outcome))
  hospitalOutcomes <- hospitalOutcomes[order(hospitalOutcomes$Outcome, hospitalOutcomes$Hospital),]
  possibleOutcomes <-  nrow(hospitalOutcomes)
  
  rankRequest = 0
  if (num=="best") {
    rankRequest = 1
  } else if (num=="worst") {
    rankRequest = possibleOutcomes
  } else {
    if (num>possibleOutcomes | num<1) {
      return (NA)
    } else {
      rankRequest = num
    }
  }
  
  
  hospitalOutcomes[rankRequest,]
  
  
  
  
  
  
  
}