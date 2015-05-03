best <- function(state, outcome) {
  data <- read.csv("outcome-of-care-measures.csv",colClasses="character")
  outcome <- gsub(" ",".",outcome)
  names(data)[-1:-10] <- tolower(names(data)[-1:-10])
  
  oString <- tolower(paste("Hospital.30.Day.Death..Mortality..Rates.from",outcome,sep="."))

  stateVec <- data$State
  findState = which(stateVec==state)
  if (length(findState)==0)
  {
    stop("invalid state")
  }
  if (length((which(names(data)==oString)))==0)
  {
    stop("invalid outcome")
  }
  subData <- data[data$State==state,]
  options(warn=-1)
  subData[,oString]<-as.numeric(subData[,oString])
  options(warn=0)
  orderVec <- order(subData[,oString],subData[,"Hospital.Name"])
  subData[orderVec[1],"Hospital.Name"]
 # subData[orderVec,]
}