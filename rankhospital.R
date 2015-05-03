rankhospital <- function(state, outcome,num="best") {
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

  subData <- subData[complete.cases(subData[,oString]),]
  orderVec <- order(subData[,oString],subData[,"Hospital.Name"])
  index<-1
  if(num=="worst")
  {
    index<-length(subData[,1]);
  }
  else if (num!="best")
  {
    index<-as.numeric(num)
  }
  subData[orderVec[index],"Hospital.Name"]
  #subData[orderVec,]
}