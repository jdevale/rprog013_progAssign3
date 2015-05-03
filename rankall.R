rankall <- function(outcome,num="best") {
  data <- read.csv("outcome-of-care-measures.csv",colClasses="character")
  origOutcome <-outcome
  outcome <- gsub(" ",".",outcome)
  names(data)[-1:-10] <- tolower(names(data)[-1:-10])
  
  oString <- tolower(paste("Hospital.30.Day.Death..Mortality..Rates.from",outcome,sep="."))
  
  if (length((which(names(data)==oString)))==0)
  {
    stop("invalid outcome")
  }

  
  options(warn=-1)
  data[,oString]<-as.numeric(data[,oString])
  options(warn=0)
  
  data <- data[complete.cases(data[,oString]),]
  orderVec <- order(data[,oString],data[,"Hospital.Name"])
  sorteddata <-data[orderVec,]
  #now the data should be ordered by outcome and name
  
  #split based on state
  splitList <- split(sorteddata,sorteddata$State)
  
  #get the index since split changes the column 
  #should have done this to begin with, but didn't
  cIndex=data.frame(cbind(11,17,23))
  names(cIndex) = c("heart attack","heart failure","pneumonia")
  myIndex=as.numeric(cIndex[,origOutcome])
  
  #create an empty data frame
  mylist <- data.frame()

  #get the split names -- these are the state 2 char IDs
  nameList<-names(splitList)
  
  #loop through all the splits
  #I tried LAPPLY but couldn't gete it right
  for (i in 1:length(splitList))
  {
    test<-as.data.frame(splitList[i])
    
    #get the index comes here because worst potentially changes for each state
    index<-1
    if(num=="worst")
    {
      index<-length(test[,1]);
    }
    else if (num!="best")
    {
      index<-as.numeric(num)
    }
    #this is the one line that actually matters  the rest is here because of
    #the ludicrous program spec
    #append the data onto the frame
    mylist <- rbind(mylist,cbind(test[index,2],nameList[i]))
  }
  #name the rows
  row.names(mylist)<-nameList
  #name the columnds
  names(mylist)=c("hospital","state")
  return(mylist)
  #subData[orderVec,]
}