rankall <- function(outcome, num = "best") {

  ## Read outcome data
  outcomeData<- read.csv("outcome-of-care-measures.csv")
  
  ## Extract stateList
  stateList<- outcomeData["State"]
  stateList<- unique(stateList)
  stateList<- sapply(stateList,identical)
  stateList<- as.character(stateList)
  
  outcomeList<- c("heart attack","heart failure", "pneumonia")
  
  ##Verify if outcome is valid
  flag<- FALSE
  for(o in outcomeList)
  {
    if ( o == outcome)
    {flag<- TRUE
     break
    }
    else
      next
    
  }
  if(flag == FALSE)
    stop ('invalid outcome')
  
  result<-data.frame(hospital=character(), state=character())
  
  
  ##Now get best ranked hospital for each state
  resultctr<- 1
  
  for(st in stateList)
  {
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
    hospinfo<- outcomeData[outcomeData$State == st,]
    hospinfo
    
    ##Logic to derive best hospital from state's hospitals info
    
    ##Sort hospinfo based on hospital and mortality rate params
    ##But before sorting, filter out reqd data
    
    if(outcome == "heart attack")
    {
      fhospinfo<- hospinfo[,c(2,11)]
    }
    
    if(outcome == "heart failure")
    {
      fhospinfo<- hospinfo[,c(2,17)] 
    }
    
    if(outcome == "pneumonia")
    {
      fhospinfo<- hospinfo[,c(2,23)] 
    }
    
    ##Rename column names to avoid confusion
    names(fhospinfo)<- c("name","death")
    
    ##Call helper which directly gives reqd hospital name
    hospname<- rankallhelper(fhospinfo,num)
    
    ##Now create a row and add to data frame
    row<- c(hospname,st)
    rbind(result,setNames(as.list(row), nm=names(result)))
    
  }
 # names(result)<- c("hospital","state")
  result
  
}

identical<- function(x){x}
