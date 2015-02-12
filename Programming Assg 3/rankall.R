rankall <- function(outcome, num = "best") {

  ## Read outcome data
  outcomeData<- read.csv("outcome-of-care-measures.csv")
  
  ## Extract stateList
  stateList<- outcomeData["State"]
  stateList<- unique(stateList)
  stateList<- stateList[order(stateList$State),]
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
  
  result<- data.frame()
  
  
  ##Now get best ranked hospital for each state

  hspnames<- numeric()
  
  for(st in stateList)
  {
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
    hospinfo<- outcomeData[outcomeData$State == st,]
    hospinfo
    
    #Logic to derive best hospital from state's hospitals info
    
    #Sort hospinfo based on hospital and mortality rate params
    #But before sorting, filter out reqd data
    
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
    
    #Rename column names to avoid confusion
    names(fhospinfo)<- c("name","death")
    
    #Call helper which directly gives reqd hospital name
    hospname<- rankallhelper(fhospinfo,num)
    
    hspnames<- c(hspnames,hospname)
    
  }

 
 hspnames<- as.data.frame(hspnames)
 names(hspnames)<- "hosptial"
  
 stateList<- as.data.frame(stateList)
 names(stateList)<- "state"
 
 result<- cbind(hspnames,stateList)
 result
}

identical<- function(x){x}
