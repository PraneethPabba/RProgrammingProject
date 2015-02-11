best <- function(state, outcome) {
  ## Read outcome data
  outcomeData<- read.csv("outcome-of-care-measures.csv")
  
  ## Check that state and outcome are valid
  stateList<- outcomeData["State"]
  stateList<- unique(stateList)
  stateList<- sapply(stateList,identical)
  stateList<- as.character(stateList)
  
  
  flag<- FALSE
  for(st in stateList)
  {
    if ( st == state)
    {flag<- TRUE
      break
    }
    else
      next
    
  }
  if(flag == FALSE)
    stop ('invalid state')
  
  outcomeList<- c("heart attack","heart failure", "pneumonia")
  
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
  
   
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  hospinfo<- outcomeData[outcomeData$State == state,]
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
  
    
  #This is important cleaning step. The data in % was in factor datatype. First convert it to character, then to numeric
 
  fhospinfo[,2]<- sapply(fhospinfo[,2],as.character)
  fhospinfo[,2]<- sapply(fhospinfo[,2],as.numeric)
  
  #Step to remove NA related data
  fhospinfo<- fhospinfo[complete.cases(fhospinfo),]

  
  #Now sort data on both parameters
  #IMP NOTE: Need to sort with death rate first, then with name
  #because death rate is primary sort criteria. Alphabetical sort only used in ties
  shospinfo<- fhospinfo[order(fhospinfo$death,fhospinfo$name),]
  Hospital.Name<- as.character(shospinfo[1,1])
  Hospital.Name
        
  
  
}

identical<- function(x){x}
