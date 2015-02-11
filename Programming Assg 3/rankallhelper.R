rankallhelper<- function(fhospinfo,num){
  
  
  
  #This is important cleaning step. The data in % was in factor datatype. First convert it to character, then to numeric
  
  fhospinfo[,2]<- sapply(fhospinfo[,2],as.character)
  fhospinfo[,2]<- sapply(fhospinfo[,2],as.numeric)
  
  #Step to remove NA related data
  fhospinfo<- fhospinfo[complete.cases(fhospinfo),]
  
  
  #Now sort data on both parameters
  #IMP NOTE: Need to sort with death rate first, then with name
  #because death rate is primary sort criteria. Alphabetical sort only used in ties
  shospinfo<- fhospinfo[order(fhospinfo$death,fhospinfo$name),]
  
  shospinfo
  
  #num is a string. check all boundary cases first
  if(num == "best")
    num=1
  len<- nrow(shospinfo)
  if(num == "worst")
    num=len
  notApp<- NA
  #now convert num into number and check rest of the conditions
  num<- as.numeric(num)
  num
  
  if(num > len)
    return(notApp)
  else{
    Hospital.Name<- as.character(shospinfo[num,1])
    return(Hospital.Name)
  }
}