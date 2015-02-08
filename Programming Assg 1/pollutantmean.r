pollutantmean<- function(directory, pollutant, id=1:332)
{
  directoryPath<- paste(sep="","./",directory)
  directoryPath
  filelist<- list.files(path=directoryPath)
  totalfiledata<- data.frame()
  for(i in id){
    
    filename<- filelist[i]
    filepath<- paste(sep="",directoryPath,"/",filename)
    filedata<- read.csv(filepath)
    
    #Append filedata to totalfiledata
    totalfiledata<- rbind(totalfiledata, filedata)
    
  }
  coldata<- numeric()
  if(pollutant == "sulfate")
    coldata<- totalfiledata[,2]
  if(pollutant == "nitrate")
    coldata<- totalfiledata[,3]
  bad<- is.na(coldata)
  coldatacleaned<- coldata[!bad]
  mean(coldatacleaned)
  
}