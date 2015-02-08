complete<- function(directory, id=1:332)
{
  directoryPath<- paste(sep="","./",directory)
  directoryPath
  filelist<- list.files(path=directoryPath)
  outputdata<- data.frame() 
  for(i in id){
    
    filename<- filelist[i]
    filepath<- paste(sep="",directoryPath,"/",filename)
    filedata<- read.csv(filepath)
    
    
    #Logic to find nobs
    coldata1<- filedata[,2]
    coldata2<- filedata[,3]
    good<- complete.cases(coldata1,coldata2)
    coldata1mod<- coldata1[good]
    nobs<- length(coldata1mod)
    
    
    outputvector<- c(i,nobs)
    outputdata<- rbind(outputdata,outputvector) 
  }
  #Change names of outputdata
  colnames(outputdata)<- c("id","nobs")
  outputdata
  
  
}