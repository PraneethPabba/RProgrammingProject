corr <- function(directory, threshold = 0) {
  #basic data
  directoryPath<- paste(sep="","./",directory)
  filelist<- list.files(path=directoryPath)
  
completeList<-complete('specdata')
corrdata<- numeric()

for(i in 1:332)
{
  if(completeList[i,2] <= threshold){
    next
  }
  
  #logic to read file
  filename<- filelist[i]
  filepath<- paste(sep="",directoryPath,"/",filename)
  filedata<- read.csv(filepath)
  
  #Logic to find correlation
  col1<- filedata[,2]
  col2<- filedata[,3]
  corr<- cor(col1,col2,use="complete.obs")
  corrdata<- c(corrdata,corr)
  
  
}

corrdata

}