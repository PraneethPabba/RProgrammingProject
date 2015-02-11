#Answers for Quiz 1 11-20
#Author: Pabba Praneeth
#Dated: 8 Feb, 2015
#Please use answers for ref. Learn DONT copy. Pass the quiz like a man !! 

displayAnswers<- function(){
  
inputData<- read.csv("hw1_data.csv")  
  #Q11
print("Answer for q11")


#Q12
print("Answer for q12")

temp<- inputData[1:2,]
print(temp)

tapply(iris,Species,mean)
lapply(s, function(x) colMeans(x[,c("Sepal.Length","Petal.Length")]))

#Q13
print("Answer for q13")

temp<- nrow(inputData)
print(temp)

#Q14
print("Answer for Q14")
l<- nrow(inputData)
print(inputData[(l-1):l,])

#Q15
print("Answer for Q15")
print(inputData[47,"Ozone"])

#Q16
print("Answer for Q16")

column1<- inputData[,1]
bad<- is.na(column1)
naVector<- column1[bad]
print(length(naVector))


#Q17
print("Answer for Q17")
col1cleaned<- column1[!bad]
print(mean(col1cleaned))

#q18
print("Answer for Q18")
filter1<- inputData[inputData$Ozone > 31,]
filter2<- filter1[filter1$Temp >90,]
solars<- filter2["Solar.R"]
bad<- is.na(solars)
solarscleaned<- solars[!bad]
print(mean(solarscleaned))

#Q19
print("Answer for Q19")
monthset<- inputData[inputData$Month == 6,]
temps<- monthset["Temp"]
bad<- is.na(temps)
tempscleaned<- temps[!bad]
print(mean(tempscleaned))

#Q20
print("Answer for Q20")
monthset<- inputData[inputData$Month == 5,]
ozones<- monthset["Ozone"]
bad<- is.na(ozones)
ozonescleaned<- ozones[!bad]
print(max(ozonescleaned))

}




