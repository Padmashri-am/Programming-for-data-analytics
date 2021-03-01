#Clears the environment
rm(list=ls())

#Reading the file from Google Drive

#installing the googledrive package
install.packages('googledrive')
library(googledrive)

id <- "18Y3j9IDSuNhjuFpGpZ5XXTrFPs-eg8Cd"   #Assigning id of the file uploaded on drive

#Reading the fle from google drive
AdmissionData <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download",id)) 

#Checking summary and structure of the data
summary(AdmissionData)
str(AdmissionData)

#Checking for Null/Na values
colSums(is.na(AdmissionData))
colSums(AdmissionData=='')



############# HYPOTHESIS 1 #######################



#Checking for correlation between variables
cor(AdmissionData[,c(2:9)]) 

#Checking multicolinearity

#Install 'mctest' package
install.packages('mctest')
library(mctest)

#Creating data frame of predictor variables
PredictorAD <- AdmissionData[,c(2:8)]

#Checking for collinearity between predictor variables 
omcdiag(PredictorAD, AdmissionData$Chance.of.Admit) 

#Find which variables are non significant
imcdiag(PredictorAD, AdmissionData$Chance.of.Admit)

#Removing SOP and University rating
PredictorADMod <- AdmissionData[,c(-1,-4,-5,-9)]

#Checking for collinearity between predictor variables
omcdiag(PredictorADMod, AdmissionData$Chance.of.Admit)

#Checking for non significant varibales again
imcdiag(PredictorADMod, AdmissionData$Chance.of.Admit)



############# HYPOTHESIS 2 #######################


# test whether the chance of admission is dependant on research
t.test(Chance.of.Admit ~ Research, mu = 0, alt = "two.sided", conf  = 0.95, data=AdmissionData)

#Factoring variables
AdmissionData$University.Rating <- as.factor(AdmissionData$University.Rating)
AdmissionData$Research <- as.factor(AdmissionData$Research)

#Installing ggplot library
install.packages('ggplot2')
library(ggplot2)

#Test with ggplot for hypothesis 2
ggplot(AdmissionData, aes(x=Research, y = Chance.of.Admit))+ geom_boxplot(outlier.colour = "red")



############# HYPOTHESIS 3 #######################



#Checking number of records having Chance of Admit above a limit
table(AdmissionData$Chance.of.Admit > 0.5)
table(AdmissionData$Chance.of.Admit > 0.7)
table(AdmissionData$Chance.of.Admit >= 0.73)

#Transforming Chance of Admit for Logistic regression and storing it as binary variable
AdmissionData$Chance.of.Admit.Binary = as.factor(ifelse(AdmissionData$Chance.of.Admit >= 0.73,1,0))

#Removing Chance of Admit and Serial No. as they are not predictor variables
AdmissionData$Chance.of.Admit = NULL
AdmissionData$Serial.No. = NULL

#Splitting the data set
set.seed(1000)
index= sample(1:nrow(AdmissionData), 0.7*nrow(AdmissionData)) #creating index
train = AdmissionData[index,] #Creating train set with 70% records
test = AdmissionData[-index,] #Creating test set with 30% records


#Performing logistic regression and creating model of train set
model1 = glm(Chance.of.Admit.Binary ~ ., data = train, family = binomial)
summary(model1)

#Using the predict function to calculate chance of admit on test set
Outcome = predict(model1, test, type = "response")

#Converting prediction to binary based on the same condition (>=0.73 is 1 and <0.73 is 0)
OutcomeBinary <- ifelse(Outcome >= 0.73, 1, 0)

#Checking for accuracy
table(test$Chance.of.Admit.Binary, OutcomeBinary)
