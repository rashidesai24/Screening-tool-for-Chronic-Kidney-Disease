# Loading the libraries
library(readxl) 
library(caTools) 
library(ROCR)
library(tidyverse)
library(caret)
library(openxlsx)

# Reading the dataset
dataset <- read_excel("/Users/rashidesai/Desktop/ChronicKidneyDisease.xlsx")
# setting the seed to a number so we train on same data everytime
set.seed(12345)
# Data Description
attach(dataset) 
str(dataset)

# creating empty category variable 
cat.ind <- c()
# factorizing the categorical variables 
n <- 0 
#converting num to factors 
for (i in 1:ncol(dataset)){
  dataset[[i]]=as.factor(dataset[[i]])
  if (length(levels(dataset[[i]]))>=5){
    dataset[[i]]=as.numeric(dataset[[i]])
  }
  if (is.factor(dataset[[i]])){
    n <- n + 1
    cat.ind[n] <- i
  }
}

#CKD has ‘NA’ which needs to be predicted -> Storing those rows seperately
# ‘grep’ function gives us the index of a column
test.ind=which(is.na(dataset$CKD)) 
data.test=dataset[test.ind,] 
p=grep("CKD",colnames(data.test))

# removing the CKD column in the test data 
data.test_without=data.test[,-p]

# removing all the rows that have N/A in any of the columns 
# This willl avoid injecting any artificial imputation in the test dataset
for (i in 1:ncol(data.test_without)){
  if (sum(is.na(data.test_without[[i]]))!=0){
    a=which(is.na(data.test_without[[i]]))
    data.test_without=data.test_without[-a,]
  }
}

# training dataset after removing the N/A rows in the target variable ‘CKD’ [removing 2819 records] 
b=grep("CKD",colnames(dataset)) 
dataset=dataset[-test.ind,]

# removing all the rows that have any missing values from the training data
for (i in 1:ncol(dataset)){ 
  if (sum(is.na(dataset[[i]]))!=0){ 
    a=which(is.na(dataset[[i]])) 
    dataset=dataset[-a,] 
  } 
}

table(dataset$CKD)

# seperating the training data into categorical variables and continuous variables 
cat.df <- dataset[,cat.ind] 
cat.num<-dataset[,-cat.ind]

# checking if there are any rows which are NA 
na_dataset=lapply(dataset,function(x) {length(which(is.na(x)))}) 
unlist(na_dataset)

str(dataset)
dim(chronic$CKD)

# Run initial model for VIF
model = glm(dataset$CKD ~ .,family = binomial, data = dataset)
summary(model)
library(car)
vif(model)

# splitting the dataset into train and test 
data.train=dataset[-test.ind,]
dim(data.train)

# Exploratory Data Analysis
cor(dataset$`Total Chol`,dataset$LDL) # 0.9285436
cor(dataset$`Total Chol`,dataset$HDL) # 0.1711896

# Total Chol is sum of HDL, LDL, and 20% of total glycerides
# Removed Total chol, because there is high correlation

# Checking for correlations
cor(dataset$SBP,dataset$DBP)

#checking correlation between Fam CVD and CVD
chi.FamCVD=table(dataset$`Fam CVD`,dataset$CVD) 
print(chi.FamCVD) 
print(chisq.test(chi.FamCVD)) 

#checking correlation between Fam Diabetes and Diabetes 
chi.Diabetes=table(dataset$Diabetes,dataset$`Fam Diabetes`) 
print(chi.Diabetes) 
print(chisq.test(chi.Diabetes)) 

#checking correlation between Hypertension and Fam Hypertension 
chi.Hypertension = table(Hypertension,`Fam Hypertension`) 
print(chi.Hypertension) 
print(chisq.test(chi.Hypertension)) 

#checking correlation between Female and CKD
chi.Female=table(dataset$Female,dataset$CKD) 
print(chi.Female) 
print(chisq.test(chi.Female)) 

#checking correlation between race and CKD 
chi.race=table(dataset$Racegrp,dataset$CKD) 
print(chi.race) 
print(chisq.test(chi.race)) 

#checking correlation between education and CKD
chi.edu=table(dataset$Educ,dataset$CKD) 
print(chi.edu) 
print(chisq.test(chi.edu)) 

#checking correlation between Unmarried and CKD 
chi.Unmarried=table(dataset$Unmarried,dataset$CKD) 
print(chi.Unmarried) 
print(chisq.test(chi.Unmarried))

#checking correlation between Income and CKD 
chi.Income=table(dataset$Income,dataset$CKD) 
print(chi.Income) 
print(chisq.test(chi.Income))

#checking correlation between caresource and CKD 
chi.care=table(dataset$CareSource,dataset$CKD) 
print(chi.care) 
print(chisq.test(chi.care)) 

#checking correlation between Insured and CKD 
chi.insurance=table(dataset$Insured,dataset$CKD) 
print(chi.insurance) 
print(chisq.test(chi.insurance))

#checking correlation between Obese and CKD 
chi.Obese=table(dataset$Obese,dataset$CKD) 
print(chi.Obese) 
print(chisq.test(chi.Obese)) 

#checking correlation between Dyslipidemia and CKD 
chi.Dyslipidemia=table(dataset$Dyslipidemia,dataset$CKD) 
print(chi.Dyslipidemia) 
print(chisq.test(chi.Dyslipidemia)) 

#checking correlation between PVD and CKD 
chi.pvd=table(dataset$PVD,dataset$CKD) 
print(chi.pvd) 
print(chisq.test(chi.pvd)) 

#checking correlation between activity and CKD 
chi.activity=table(dataset$Activity,dataset$CKD) 
print(chi.activity)
print(chisq.test(chi.activity))

#checking correlation between Poor Vision and CKD 
chi.vision=table(dataset$PoorVision,dataset$CKD) 
print(chi.vision)
print(chisq.test(chi.vision)) 

#checking correlation between smoker and CKD 
chi.smoker=table(dataset$Smoker,dataset$CKD) 
print(chi.smoker)
print(chisq.test(chi.smoker)) 

#checking correlation between CVD and CKD 
chi.CVD=table(dataset$CVD,dataset$CKD) 
print(chi.CVD)
print(chisq.test(chi.CVD)) 

#checking correlation between Stroke and CHF 
chi.CHF=table(dataset$CHF,dataset$CKD) 
print(chi.CHF)
print(chisq.test(chi.CHF)) 

#checking correlation between Stroke and CKD 
chi.stroke=table(dataset$Stroke,dataset$CKD) 
print(chi.stroke)
print(chisq.test(chi.stroke)) 

#checking correlation between Anemia and CKD 
chi.Diabetes=table(dataset$Diabetes,dataset$CKD) 
print(chi.Diabetes)
print(chisq.test(chi.Diabetes)) 

str(dataset)

# t-tests
t.test(dataset$SBP~dataset$CKD)
t.test(dataset$DBP~dataset$CKD)
t.test(dataset$HDL~dataset$CKD)
t.test(dataset$LDL~dataset$CKD)
t.test(dataset$`Total Chol`~dataset$CKD)
t.test(dataset$Age~dataset$CKD)
t.test(dataset$Weight~dataset$CKD)
t.test(dataset$Height~dataset$CKD)
t.test(dataset$BMI~dataset$CKD)
t.test(dataset$Waist~dataset$CKD)

# Removed the multi-collinearity among the predictors and found the variables that needs to be included in the final model.
# ID, Educ, Unmarried, Income, Insured, Weight, Height, BMI, Waist, Total Chol, Dyslipidemia, Poor Vision
# col index of the dataset which are not needed 

grep("Female",colnames(dataset))
grep("Educ",colnames(dataset))
grep("Unmarried",colnames(dataset))
grep("Income",colnames(dataset))
grep("Insured",colnames(dataset))
grep("Weight",colnames(dataset))
grep("Height",colnames(dataset))
grep("BMI",colnames(dataset))
grep("Waist",colnames(dataset))
grep("Total Chol",colnames(dataset))
grep("Dyslipidemia",colnames(dataset))
grep("PoorVision",colnames(dataset))
grep("Fam Hypertension",colnames(dataset))
grep("Fam Diabetes",colnames(dataset))
grep("Fam CVD",colnames(dataset))
grep("Anemia",colnames(dataset))
grep("CareSource",colnames(dataset))


col=c(7,5,6,10,11,12,14,19,20,23)
# col=c(1,5,6,7,9,10,11,12,19,20,23,26,28,31) 

dataset = dataset[,-col]

require(caTools) 
set.seed(123) 
sample <-sample.split(dataset, SplitRatio = 0.75) 
data.train1 <-subset(dataset, sample ==TRUE)
data.test1 <- subset(dataset, sample ==FALSE)

# MODEL 1
# keeps <- c("Age", "Racegrp", "HDL", "LDL", "PVD",  "Hypertension", "Diabetes", "CVD", "CHF", "CKD") 

# MODEL 2
# keeps <- c("Age", "Female", "Racegrp", "CareSource", "HDL", "LDL", "Activity", "Smoker", "PVD",  "Hypertension", "Diabetes", "CHF", "CVD") 

# FINAL MODEL
keeps <- c("Age", "Female", "Racegrp", "HDL", "LDL", "PVD", "Hypertension", "Diabetes", "CVD", "CKD") 

# keeps <- c("Age", "Racegrp", "CareSource", "HDL", "LDL", "PVD", "Activity", "Hypertension", "Diabetes", "Stroke", "CVD", "CHF", "CKD") 

data.train1 <- data.train1[keeps] 
data.test1 <- data.test1[keeps]
# Building the Logistic Regression model 
log.model = glm( data.train1$CKD ~ .,family = binomial, data = data.train1)
# summary of the logistric regression model 
summary(log.model)

# For the better interpretation, we are finding the odds ratio below 
exp(coefficients(log.model))

exp(cbind("Odds ratio" = coef(log.model), confint.default(log.model, level = 0.95)))

# Finally, predicting the results on the test data
# Using a threshold value to classify the target variable if a person has chronic kidney disease 
# We need to have less false negatives in the prediction
# The focus is on getting better recall for this reason

# Predicting the Test set results 
prob_pred = predict(log.model, type = 'response', newdata = data.test1) 
y_pred = ifelse(prob_pred > 0.1, 1, 0)

# prob_pred = predict(step.model, type = 'response', newdata = data.test1) 
# y_pred = ifelse(prob_pred > 0.1, 1, 0)

cm = table(data.test1$CKD, y_pred > 0.1) 
cm

Sensitivty = cm[4]/(cm[4]+cm[2]) # Recall
Sensitivty

Specificity = cm[1] / (cm[1] + cm[3])
Specificity

Accuracy = (cm[1] + cm [4])/(cm[1] + cm[2] + cm[3] + cm[4])
Accuracy

Precision = cm[4] / (cm[4] + cm[3])
Precision

mylogit = glm( data.train1$CKD ~ .,family = "binomial", data = data.train1)
summary(mylogit)
prob=predict(mylogit,type=c("response"))
data.test1$prob=prob
library(pROC)
g <- roc(CKD ~ prob, data = data.test1)
plot(g)  
abline(0, 1) #add a 45 degree line


# plot ROC
library(ROCR)
library(Metrics)
pr <- prediction(prob_pred,data.test1$CKD)
perf <- performance(pr,measure = "tpr",x.measure = "fpr")
plot(perf)
auc(data.test1$CKD,prob_pred)

# Making the Confusion Matrix to get the recall value for the prediction 
#Recall = tp/(tp+fn)

str(data.train1)

hist(prob_pred)

dim(data.test1)
table(data.test1$CKD)

library(MASS)
model <- glm(data.train1$CKD ~., data = data.train1, family = binomial) %>% stepAIC(trace = FALSE)
# Summarize the final selected model
summary(model)
# Make predictions
probabilities <- model %>% predict(data.test1, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")

full.model <- glm(data.train1$CKD ~., data = data.train1, family = binomial)
coef(full.model)

library(MASS)
step.model <- full.model %>% stepAIC(trace = FALSE)
coef(step.model)
