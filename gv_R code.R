library(tidyr)
library(tidyverse)
library(janitor)
library(lubridate)
library(data.table)
library(zoo)
library(VIM)
library(gridExtra)
library(splitstackshape)
library(tidytext)
library(tm)
library(dplyr)
library(pROC)
library(dplyr)
library(lattice)
library(ggplot2)
library(caret)
library(gbm)

# Import the data set gun_violence
getwd()
gv <- read.csv("C:/Users/Admin/Documents/Gun Violence/gun-violence-data_01-2013_03-2018.csv")


# Remove unnecessary variables

gv_updated <- gv[-c(8:10,16,19,23,26,27:29)]


# Many variables have multiple values stored, so we must seperate the values for better understanding.

data_updated=gv_updated

# Most of the crimes involved less than 8 participants, so
# Considerng 8 participants in every observation, if crime involves more than 8 participants, rest were dropped.


# Participant age - 

data_updated=separate(data_updated,participant_age,c("p1","p2","p3","p4","p5","p6","p7","p8"),sep="::",extra="drop",convert="true")

str(data_updated)

data_updated=separate(data_updated,p1,"person_0",sep="[||]",extra="drop",convert="true")
data_updated=separate(data_updated,p2,"person_1",sep="[||]",extra="drop",convert="true")
data_updated=separate(data_updated,p3,"person_2",sep="[||]",extra="drop",convert="true")
data_updated=separate(data_updated,p4,"person_3",sep="[||]",extra="drop",convert="true")
data_updated=separate(data_updated,p5,"person_4",sep="[||]",extra="drop",convert="true")
data_updated=separate(data_updated,p6,"person_5",sep="[||]",extra="drop",convert="true")
data_updated=separate(data_updated,p7,"person_6",sep="[||]",extra="drop",convert="true")
data_updated=separate(data_updated,p8,"person_7",sep="[||]",extra="drop",convert="true")

#data_updated$age_mean=mean(data_updated$pp2,data_updated$pp3,data_updated$pp4,data_updated$pp5,data_updated$pp6,data_updated$pp7)


#participant gender

data_updated=separate(data_updated,participant_gender,c("G1","G2","G3","G4","G5","G6","G7","G8"),sep="::",extra="drop",convert="true")

str(data_updated)

data_updated=separate(data_updated,G1,"Gender_0",sep="[||]",extra="drop",convert="true")
data_updated=separate(data_updated,G2,"Gender_1",sep="[||]",extra="drop",convert="true")
data_updated=separate(data_updated,G3,"Gender_2",sep="[||]",extra="drop",convert="true")
data_updated=separate(data_updated,G4,"Gender_3",sep="[||]",extra="drop",convert="true")
data_updated=separate(data_updated,G5,"Gender_4",sep="[||]",extra="drop",convert="true")
data_updated=separate(data_updated,G6,"Gender_5",sep="[||]",extra="drop",convert="true")
data_updated=separate(data_updated,G7,"Gender_6",sep="[||]",extra="drop",convert="true")
data_updated=separate(data_updated,G8,"Gender_7",sep="[||]",extra="drop",convert="true")


#participant status

data_updated=separate(data_updated,participant_status,c("S1","S2","S3","S4","S5","S6","S7","S8"),sep="::",extra="drop",convert="true")

str(data_updated)

data_updated=separate(data_updated,S1,"Status_0",sep="[||]",extra="drop",convert="true")
data_updated=separate(data_updated,S2,"Status_1",sep="[||]",extra="drop",convert="true")
data_updated=separate(data_updated,S3,"Status_2",sep="[||]",extra="drop",convert="true")
data_updated=separate(data_updated,S4,"Status_3",sep="[||]",extra="drop",convert="true")
data_updated=separate(data_updated,S5,"Status_4",sep="[||]",extra="drop",convert="true")
data_updated=separate(data_updated,S6,"status_5",sep="[||]",extra="drop",convert="true")
data_updated=separate(data_updated,S7,"Status_6",sep="[||]",extra="drop",convert="true")
data_updated=separate(data_updated,S8,"Status_7",sep="[||]",extra="drop",convert="true")


#Gun Type

data_updated=separate(data_updated,gun_type,c("gt1","gt2","gt3","gt4"),sep="::",extra="drop",convert="true")


data_updated=separate(data_updated,gt1,"Gun_type0",sep="[||]",extra="drop",convert="true")
data_updated=separate(data_updated,gt2,"Gun_type1",sep="[||]",extra="drop",convert="true")
data_updated=separate(data_updated,gt3,"Gun_type2",sep="[||]",extra="drop",convert="true")
data_updated=separate(data_updated,gt4,"Gun_type3",sep="[||]",extra="drop",convert="true")

# characteristics

data_updated=separate(data_updated,incident_characteristics,c("incident_char_1","incident_char_11","incident_char_2","incident_char_22","incident_char_3","incident_char_33","incident_char_4","incident_char_44","incident_char_5"),sep="[||]",extra="drop",convert="true")

# Relationship

data_updated=separate(data_updated,participant_relationship,c("pr1","pr2","pr3","pr4"),sep="::",extra="drop",convert="true")


data_updated=separate(data_updated,pr1,"participant_relationship_0",sep="[||]",extra="drop",convert="true")
data_updated=separate(data_updated,pr2,"participant_relationship_1",sep="[||]",extra="drop",convert="true")
data_updated=separate(data_updated,pr3,"participant_relationship_2",sep="[||]",extra="drop",convert="true")
data_updated=separate(data_updated,pr4,"participant_relationship_3",sep="[||]",extra="drop",convert="true")



# Out of new columns created, we dont require columns ending with '_0'.
# Agian deleting extra columns

data_updated1 <- data_updated[-c(5,8,10,15,17,19,21,26,34,35,43,47)]

# checking data Quality(NA's)


diagnose(data_updated)
diagnose(data_updated[20:30])
diagnose(data_updated[30:40])
diagnose(data_updated[40:43])


# Gun_type2 and gun_type3 have 94.7 and 97.8 % of data missing
# incident_char_5 have 87.8% missing data
# Most of the incidents have below 3 participants, so we are have experiencing missing data for person 4,5,6 and 7
# Status_4 to status_7 also have more than 94% of missing data.

# So eliminating all the columns with missing data.

data_updated1 <- data_updated1[-c(7,9,10,15,22:25,29:32,34,35,39:42)]


gun <- data_updated1
str(gun)

# Converting variables to proper data types

gun$date <- as.Date(gun$date)
gun$Gun_type1 <- as.factor(gun$Gun_type1)
gun$incident_char_1 <- as.factor(gun$incident_char_1)
gun$incident_char_2 <- as.factor(gun$incident_char_2)
gun$incident_char_3 <- as.factor(gun$incident_char_3)
gun$Gender_1 <- as.factor(gun$Gender_1)
gun$Gender_2 <- as.factor(gun$Gender_2)
gun$Gender_3 <- as.factor(gun$Gender_3)
gun$participant_relationship_1 <- as.factor(gun$participant_relationship_1)
gun$Status_1 <- as.factor(gun$Status_1)
gun$Status_2 <- as.factor(gun$Status_2)
gun$Status_3 <- as.factor(gun$Status_3)

str(gun)

#index <- sample(1:nrow(gun.comp),(.05)*nrow(gun.comp))  #reduce dataset
#gun <- gun.comp [index, ]

# modeling target variable
# Severity is considered when there is somebody killed.

gun$severity <- ifelse(gun$n_killed>0,1,0)
gun$severity <- as.factor(gun$severity)


table(gun$severity)   # to check balance
prop.table(table(gun$severity))

# Identifying highest crime rate state:

barplot(table(gun$state),main="Number of incidents per state",col=4)

barplot(table(gun$state)[table(gun$state)>10000],col=2,main="Top states with incidents greater than 10000")

# California, Florida, Illinois and Texas are amongst the top states

# Now calculating highest killing state wise

hg <- aggregate(gun$n_killed,by=list(gun$state),sum)
plot(hg,main="Total killings per State")
# California and Texas are the states with highest number of killings

# Checking target variable balance for both the states

gun.CA<-subset(gun, state=="California")
prop.table(table(gun.CA$severity))

gun.TX<-subset(gun, state=="Texas")
prop.table(table(gun.TX$severity))

# It shows that severity is almost balanced same with both the states.
# I will be concentrating on Texas

str(gun.TX)

##quality check
library(dataQualityR)
checkDataQuality(data = gun.TX, 
                 out.file.num ="dq_num.csv", 
                 out.file.cat= "dq_cat.csv")
dq_num<-read.csv("~/dq_num.csv")
dq_cat<-read.csv("~/dq_cat.csv")
View(dq_num)   # high perc missing for split columns
View(dq_cat)   

###Data cleaning


gun.TX <- as.data.frame(gun.TX)

#grouping age

age.df <- data.frame(gun.TX$person_1,gun.TX$person_2,gun.TX$person_3)
age.df[is.na(age.df)] <- 0

gun.TX$age_u18 <- rowSums(age.df < 18 & age.df != 0)
gun.TX$age_u35 <- rowSums(age.df > 17 & age.df < 36 & age.df != 0)
gun.TX$age_old <- rowSums(age.df > 35 & age.df != 0)

# grouping gender - 
# Finding sum of male and female

gun.TX$Gender_1 <- addNA(gun.TX$Gender_1)
gun.TX$Gender_2 <- addNA(gun.TX$Gender_2)
gun.TX$Gender_3 <- addNA(gun.TX$Gender_3)

gender.df <- data.frame(gun.TX$Gender_1,gun.TX$Gender_2,gun.TX$Gender_3)

gun.TX$male <- rowSums(gender.df == 'Male')
gun.TX$female <- rowSums(gender.df == 'Female')

#removing NA's from guns_involves and considering 1 by default

gun.TX$n_guns_involved[is.na(gun.TX$n_guns_involved)] <- 1
gun.TX$severity=as.factor(gun.TX)

##MODELING:

gun.model <- select(gun.TX,
                  male,
                  female,
                  age_old,
                  age_u18,
                  age_u35,
                  n_guns_involved,
                  latitude,
                  longitude,
                  n_injured,
                  severity)

str(gun.model)
# make.names(names(gun.model))

# Now we have severity as our target variable and rest of the variables are the model parameters
# As we dont have any categorical predictors, we wont need to dummify the data


gun.model$severity <- as.integer(gun.model$severity)

dataDummy <- dummyVars("~.",data=gun.model, fullRank=F)
gun.model <- as.data.frame(predict(dataDummy,gun.model))

table(gun.model$severity)

gun.model$severity <- as.factor(gun.model$severity)
levels(gun.model$severity) <- make.names(levels(factor(gun.model$severity)))


str(gun.model)
# Note: X1 in severity denotes it is 'non-severe'
# X2 in severity denotes that it is 'severe'


# Generalize outcome and predictors 

ocname <- 'severity'
predname <- names(gun.model)[names(gun.model)!=ocname]

# Split data

set.seed(1234)  # setting seed to reproduce results of random sampling
split<-(.80)
index <- createDataPartition(gun.TX$severity, p=split, list=FALSE)  # row indices for training data

train.TX <- gun.model[index, ]  # model training data
test.TX  <- gun.model[-index, ]   # test data



# Using trainControl to reduce the time taken by the model

objControl <- trainControl(method = 'cv',number = 3,
                           returnResamp = 'none',
                           summaryFunction = twoClassSummary,
                           classProbs = TRUE)

# Run Model - GBM

gbm.model<-train(train.TX[,predname],train.TX[,ocname],
           method='gbm',
           trControl=objControl)

summary(gbm.model)

# Summary

gbm.predict<-predict(gbm.model,test.TX[,predname],type="raw")
confusionMatrix(gbm.predict,test.TX[,ocname])

gbm.probs <- predict(gbm.model,test.TX[,predname],type="prob") 
head(gbm.probs)

gbm.plot<-plot(roc(test.TX$severity,gbm.probs[,2]))

# Summary Statistcs:

# Accuracy - 81%
# False-positive prediction is high(=328) , i.e predicted 'non-severe' but actually a severe event




##2: rPart

rpart.model <- gbm.model<-train(train.TX[,predname],train.TX[,ocname],
                                method='rpart',
                                trControl=objControl)

summary(rpart.model)

# Analysing rpart statistics

rpart.predict<-predict(rpart.model,test.TX[,predname],type="raw")
confusionMatrix(rpart.predict,test.TX[,ocname])

# Summary Statistics- rpart

# Accuracy - 77%
# False positive error is high (=434)

##3: Random Forest:

# NA are not permitted in Random Forest

rf.train.TX <- na.omit(gun.model[index, ])  # model training data
rf.test.TX  <- na.omit(gun.model[-index, ])   # test data

rf.model <- gbm.model<-train(rf.train.TX[,predname],rf.train.TX[,ocname],
                                method='rf',
                                trControl=objControl)

summary(rf.model)

# Analysing random Forest statistics

rf.predict<-predict(rf.model,rf.test.TX[,predname],type="raw")
confusionMatrix(rf.predict,rf.test.TX[,ocname])

# Summary Statistics- random Forest

# Accuracy - 82%
# False positive error is high, but least of all three models (=315)

# Conclusion-
# We would prefer Random Forest  method to predict if the incident happening will be a severe one or not


