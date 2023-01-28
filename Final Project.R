#GB 656: Final Project
#Xiao (Freeman) Zhang

#Load data
rm(list=ls())
library(dplyr)
spaceship_train <- 
  read.csv("~/Desktop/UW - Madison/Fall 2022/GB 656/Final Project/spaceship-titanic/train.csv")
head(spaceship_train)

#Organized the data variable
#HomePlanet
freq_HomePlanet <- table(spaceship_train$HomePlanet)
most_HomePlanet <- names(which.max(freq_HomePlanet))
spaceship_train <- spaceship_train %>%
  mutate(HomePlanet = ifelse(HomePlanet == "", most_HomePlanet, HomePlanet))
HomePlanet_categories_test <- unique(spaceship_train$HomePlanet) 
HomePlanet_categories_test
#CryoSleep
freq_CryoSleep <- table(spaceship_train$CryoSleep)
most_CryoSleep <- names(which.max(freq_CryoSleep))
spaceship_train <- spaceship_train %>%
  mutate(CryoSleep = ifelse(CryoSleep == "", most_CryoSleep, CryoSleep))
CryoSleep_categories_test <- unique(spaceship_train$CryoSleep) 
CryoSleep_categories_test
#Cabin
random_Cabin <- spaceship_train$Cabin
get_non_missing_value <- function(x) {
  return(sample(random_Cabin[!is.na(random_Cabin) & random_Cabin != ""], size=1, replace=TRUE))
}
random_Cabin <- ifelse(random_Cabin == "", get_non_missing_value(random_Cabin), random_Cabin)
spaceship_train$Cabin <- random_Cabin
#Destination
freq_Destination<- table(spaceship_train$Destination)
most_Destination <- names(which.max(freq_Destination))
spaceship_train <- spaceship_train %>%
  mutate(Destination = ifelse(Destination == "", most_Destination, Destination))
Destination_categories_test <- unique(spaceship_train$Destination) 
Destination_categories_test
#VIP
freq_VIP<- table(spaceship_train$VIP)
most_VIP <- names(which.max(freq_VIP))
spaceship_train <- spaceship_train %>%
  mutate(VIP = ifelse(VIP == "", most_VIP, VIP))
VIP_categories_test <- unique(spaceship_train$VIP) 
VIP_categories_test
#Age
median_age <- spaceship_train$Age
median <- median(spaceship_train$Age, na.rm=TRUE)
median_age <- ifelse(is.na(median_age), median, median_age)
spaceship_train$Age <- median_age
head(spaceship_train)
#RoomService
median_RoomService <- spaceship_train$RoomService
median <- median(spaceship_train$RoomService, na.rm=TRUE)
median_RoomService <- ifelse(is.na(median_RoomService), median, median_RoomService)
spaceship_train$RoomService <- median_RoomService
#FoodCourt
median_FoodCourt <- spaceship_train$FoodCourt
median <- median(spaceship_train$FoodCourt, na.rm=TRUE)
median_FoodCourt <- ifelse(is.na(median_FoodCourt), median, median_FoodCourt)
spaceship_train$FoodCourt <- median_FoodCourt
#ShoppingMall
median_ShoppingMall <- spaceship_train$ShoppingMall
median <- median(spaceship_train$ShoppingMall, na.rm=TRUE)
median_ShoppingMall<- ifelse(is.na(median_ShoppingMall), median, median_ShoppingMall)
spaceship_train$ShoppingMall <- median_ShoppingMall
#Spa
median_Spa <- spaceship_train$Spa
median <- median(spaceship_train$Spa, na.rm=TRUE)
median_Spa<- ifelse(is.na(median_Spa), median, median_Spa)
spaceship_train$Spa <- median_Spa
#VRDeck
median_VRDeck <- spaceship_train$VRDeck
median <- median(spaceship_train$VRDeck, na.rm=TRUE)
median_VRDeck<- ifelse(is.na(median_VRDeck), median, median_VRDeck)
spaceship_train$VRDeck <- median_VRDeck

#Home_planet
spaceship_train$HomePlanet
HomePlanet_categories <- unique(spaceship_train$HomePlanet) 
HomePlanet_categories
spaceship_train$HomePlanet_number <- spaceship_train$HomePlanet

spaceship_train[spaceship_train$HomePlanet_number == "Earth",]$HomePlanet_number = 0
spaceship_train[spaceship_train$HomePlanet_number == "Mars",]$HomePlanet_number = 1
spaceship_train[spaceship_train$HomePlanet_number == "Europa",]$HomePlanet_number = 2

HomePlanet_number_categories <- unique(spaceship_train$HomePlanet_number) 
HomePlanet_number_categories
head(spaceship_train)

#CryoSleep
spaceship_train$CryoSleep
CryoSleep_categories <- unique(spaceship_train$CryoSleep) 
CryoSleep_categories
spaceship_train$CryoSleep_number <- spaceship_train$CryoSleep
spaceship_train[spaceship_train$CryoSleep_number == "True",]$CryoSleep_number = 0
spaceship_train[spaceship_train$CryoSleep_number == "False",]$CryoSleep_number = 1
head(spaceship_train)

#Cabin
#Cabin: deck number
spaceship_train$Cabin
spaceship_train$Cabin_deck_number <- spaceship_train$Cabin
spaceship_train$Cabin_deck_number

spaceship_train$Cabin_deck_number <- substr(spaceship_train$Cabin_deck_number, start = 1, stop = 1)
spaceship_train$Cabin_deck_number

Cabin_deck_number_categories <- unique(spaceship_train$Cabin_deck_number) 
Cabin_deck_number_categories

spaceship_train[spaceship_train$Cabin_deck_number == "A",]$Cabin_deck_number = 0
spaceship_train[spaceship_train$Cabin_deck_number == "B",]$Cabin_deck_number = 1
spaceship_train[spaceship_train$Cabin_deck_number == "C",]$Cabin_deck_number = 2
spaceship_train[spaceship_train$Cabin_deck_number == "D",]$Cabin_deck_number = 3
spaceship_train[spaceship_train$Cabin_deck_number == "E",]$Cabin_deck_number = 4
spaceship_train[spaceship_train$Cabin_deck_number == "F",]$Cabin_deck_number = 5
spaceship_train[spaceship_train$Cabin_deck_number == "G",]$Cabin_deck_number = 6
spaceship_train[spaceship_train$Cabin_deck_number == "T",]$Cabin_deck_number = 7
spaceship_train$Cabin_deck_number

Cabin_deck_number_categories <- unique(spaceship_train$Cabin_deck_number) 
Cabin_deck_number_categories

#Cabin: size number
library(stringi)
spaceship_train$Cabin
spaceship_train$Cabin_side_number <- spaceship_train$Cabin
spaceship_train$Cabin_side_number

spaceship_train$Cabin_side_number <- stri_sub(spaceship_train$Cabin_side_number, -1)

Cabin_side_number_categories <- unique(spaceship_train$Cabin_side_number) 
Cabin_side_number_categories

spaceship_train[spaceship_train$Cabin_side_number == "P",]$Cabin_side_number = 0
spaceship_train[spaceship_train$Cabin_side_number == "S",]$Cabin_side_number = 1
head(spaceship_train)

#Cabin: num
spaceship_train$Cabin_num_number <- spaceship_train$Cabin
spaceship_train$Cabin_num_number

library(stringr)
spaceship_train$Cabin_num_number <- str_extract(spaceship_train$Cabin_num_number, "(?<=/).*(?=/)")
spaceship_train$Cabin_num_number <- as.numeric(spaceship_train$Cabin_num_number)
spaceship_train$Cabin_num_number
head(spaceship_train)

#Destination
Destination_number_categories <- unique(spaceship_train$Destination) 
Destination_number_categories

spaceship_train$Destination_number <- spaceship_train$Destination
spaceship_train[spaceship_train$Destination_number == "TRAPPIST-1e",]$Destination_number = 0
spaceship_train[spaceship_train$Destination_number == "PSO J318.5-22",]$Destination_number = 1
spaceship_train[spaceship_train$Destination_number == "55 Cancri e",]$Destination_number = 2

spaceship_train$Destination_number
head(spaceship_train)

#VIP
VIP_number_categories <- unique(spaceship_train$VIP) 
VIP_number_categories

spaceship_train$VIP_number <- spaceship_train$VIP
spaceship_train[spaceship_train$VIP_number == "False",]$VIP_number = 0
spaceship_train[spaceship_train$VIP_number == "True",]$VIP_number = 1

spaceship_train$Destination_number
head(spaceship_train)

#Transported Result
spaceship_train$Transported_number <- I(spaceship_train$Transported == 'True') * 1 
head(spaceship_train)
dim(spaceship_train)
str(spaceship_train)

#Set training data to sub train and test
set.seed(1)
spaceship_train <- spaceship_train[sample(1:8693),]
spaceship_train_test  <- spaceship_train[1:2000,]
spaceship_train_train <- spaceship_train[2001:8693,]
dim(spaceship_train_train)

#Logistic regression
spaceship_train_glm <- glm(Transported_number ~ HomePlanet_number + CryoSleep_number 
                           + Cabin_deck_number + Cabin_side_number + Cabin_num_number
                           + Destination_number + Age + VIP_number + RoomService + FoodCourt
                           + ShoppingMall + Spa + VRDeck, family="binomial", spaceship_train_train)


spaceship_train_glm
summary(spaceship_train_glm)
spaceship_logistic_test <- 
  predict(spaceship_train_glm, spaceship_train_test, type="response")

table(spaceship_train_test$Transported_number, (spaceship_logistic_test >0.5))
spaceship_logistic_test

#AUC and ROC curve
library("pROC")
spaceship_logistics.roc <- roc(spaceship_train_test$Transported_number, spaceship_logistic_test, direction="<")
spaceship_logistics.roc
# 88.06%
plot(spaceship_logistics.roc, lwd=3)


##create the classification tress
library(rpart)
spaceship_form1 <- formula(Transported_number ~ HomePlanet_number + CryoSleep_number 
                           + Cabin_deck_number + Cabin_side_number + Cabin_num_number
                           + Destination_number + Age + VIP_number + RoomService + FoodCourt
                           + ShoppingMall + Spa + VRDeck)
spaceship_t1 <- rpart(spaceship_form1, data=spaceship_train_train, cp=.001, method="class")

plot(spaceship_t1,uniform=T,compress=T,margin=.05,branch=0.3)
text(spaceship_t1, cex=.7, col="navy",use.n=TRUE)
plotcp(spaceship_t1)
summary(spaceship_t1)

spaceship_CP <- printcp(spaceship_t1)
spaceship_CP <- spaceship_CP[,1][spaceship_CP[,2] == 11]
spaceship_CP[1]

#Prune
spaceship_t2 <- prune(spaceship_t1,cp=spaceship_CP[1])
plot(spaceship_t2,uniform=T,compress=T,margin=.05,branch=0.3)
text(spaceship_t2, cex=.7, col="navy",use.n=TRUE)
summary(spaceship_t2)

#Apply the new tree
spaceship_t2_test <- predict(spaceship_t2, spaceship_train_test, type="prob")[,2]
table(spaceship_train_test$Transported_number, (spaceship_t2_test > 0.5))

spaceship_t2.roc <- roc(spaceship_train_test$Transported_number, spaceship_t2_test, direction="<")
spaceship_t2.roc
plot(spaceship_t2.roc, lwd=3)

# Cross Validation
library(tree)
spaceship_t3 <- tree(Transported_number ~ HomePlanet_number + CryoSleep_number 
                     + Cabin_deck_number + Cabin_side_number + Cabin_num_number
                     + Destination_number + Age + VIP_number + RoomService + FoodCourt
                     + ShoppingMall + Spa + VRDeck,  data=spaceship_train_train)
plot(spaceship_t3)
text(spaceship_t3, pretty=0)

spaceship_t3.cv <- cv.tree(spaceship_t3, K = 5)
plot(spaceship_t3.cv$size, spaceship_t3.cv$dev, type='b')

spaceship_t4 = prune.tree(spaceship_t3, best = 9)
plot(spaceship_t4)
text(spaceship_t4)

# AUC and curve with the new tree after cv
spaceship_t4_test <- predict(spaceship_t4, newdata = spaceship_train_test)

table(spaceship_train_test$Transported_number, (spaceship_t4_test> 0.5))

library("pROC")
spaceship_t4.roc <- roc(spaceship_train_test$Transported_number, spaceship_t4_test, direction="<")
spaceship_t4.roc
#83.56%
plot(spaceship_t4.roc, lwd=3)

#Create the boosting model
#Change the data type
spaceship_train_train_2 <- spaceship_train_train
spaceship_train_train_2$HomePlanet_number <- as.factor(spaceship_train_train_2$HomePlanet_number)
spaceship_train_train_2$CryoSleep_number <- as.factor(spaceship_train_train_2$CryoSleep_number)
spaceship_train_train_2$Cabin_deck_number <- as.factor(spaceship_train_train_2$Cabin_deck_number)
spaceship_train_train_2$Cabin_side_number <- as.factor(spaceship_train_train_2$Cabin_side_number)
spaceship_train_train_2$Cabin_num_number <- as.numeric(spaceship_train_train_2$Cabin_num_number)
spaceship_train_train_2$Destination_number <- as.factor(spaceship_train_train_2$Destination_number)
spaceship_train_train_2$VIP_number <- as.factor(spaceship_train_train_2$VIP_number)

spaceship_train_test_2 <- spaceship_train_test
spaceship_train_test_2$HomePlanet_number <- as.factor(spaceship_train_test_2$HomePlanet_number)
spaceship_train_test_2$CryoSleep_number <- as.factor(spaceship_train_test_2$CryoSleep_number)
spaceship_train_test_2$Cabin_deck_number <- as.factor(spaceship_train_test_2$Cabin_deck_number)
spaceship_train_test_2$Cabin_side_number <- as.factor(spaceship_train_test_2$Cabin_side_number)
spaceship_train_test_2$Cabin_num_number <- as.numeric(spaceship_train_test_2$Cabin_num_number)
spaceship_train_test_2$Destination_number <- as.factor(spaceship_train_test_2$Destination_number)
spaceship_train_test_2$VIP_number <- as.factor(spaceship_train_test_2$VIP_number)

library(gbm)
spaceship_boost <- gbm(Transported_number ~ HomePlanet_number + CryoSleep_number 
                       + Cabin_deck_number + Cabin_side_number + Destination_number 
                       + Age + VIP_number + RoomService + FoodCourt + ShoppingMall + Spa + VRDeck, 
                       data=spaceship_train_train_2, distribution = "adaboost", 
                       interaction.depth = 6, n.trees = 500, shrinkage = 0.005)

spaceship_boost_predict <- predict(spaceship_boost, spaceship_train_test_2,n.trees = 500, type = "response")
spaceship_boost_predict <- (spaceship_boost_predict-min(spaceship_boost_predict))/(max(spaceship_boost_predict)-min(spaceship_boost_predict))
spaceship_boost_predict
table(spaceship_train_test_2$Transported_number, spaceship_boost_predict > 0.5)

#AUC and ROC curve
spaceship_boost.roc <- roc(spaceship_train_test_2$Transported_number, spaceship_boost_predict, direction="<")
spaceship_boost.roc
plot(spaceship_boost.roc, lwd=3)

# Random Forest
library(randomForest)
spaceship_forest <- randomForest(Transported_number ~ HomePlanet_number + CryoSleep_number 
                                 + Cabin_deck_number + Cabin_side_number + Cabin_num_number
                                 + Destination_number + Age + VIP_number + RoomService + FoodCourt
                                 + ShoppingMall + Spa + VRDeck, ntree = 500, 
                                 data=spaceship_train_train_2,importance=TRUE, na.action = na.roughfix)
spaceship_forest_predict <- predict(spaceship_forest, spaceship_train_test_2)
table(spaceship_train_test_2$Transported_number, spaceship_forest_predict > 0.5)
# AUC and ROC score
spaceship_forest.roc <- roc(spaceship_train_test_2$Transported_number, spaceship_forest_predict, direction="<")
spaceship_forest.roc
plot(spaceship_boost.roc, lwd=3)

#Neural Net
#Fir a single neural net
library(nnet)
spaceship_n1 <- nnet(Transported_number ~ HomePlanet_number + CryoSleep_number 
                     + Cabin_deck_number + Cabin_side_number + Cabin_num_number
                     + Destination_number + Age + VIP_number + RoomService + FoodCourt
                     + ShoppingMall + Spa + VRDeck, data = spaceship_train_train_2, size = 11, maxit = 1000, decay=0.001)
# Predict it to test data
spaceship_yhat.n1 <- predict(spaceship_n1, spaceship_train_test_2)
table(spaceship_train_test_2$Transported_number, spaceship_yhat.n1 > 0.5)

#Auc and ROC curve
spaceship_n1.roc <- roc(spaceship_train_test_2$Transported_number, spaceship_yhat.n1, direction="<")
spaceship_n1.roc
plot(spaceship_n1.roc, lwd=3)



# Now lets apply the result to test data!!
#Organize the data first!
spaceship_test <- 
  read.csv("~/Desktop/UW - Madison/Fall 2022/GB 656/Final Project/spaceship-titanic/test.csv")
head(spaceship_test)
str(spaceship_test)

#Lets replace the NA data
library(tidyverse)
library(dplyr)

#HomePlanet
freq_HomePlanet <- table(spaceship_test$HomePlanet)
most_HomePlanet <- names(which.max(freq_HomePlanet))
spaceship_test <- spaceship_test %>%
  mutate(HomePlanet = ifelse(HomePlanet == "", most_HomePlanet, HomePlanet))
HomePlanet_categories_test <- unique(spaceship_test$HomePlanet) 
HomePlanet_categories_test
#CryoSleep
freq_CryoSleep <- table(spaceship_test$CryoSleep)
most_CryoSleep <- names(which.max(freq_CryoSleep))
spaceship_test <- spaceship_test %>%
  mutate(CryoSleep = ifelse(CryoSleep == "", most_CryoSleep, CryoSleep))
CryoSleep_categories_test <- unique(spaceship_test$CryoSleep) 
CryoSleep_categories_test
#Cabin
random_Cabin <- spaceship_test$Cabin
get_non_missing_value <- function(x) {
  return(sample(random_Cabin[!is.na(random_Cabin) & random_Cabin != ""], size=1, replace=TRUE))
}
random_Cabin <- ifelse(random_Cabin == "", get_non_missing_value(random_Cabin), random_Cabin)
spaceship_test$Cabin <- random_Cabin
#Destination
freq_Destination<- table(spaceship_test$Destination)
most_Destination <- names(which.max(freq_Destination))
spaceship_test <- spaceship_test %>%
  mutate(Destination = ifelse(Destination == "", most_Destination, Destination))
Destination_categories_test <- unique(spaceship_test$Destination) 
Destination_categories_test
#VIP
freq_VIP<- table(spaceship_test$VIP)
most_VIP <- names(which.max(freq_VIP))
spaceship_test <- spaceship_test %>%
  mutate(VIP = ifelse(VIP == "", most_VIP, VIP))
VIP_categories_test <- unique(spaceship_test$VIP) 
VIP_categories_test
#Age
median_age <- spaceship_test$Age
median <- median(spaceship_test$Age, na.rm=TRUE)
median_age <- ifelse(is.na(median_age), median, median_age)
spaceship_test$Age <- median_age
head(spaceship_test)
#RoomService
median_RoomService <- spaceship_test$RoomService
median <- median(spaceship_test$RoomService, na.rm=TRUE)
median_RoomService <- ifelse(is.na(median_RoomService), median, median_RoomService)
spaceship_test$RoomService <- median_RoomService
#FoodCourt
median_FoodCourt <- spaceship_test$FoodCourt
median <- median(spaceship_test$FoodCourt, na.rm=TRUE)
median_FoodCourt <- ifelse(is.na(median_FoodCourt), median, median_FoodCourt)
spaceship_test$FoodCourt <- median_FoodCourt
#ShoppingMall
median_ShoppingMall <- spaceship_test$ShoppingMall
median <- median(spaceship_test$ShoppingMall, na.rm=TRUE)
median_ShoppingMall<- ifelse(is.na(median_ShoppingMall), median, median_ShoppingMall)
spaceship_test$ShoppingMall <- median_ShoppingMall
#Spa
median_Spa <- spaceship_test$Spa
median <- median(spaceship_test$Spa, na.rm=TRUE)
median_Spa<- ifelse(is.na(median_Spa), median, median_Spa)
spaceship_test$Spa <- median_Spa
#VRDeck
median_VRDeck <- spaceship_test$VRDeck
median <- median(spaceship_test$VRDeck, na.rm=TRUE)
median_VRDeck<- ifelse(is.na(median_VRDeck), median, median_VRDeck)
spaceship_test$VRDeck <- median_VRDeck

#Organized the data variable
#Home_planet
spaceship_test$HomePlanet
spaceship_test$HomePlanet_number <- spaceship_test$HomePlanet
head(spaceship_test)

spaceship_test[spaceship_test$HomePlanet_number == "Earth",]$HomePlanet_number = 0
spaceship_test[spaceship_test$HomePlanet_number == "Mars",]$HomePlanet_number = 1
spaceship_test[spaceship_test$HomePlanet_number == "Europa",]$HomePlanet_number = 2

head(spaceship_test)

#CryoSleep
spaceship_test$CryoSleep
spaceship_test$CryoSleep_number <- spaceship_test$CryoSleep
spaceship_test[spaceship_test$CryoSleep_number == "True",]$CryoSleep_number = 0
spaceship_test[spaceship_test$CryoSleep_number == "False",]$CryoSleep_number = 1
head(spaceship_test)

#Cabin
#Cabin: deck number
spaceship_test$Cabin_deck_number <- spaceship_test$Cabin
spaceship_test$Cabin_deck_number

spaceship_test$Cabin_deck_number <- substr(spaceship_test$Cabin_deck_number, start = 1, stop = 1)
spaceship_test$Cabin_deck_number

spaceship_test[spaceship_test$Cabin_deck_number == "A",]$Cabin_deck_number = 0
spaceship_test[spaceship_test$Cabin_deck_number == "B",]$Cabin_deck_number = 1
spaceship_test[spaceship_test$Cabin_deck_number == "C",]$Cabin_deck_number = 2
spaceship_test[spaceship_test$Cabin_deck_number == "D",]$Cabin_deck_number = 3
spaceship_test[spaceship_test$Cabin_deck_number == "E",]$Cabin_deck_number = 4
spaceship_test[spaceship_test$Cabin_deck_number == "F",]$Cabin_deck_number = 5
spaceship_test[spaceship_test$Cabin_deck_number == "G",]$Cabin_deck_number = 6
spaceship_test[spaceship_test$Cabin_deck_number == "T",]$Cabin_deck_number = 7
spaceship_test$Cabin_deck_number

#Cabin: size number
library(stringi)
spaceship_test$Cabin
spaceship_test$Cabin_side_number <- spaceship_test$Cabin

spaceship_test$Cabin_side_number <- stri_sub(spaceship_test$Cabin_side_number, -1)

spaceship_test[spaceship_test$Cabin_side_number == "P",]$Cabin_side_number = 0
spaceship_test[spaceship_test$Cabin_side_number == "S",]$Cabin_side_number = 1
head(spaceship_test)

#Cabin: num
spaceship_test$Cabin_num_number <- spaceship_test$Cabin
spaceship_test$Cabin_num_number

library(stringr)
spaceship_test$Cabin_num_number <- str_extract(spaceship_test$Cabin_num_number, "(?<=/).*(?=/)")
spaceship_test$Cabin_num_number <- as.numeric(spaceship_test$Cabin_num_number)
spaceship_test$Cabin_num_number
head(spaceship_test)

#Destination
spaceship_test$Destination_number <- spaceship_test$Destination
spaceship_test[spaceship_test$Destination_number == "TRAPPIST-1e",]$Destination_number = 0
spaceship_test[spaceship_test$Destination_number == "PSO J318.5-22",]$Destination_number = 1
spaceship_test[spaceship_test$Destination_number == "55 Cancri e",]$Destination_number = 2
head(spaceship_test)

#VIP

spaceship_test$VIP_number <- spaceship_test$VIP
spaceship_test[spaceship_test$VIP_number == "False",]$VIP_number = 0
spaceship_test[spaceship_test$VIP_number == "True",]$VIP_number = 1
head(spaceship_test)
str(spaceship_test)

#logistic model
#77.998%
#77.53%
spaceship_test_lo <- spaceship_test
spaceship_test_lo$Transported <- predict(spaceship_train_glm, newdata = spaceship_test_lo)
spaceship_test_lo$Transported <- I(spaceship_test_lo$Transported > 0.5) * 1
spaceship_test_lo$Transported
#Transfer it to the final result
spaceship_test_lo[spaceship_test_lo$Transported == 0,]$Transported = "False"
spaceship_test_lo[spaceship_test_lo$Transported == 1,]$Transported = "True"
head(spaceship_test_lo)
#Generate the final submission
spaceship_test_lo_submit = subset(spaceship_test_lo, select = c(PassengerId, Transported))
head(spaceship_test_lo_submit)
write.csv(spaceship_test_lo_submit, "~/Desktop/UW - Madison/Fall 2022/GB 656/Final Project/spaceship-titanic/sample_submission.csv", row.names=FALSE)

#Classification tree
#78.232%
#78.81%
spaceship_test_cl <- spaceship_test
spaceship_test_cl$Transported <- predict(spaceship_t2, spaceship_test_cl, type="prob")[,2]
spaceship_test_cl$Transported <- I(spaceship_test_cl$Transported > 0.5) * 1
head(spaceship_test_cl)
#Transfer it to the final result
spaceship_test_cl[spaceship_test_cl$Transported == 0,]$Transported = "False"
spaceship_test_cl[spaceship_test_cl$Transported == 1,]$Transported = "True"
head(spaceship_test_cl)
#Generate the final submission
spaceship_test_cl_submit = subset(spaceship_test_cl, select = c(PassengerId, Transported))
head(spaceship_test_cl_submit)
str(spaceship_test_cl_submit)
write.csv(spaceship_test_cl_submit, "~/Desktop/UW - Madison/Fall 2022/GB 656/Final Project/spaceship-titanic/sample_submission.csv", row.names=FALSE)

#Cross validation
#76.268%
#77.88%
spaceship_test_cv <- spaceship_test
spaceship_test_cv$Transported <- predict(spaceship_t4, newdata = spaceship_test_cv)
spaceship_test_cv$Transported <- I(spaceship_test_cv$Transported > 0.5) * 1
#Transfer it to the final result
spaceship_test_cv[spaceship_test_cv$Transported == 0,]$Transported = "False"
spaceship_test_cv[spaceship_test_cv$Transported == 1,]$Transported = "True"
head(spaceship_test_cv)
#Generate the final submission
spaceship_test_cv_submit = subset(spaceship_test_cv, select = c(PassengerId, Transported))
head(spaceship_test_cv_submit)
write.csv(spaceship_test_cv_submit, "~/Desktop/UW - Madison/Fall 2022/GB 656/Final Project/spaceship-titanic/sample_submission.csv", row.names=FALSE)

#Boosting
# 79.378%
#79.354%
spaceship_test_boost <- spaceship_test
spaceship_test_boost$Transported <- predict(spaceship_boost, spaceship_test_boost,n.trees = 500, type = "response")
spaceship_test_boost$Transported <- (spaceship_test_boost$Transported-min(spaceship_test_boost$Transported))/(max(spaceship_test_boost$Transported)-min(spaceship_test_boost$Transported))
spaceship_test_boost$Transported <- I(spaceship_test_boost$Transported > 0.5) * 1
#Transfer it to the final result
spaceship_test_boost[spaceship_test_boost$Transported == 0,]$Transported = "False"
spaceship_test_boost[spaceship_test_boost$Transported == 1,]$Transported = "True"
head(spaceship_test_boost)
#Generate the final submission
spaceship_test_boost_submit = subset(spaceship_test_boost, select = c(PassengerId, Transported))
head(spaceship_test_boost_submit)
write.csv(spaceship_test_boost_submit, "~/Desktop/UW - Madison/Fall 2022/GB 656/Final Project/spaceship-titanic/sample_submission.csv", row.names=FALSE)

#Random Forest
#79.728
spaceship_test_sf <- spaceship_test
spaceship_test_sf$HomePlanet_number <- as.factor(spaceship_test_sf$HomePlanet_number)
spaceship_test_sf$CryoSleep_number <- as.factor(spaceship_test_sf$CryoSleep_number)
spaceship_test_sf$Cabin_deck_number <- as.factor(spaceship_test_sf$Cabin_deck_number)
spaceship_test_sf$Cabin_side_number <- as.factor(spaceship_test_sf$Cabin_side_number)
spaceship_test_sf$Cabin_num_number <- as.numeric(spaceship_test_sf$Cabin_num_number)
spaceship_test_sf$Destination_number <- as.factor(spaceship_test_sf$Destination_number)
spaceship_test_sf$VIP_number <- as.factor(spaceship_test_sf$VIP_number)
spaceship_test_sf$Transported <- predict(spaceship_forest, newdata = spaceship_test_sf)
spaceship_test_sf$Transported <- I(spaceship_test_sf$Transported > 0.5) * 1
#Transfer it to the final result
spaceship_test_sf[spaceship_test_sf$Transported == 0,]$Transported = "False"
spaceship_test_sf[spaceship_test_sf$Transported == 1,]$Transported = "True"
head(spaceship_test_sf)
#Generate the final submission
spaceship_test_sf_submit = subset(spaceship_test_sf, select = c(PassengerId, Transported))
head(spaceship_test_sf_submit)
write.csv(spaceship_test_sf_submit, "~/Desktop/UW - Madison/Fall 2022/GB 656/Final Project/spaceship-titanic/sample_submission.csv", row.names=FALSE)

# Single Nerual
#78.396%
#79.167
spaceship_test_ne <- spaceship_test
spaceship_test_ne$Transported <- predict(spaceship_n1, newdata = spaceship_test_ne)
spaceship_test_ne$Transported <- I(spaceship_test_ne$Transported > 0.5) * 1
#Transfer it to the final result
spaceship_test_ne[spaceship_test_ne$Transported == 0,]$Transported = "False"
spaceship_test_ne[spaceship_test_ne$Transported == 1,]$Transported = "True"
head(spaceship_test_ne)
#Generate the final submission
spaceship_test_ne_submit = subset(spaceship_test_ne, select = c(PassengerId, Transported))
head(spaceship_test_ne_submit)
write.csv(spaceship_test_ne_submit, "~/Desktop/UW - Madison/Fall 2022/GB 656/Final Project/spaceship-titanic/sample_submission.csv", row.names=FALSE)

