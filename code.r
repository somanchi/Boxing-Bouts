setwd("C:/Users/MyHomePC/Desktop/boxing-bouts")
data <-read.csv("bouts_out_new.csv")

age_A.median <- median(data$age_A, na.rm = TRUE) 
data[is.na(data$age_A),"age_A" ] <- age_A.median 
age_B.median <- median(data$age_B, na.rm = TRUE) 
data[is.na(data$age_B),"age_B" ] <- age_B.median 
height_A.median <- median(data$height_A, na.rm = TRUE) 
data[is.na(data$height_A),"height_A" ] <- height_A.median 
height_B.median <- median(data$height_B, na.rm = TRUE) 
data[is.na(data$height_B),"height_B" ] <- height_B.median 
reach_B.median <- median(data$reach_B, na.rm = TRUE)
data[is.na(data$reach_B),"reach_B" ] <- reach_B.median 
reach_A.median <- median(data$reach_A, na.rm = TRUE) 
data[is.na(data$reach_A),"reach_A" ] <- reach_A.median 
weight_A.median <- median(data$weight_A, na.rm = TRUE) 
data[is.na(data$weight_A),"weight_A" ] <- weight_A.median 
weight_B.median <- median(data$weight_B, na.rm = TRUE)
data[is.na(data$weight_B),"weight_B" ] <- weight_B.median 
kos_B.median <- median(data$kos_B, na.rm = TRUE)
data[is.na(data$kos_B),"kos_B" ] <- kos_B.median
judge1_A.median <- median(data$judge1_A, na.rm = TRUE)
data[is.na(data$judge1_A),"judge1_A" ] <- judge1_A.median 
judge1_B.median <- median(data$judge1_B, na.rm = TRUE) 
data[is.na(data$judge1_B),"judge1_B" ] <- judge1_B.median 
judge2_A.median <- median(data$judge2_A, na.rm = TRUE) 
data[is.na(data$judge2_A),"judge2_A" ] <- judge2_A.median 
judge3_A.median <- median(data$judge3_A, na.rm = TRUE) 
data[is.na(data$judge3_A),"judge3_A" ] <- judge3_A.median 
judge3_B.median <- median(data$judge3_B, na.rm = TRUE) 
data[is.na(data$judge3_B),"judge3_B" ] <- judge3_B.median 
judge2_B.median <- median(data$judge2_B, na.rm = TRUE) 
data[is.na(data$judge2_B),"judge2_B" ] <- judge2_B.median 

data$decision <- as.factor(data$decision)

set.seed(2)
train.index <- sample(c(1:dim(data)[1]),dim(data)[1]*0.6)
train.data <- data[train.index,]
valid.data<- data[-train.index,]


# MODEL 

box.equation <- "result ~." # Creating the Formula for the model 
box.formula <- as.formula(box.equation) # Type casting the String into formula.

library(randomForest)

box.model <- randomForest(formula = box.formula,data = train.data,ntree = 3,mtry = 15,importance=TRUE)
Pr.Survived <- predict(box.model,newdata = valid.data)


library(caret)
confusionMatrix(valid.data$result,Pr.Survived) 
library(tree)
library("partykit")
library(ggplot2)

ggplot2.scatterplot(data=data, xName='reach_A',yName='reach_B', 
                    groupName='result', size=3,
                    backgroundColor="white",
                    groupColors=c('#999999','#E69F00', '#56B4E9'))  

box.model.ctree <- ctree(result~. , data=train.data)

plot(box.model.ctree)

hist(data$reach_A, xlab= "reach_A")