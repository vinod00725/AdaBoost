rm(list=ls(all=TRUE))
# install.packages("dummies")
setwd("")

# Load required libraries
library(vegan)
library(dummies)

# Feature names
attr = c('id', 'age', 'exp', 'inc', 'zip', 'family', 
         'ccavg', 'edu', 'mortgage', 'loan', 
         'securities', 'cd', 'online', 'cc')

# Read the data using csv file
data = read.csv(file = "UniversalBank.csv", 
                header = TRUE, col.names = attr)

# Removing the id, zip and experience. 
drop_Attr = c("id", "zip", "exp")
attr = setdiff(attr, drop_Attr)
data = data[, attr]
rm(drop_Attr)

# Convert attribute to appropriate type  
cat_Attr = c("family", "edu", "securities", 
             "cd", "online", "cc", "loan")
num_Attr = setdiff(attr, cat_Attr)
rm(attr)

cat_Data <- data.frame(sapply(data[,cat_Attr], as.factor))
num_Data <- data.frame(sapply(data[,num_Attr], as.numeric))

final_Data = cbind(num_Data, cat_Data)
rm(cat_Data, num_Data)

# Do the summary statistics and check for missing values and outliers.
summary(data)

#############################################################
# Divide the data intro train and test
library(caret)
set.seed(1234)

index_train <- createDataPartition(final_Data$loan, p = 0.7, list = F)
trainR <- final_Data[index_train, ]
testR <- final_Data[-index_train, ]

# build the classification model using Adaboost
library(ada) 
model = ada(loan ~ ., iter = 20,data = trainR, loss="logistic") 
# iter = 20 Iterations 
model

# predict the values using model on test data sets. 
pred = predict(model, testR);
pred 

confusionMatrix(testR$loan,pred,positive = "1")
