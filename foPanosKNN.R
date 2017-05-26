# --------------------------------------------------------------------------------
#         SETUP YOUR WORKSPACE AND CLEAR PREVISOUS
# --------------------------------------------------------------------------------
rm(list = ls())
cat("\014")
graphics.off()
require(caTools)
require(class)
# --------------------------------------------------------------------------------
#         IMPORT YOUR DATA-FILES AND RE-FORMAT THEM WHEN NECESSARY
# --------------------------------------------------------------------------------
# Import datafile, and prepare the "goal" attribute
cleveland <- read.csv("~/OneDrive/hi08/dami/datasets/processed.cleveland.csv", header = TRUE, sep = "\t")
# ---- Omit missing values ---- #
{
  cleveland[cleveland == "?"] <- NA
  cleveland <- na.omit(cleveland)
}
cleveland$goal <- ifelse(cleveland$num>=1, "Yes", "No")
cleveland$num <- NULL
cleveland$thal <- substr(cleveland$thal, 0, 1)  # To remove the commas

# Change to numerical value (like age), or categorical value (like sex).
# ---- Categorical ---- #
cols <- c("sex","cp","fbs","restecg","exang","slope","thal","goal")
for(i in cols){
  cleveland[,i]=as.factor(cleveland[,i])
}
# ---- Numerical ---- #
cols <- c("age","trestbps","chol","thalach","oldpeak","ca")
for(i in cols){
  cleveland[,i]=as.numeric(cleveland[,i])
}
rm(cols)
rm(i)
# --------------------------------------------------------------------------------
#         CREATE MODEL TRAIN AND TEST FUNCTION
# --------------------------------------------------------------------------------
#Function after different normalizations
lazyPrediction <- function(x){
  
  #Add the categorical values to the normalized tables
  cols <- c("sex","cp","fbs","restecg","exang","slope","thal","goal")
  for(i in cols){
    x[,i] <- cleveland[,i]
  }
  rm(cols)
  rm(i)
  
  # Split the data into train- and test data  
  splitMaster <- sample.split(Y = x$goal,SplitRatio = 0.9)
  trainDataNorm <- x[splitMaster,]
  testDataNorm <- x[!splitMaster,]
  trainDataNorm_target <- trainDataNorm$goal
  testDataNorm_target <- testDataNorm$goal
  trainDataNorm$goal <- NULL
  testDataNorm$goal <- NULL
  
  # ---- KNN predictions ---- #
  # k = how many nearest neighbors to use (usually the sqrt of number of rows (sqrt of 303 = 17.4)), and you always want an odd number in case of a tie.
  modelKNN <- knn(train = trainDataNorm, test = testDataNorm, cl = trainDataNorm_target, k=17, prob = TRUE)
  # TABLE
  t <- table(True=modelKNN, Test=testDataNorm_target)
  t <- t[,c(2,1)]
  t <- t[nrow(t):1, ]
  return(t)
}
# --------------------------------------------------------------------------------
#         CREATE MANUAL CROSS VALIDATION FUNCTION
# --------------------------------------------------------------------------------
lazyCrossValidation <- function(y){
  retrieved <- c()
  accuracy <- c()
  recall <- c()
  specificity <- c()
  precisionnn <- c()
  t11 <- c()
  t12 <- c()
  t21 <- c()
  t22 <- c()
  #  cv <- as.numeric(readline("Please enter how many cross validations you want: "))
  cv <- 10
  for(i in 1:cv){
    # ---- Split the data with new seed each time!---- #
    set.seed(100+i*3)                     # IMPORTANT!!! You have to set different seed for each loop!!
    t <- lazyPrediction(y)
    # Calculate Statistics
    retrieved[i] <- sum(t[,1])                                   # All the predicted 1's in the table                (TP+FN)
    accuracy[i] <- sum(diag(t))/sum(t)         # The percantage of true predictions                (TP+TN)/ALL
    recall[i] <- sum(t[1,1])/sum(t[1,])        # How many % of True Yes were Predicted accurately  (TP/(TP+FN))
    specificity[i] <- t[2,2]/(sum(t[2,]))        # How many % of True No were predictd accurately    (TN/(FP+TN))
    precisionnn[i] <- t[1,1]/sum(t[,1])          # How many % of Predicted Yes were True Yes         (TP/(TP+FP))
    t11[i] <- t[1,1]
    t12[i] <- t[1,2]
    t21[i] <- t[2,1]
    t22[i] <- t[2,2]
  }
  retrieved <- mean(retrieved)
  accuracy <- round(mean(accuracy), digits = 2)
  recall <- round(mean(recall), digits = 2)
  specificity <- round(mean(specificity), digits = 2)
  precisionnn <- round(mean(precisionnn), digits = 2)
  fScore <- round((2*precisionnn*recall/(precisionnn+recall)), digits = 2)
  table <- t
  table[1,1] <- round(mean(t11), digits = 0)
  table[1,2] <- round(mean(t12), digits = 0)
  table[2,1] <- round(mean(t21), digits = 0)
  table[2,2] <- round(mean(t22), digits = 0)
  
  cat("\n---------------------- [ TABLE ] ---------------------------------\n")
  print(table)
  cat("\n---------------------- [ STATISTICS ] ----------------------------\n")
  cat("\nRetrieved:       ", retrieved,                     "   (TP+FN)")
  cat("\nAccuracy:        ", format(accuracy, nsmall= 2),     " (TP+TN)/ALL")
  cat("\nRecall:          ", format(recall, nsmall= 2),       " (TP/(TP+FN))")
  cat("\nSpecificity:     ", format(specificity, nsmall= 2),  " (TN/(FP+TN))")
  cat("\nPrecision:       ", format(precisionnn, nsmall= 2),    " (TP/(TP+FP))")
  cat("\nF-Score:         ", format(fScore, nsmall= 2),       " 2*Precision*Recall/(Precision+Recall)")
  return(accuracy)
  
}










# --------------------------------------------------------------------------------
#         NORMALIZE YOUR NUMERIC VALUES, AND SEND DATA TO PREDICTION!
# --------------------------------------------------------------------------------
# Create function for HK normalization (Panos said this could be a bad idea because of odd max/min values!)
normalize <- function(x){return((x-min(x))/(max(x)-min(x)))}
clevelandNorm <- as.data.frame(lapply(cleveland[,c("age","trestbps","chol","thalach","oldpeak","ca")], normalize))
rm(normalize)
cat("\014")
cat("\nNormalization - (\"F-Scaling\")")
lazyCrossValidation(clevelandNorm)

# Predict with Z-score normalization (Panos suggested this method instead)
clevelandNorm <- as.data.frame(scale(cleveland[,c("age","trestbps","chol","thalach","oldpeak","ca")]))
cat("\n\n\nNormalization - Z-Score")
lazyCrossValidation(clevelandNorm)




















# --------------------------------------------------------------------------------
#         HAMED'S TEST AREA :)
# --------------------------------------------------------------------------------
