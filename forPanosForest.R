# --------------------------------------------------------------------------------
#         SETUP YOUR WORKSPACE AND CLEAR PREVISOUS
# --------------------------------------------------------------------------------
rm(list = ls())
cat("\014")
graphics.off()
require(caTools)
require(pROC)
require(randomForest)
# --------------------------------------------------------------------------------
#         IMPORT YOUR DATA-FILES AND RE-FORMAT THEM WHEN NECESSARY
# --------------------------------------------------------------------------------
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
#         CREATE A FUNCTION FOR SPLIT AND TEST MODEL
# --------------------------------------------------------------------------------
lazyModeling <- function(x){
  splitMaster <- sample.split(Y = cleveland$goal,SplitRatio = 0.90)
  trainData <- cleveland[splitMaster,]
  testData <- cleveland[!splitMaster,]
  dForest <- randomForest(goal~.,data = trainData,mtry=3,ntree=200)
  predictWithClass <- predict(dForest, testData, type = "class")
  # predictWithProbability <- predict(dForest, testData, type = "prob")
  t <- table(True=testData$goal, Predict=predictWithClass)
  t <- t[,c(2,1)]
  t <- t[nrow(t):1, ]
  return(t)
}
# --------------------------------------------------------------------------------
#         CREATE FUNCTION FOR AUC + ROC-PLOT
# --------------------------------------------------------------------------------
lazyAUC <- function(x){
  splitMaster <- sample.split(Y = cleveland$goal,SplitRatio = 0.90)
  trainData <- cleveland[splitMaster,]
  testData <- cleveland[!splitMaster,]
  dForest <- randomForest(goal~.,data = trainData,mtry=3,ntree=200)
  predictWithProbability <- predict(dForest, testData, type = "prob")
  auct <- matrix(c(testData$goal, predictWithProbability[,2]), ncol = 2)
  return(auct)
}
# --------------------------------------------------------------------------------
#         CREATE FUNCTION FOR IMPORTANCE
# --------------------------------------------------------------------------------
lazyImportance <- function(x){
  splitMaster <- sample.split(Y = cleveland$goal,SplitRatio = 0.90)
  trainData <- cleveland[splitMaster,]
  testData <- cleveland[!splitMaster,]
  dForest <- randomForest(goal~.,data = trainData,mtry=3,ntree=200)
  foo <- as.data.frame(importance(dForest))
  bar <- cbind(rownames(foo)[order(rownames(foo))], foo[order(rownames(foo)),])
  return(bar)
}
# --------------------------------------------------------------------------------
#         CREATE A FUNCTION FOR CROSS VALIDATION
# --------------------------------------------------------------------------------
lazyCrossValidation <- function (x){
  retrieved <- c()
  accuracy <- c()
  recall <- c()
  specificity <- c()
  precisionnn <- c()
  t11 <- c()
  t12 <- c()
  t21 <- c()
  t22 <- c()
  
  for(i in 1:x){
    # ---- Split the data with new seed each time!---- #
    set.seed(100+i*3)                     
    t <- lazyModeling()
    tt <- lazyAUC()
    ttt <- lazyImportance()
    ifelse(exists("impVal"), impVal <- c(impVal, ttt[,2]), impVal <- ttt)
    ifelse(exists("aucLeft"), aucLeft <- c(aucLeft, tt[,1]), aucLeft <- tt[,1])
    ifelse(exists("aucRight"), aucRight <- c(aucRight, tt[,2]), aucRight <- tt[,2])
    
    # Calculate Statistics
    retrieved[i] <- sum(t[,1])                 # All the predicted 1's in the table                (TP+FN)
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
  cat("---------------------- [ STATISTICS ] ----------------------------")
  cat("\nRetrieved:       ", retrieved,                     "   (TP+FN)")
  cat("\nAccuracy:        ", format(accuracy, nsmall= 2),     " (TP+TN)/ALL")
  cat("\nRecall:          ", format(recall, nsmall= 2),       " (TP/(TP+FN))")
  cat("\nSpecificity:     ", format(specificity, nsmall= 2),  " (TN/(FP+TN))")
  cat("\nPrecision:       ", format(precisionnn, nsmall= 2),    " (TP/(TP+FP))")
  cat("\nF-Score:         ", format(fScore, nsmall= 2),       " 2*Precision*Recall/(Precision+Recall)")
  cat("\n")
  aucc <- auc(aucLeft, aucRight)
  plot(roc(aucLeft, aucRight))
  print(aucc)
  cat("\n---------------------- [ Importance ] ----------------------------")
  foo <- matrix(impVal, nrow = (length(impVal)/11))
  for(i in 1:nrow(foo)) {
    cat("\n",
        foo[i,1],"",sep='\t',
        round(mean(as.numeric(foo[i,2:11])), 2)
    )
  }
  return(t)
}
# --------------------------------------------------------------------------------
#         RUN THE FUNCTIONS
# --------------------------------------------------------------------------------
# ---- HERE YOU CAN CHOOSE IF YOU WANT TO REMOVE ANY ATTRIBUTES ---- #
# cleveland$age <- NULL
# cleveland$sex <- NULL
# cleveland$cp <- NULL          # Chest pain type
# cleveland$trestbps <- NULL    # Resting Blood Pressure
# cleveland$chol <- NULL        # Cholesterol
# cleveland$fbs <- NULL         # Fasting blood sugar (above/below 120)
# cleveland$restecg <- NULL     # Resting ECG results
# cleveland$thalach <- NULL     # Max HR
# cleveland$exang <- NULL       # Exercise induced angina (yes/no)
# cleveland$oldpeak <- NULL     # ST depression induced by exercise relative to rest
# cleveland$slope <- NULL       # the slope of the peak exercise ST segment (Up/Flat/Down)
# cleveland$ca <- NULL          # number of major vessels colored by flourosopy (0-3)
# cleveland$thal <- NULL        # Thalium radioisotope chemical stress test (Normal/Reversible/Fixed defect)
t <- lazyCrossValidation(10)    # The number that's passed through is the number/amount of cross-validations

# --------------------------------------------------------------------------------
#         CALCULATE SOME P-VALUES FOR SIGNIFIGANCE OF TABLE
# --------------------------------------------------------------------------------
cat("\n\n---------------------- [OTHER STUFF] -----------------------------\n")
cat("\nMaybe someone will still ask for it..")
cat("\nFisher P-value: ",fisher.test(t)$p.value)
cat("\nChi-Square P-value: ",chisq.test(t)$p.value)

# --------------------------------------------------------------------------------
#         SOME NOTES FOR DR PANOS :)
# --------------------------------------------------------------------------------
# We were having some difficulties with passing multiple values from a function, therefore we made several functions. Since we use a library to shuffle and split the date, this could have been a problem with having to split the data for the same experiment several times with different functions. We think that set.seed() will however prevent this function.

# --------------------------------------------------------------------------------
#         HAMED'S TEST AREA
# --------------------------------------------------------------------------------
# bestmtry <- tuneRF(trainData, trainData$goal, ntreeTry = 200, stepFactor = 1.1, improve = 0.005, trace = T, plot = T)