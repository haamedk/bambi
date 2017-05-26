require(shiny)
library(shinyjs)
shinyServer(function(input, output) {
  #-----------------------------------------------------------------------
  #          SETTING UP WORK SPACE AND PREPARING ML-PREDICTION
  #-----------------------------------------------------------------------
  # ---- Import libraries, and set seed ---- #
  require(caTools)                  # Used to split the model - shuffles automatically
  require(pROC)                     # Used to predict data with probability
  require(randomForest)             # For Random Forest
  set.seed(127)                     # Probably not needed since we use the whole dataset, but just to be sure.
  # ---- Import datafile, and prepare the "goal" attribute ---- #
  cleveland <- read.csv("datasets/processed.cleveland.csv", header = TRUE, sep = "\t")
  cleveland[cleveland == "?"] <- NA
  # ---- Omit missing values ----#
  cleveland <- na.omit(cleveland)
  cleveland$goal <- ifelse(cleveland$num>=1, "Yes", "No")
  cleveland$num <- NULL
  cleveland$thal <- substr(cleveland$thal, 0, 1)  # To remove the commas
  # Change to numerical value (like age), or categorical value (like sex).
  # ---- Numerical ---- #
  cols <- c("age","trestbps","chol","thalach","oldpeak","ca")
  for(i in cols){cleveland[,i]=as.numeric(cleveland[,i])}
  # ---- Categorical ---- #
  cols <- c("sex","cp","fbs","restecg","exang","slope","thal","goal")
  for(i in cols){cleveland[,i]=as.factor(cleveland[,i])}
  # ---- Impute missing values ---- #
  # cleveland <- rfImpute(goal ~ ., cleveland)
  
  # ---- Train your model ----#
  dForest <- randomForest(goal~.,data = cleveland,mtry=3,ntree=200)
  # ---- Setup new table for receving input from Shiny-web-app, and passing the values for prediction
  dfShiny <- merge(cleveland,as.data.frame(c()))
  hide("resultsPage")  # TODO: Can't set this as hidden to begin with. This is a work-around
  
  #-----------------------------------------------------------------------
  #          SOME SUPER AWESOME FUNCTIONS HERE!
  #-----------------------------------------------------------------------
  # ---- FORMAT TO PERCENT WITH 1 DIGIT ---- #
  percent <- function(y, digits = 1, format = "f", ...) {
    return(paste0(formatC(100 * y, format = format, digits = digits, ...), "%"))
  }
  # ---- READ INPUTS AND CALCULATE RISK ---- #
  lazyRiskCalculator <- function(){
    {
      dfShiny[1, "age"] <- input$ageInput
      dfShiny[1, "trestbps"] <- input$trestbpsInput
      dfShiny[1, "chol"] <- input$cholInput
      dfShiny[1, "thalach"] <- input$thalachInput
      dfShiny[1, "oldpeak"] <- input$oldpeakInput
      dfShiny[1, "ca"] <- input$caInput
      dfShiny[1, "sex"] <- as.factor(input$sexInput)
      dfShiny[1, "cp"] <- as.factor(input$cpInput)
      dfShiny[1, "fbs"] <- as.factor(input$fbsInput)
      dfShiny[1, "restecg"] <- as.factor(input$restecgInput)
      dfShiny[1, "exang"] <- as.factor(input$exangInput)
      dfShiny[1, "slope"] <- as.factor(input$slopeInput)
      dfShiny[1, "thal"] <- input$thalInput
      predictFromShiny <- predict(dForest, dfShiny, type = "prob")
      return(predictFromShiny[1])
    }
  }
  # ---- DEAD MAN WALKING ---- #
  lazyDead <- function(x){
    output$bambiPredictionTextReturnedFromServer <- renderText({
      paste('Bambi predicts that you have a risk <br>for heart disease with ', percent(1-x), ' certainty!')
    })
    toggle("resultsBadOne")
    toggle("resultsBadTwo")
  }
  # ---- YOU LIVE! ---- #
  lazyLive <- function(x){
    output$bambiPredictionTextReturnedFromServer <- renderText({
      paste('Bambi predicts that you have no risk <br>for heart disease with ', percent(x), ' certainty!')
    })
    toggle("resultsGoodOne")
    toggle("resultsGoodTwo")
  }
  #-----------------------------------------------------------------------
  #          BUTTON CODES START HERE (THEY CALL THE FUNCTIONS)
  #-----------------------------------------------------------------------
  # ---- SUBMIT-BUTTON CODE ---- #
  observeEvent(input$runif, {
    toggle("inputArea")
    toggle("resultsPage")
    toggle("btnReset")
    toggle("btnSubmit")
    risk <- lazyRiskCalculator()
    if(risk<=0.5){
      lazyDead(risk)
    }else{
      lazyLive(risk)
    }
  })
  # ---- RESET-BUTTON CODE ---- #
  observeEvent(input$reset, {
    toggle("inputArea")
    hide("resultsPage")
    hide("btnReset")
    toggle("btnSubmit")
    hide("resultsGoodOne")
    hide("resultsGoodTwo")
    hide("resultsBadOne")
    hide("resultsBadTwo")
  })
})#./shinyServer()