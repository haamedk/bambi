require(shiny)
require(shinyjs)
bootstrapPage(
  useShinyjs(rmd = FALSE, debug = TRUE, html = FALSE, showLog = NULL),
  tags$head(
    # CSS
    tags$link(rel = "stylesheet", type = "text/css", href = ""),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/animate.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=Raleway:400,100,200,300,500,600,700,800,900|Montserrat:400,700"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/bootstrap.min.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/main.css"),
    # JS
    tags$script(src="js/modernizr-2.7.1.js"),
    tags$script(src="js/wow.min.js"),
    tags$head(tags$script(src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.0/jquery.min.js")),
    tags$script(src="js/bootstrap.min.js"),
    tags$script(src="js/main.js")
  ),
  # Navbar
  HTML('<div id="hktop"></div><div class="navbar navbar-inverse navbar-fixed-top"> <div class="container"> <div class="navbar-header"> <button type="button" class="navbar-toggle" data-toggle="collapse" data-target=".navbar-collapse"> <span class="icon-bar"></span> <span class="icon-bar"></span> <span class="icon-bar"></span> </button> <a class="logo scroll" href="#hktop"><img src="img/logo.svg" alt="Logo"></a></div><div class="navbar-collapse collapse"> <ul class="nav navbar-nav navbar-right"> <li><a href="#test" class="scroll">Take the Test</a></li></ul> </div></div></div>'),
  # Pre-form-data
  HTML('<header> <div class="container"> <div class="row"> <div class="col-xs-6"> <a href="#"><img src="img/logo.svg" alt="Logo"></a> </div><div class="col-xs-6 signin text-right navbar-nav"> <a href="#test" class="scroll">Take the Test</a> </div></div><div class="row header-info"> <div class="col-sm-10 col-sm-offset-1 text-center"> <img src="img/webLogo.png" width="50%"> <br/> <p class="lead wow fadeIn">Predicting your heart attack since 2017! &nbsp; &nbsp; </p><br/> <div class="row"> <div class="col-md-8 col-md-offset-2 col-sm-10 col-sm-offset-1"> <div class="row"> <div class="col-xs-12 text-center wow fadeInUp"> <a href="#test" class="btn btn-primary btn-lg scroll">Take the Test</a> </div></div></div></div></div></div></div></header><div class="mouse-icon hidden-xs"> <div class="scroll"></div></div><section id="be-the-first" class="pad-lg"> <div class="container"> <div class="row"> <div class="col-sm-8 col-sm-offset-2 text-center margin-30 wow fadeIn"> <h2>Predict your risk<br/> with Machine Learning</h2> <p class="lead">Cutting-edge technology from Kista!</p></div></div></div></section><section id="main-info" class="pad-sm"> <div class="container"> <div class="row"> <div class="col-sm-4 wow fadeIn"> <hr class="line blue"> <h3>Supervised by Panos and Vasilis</h3> <p>Two cool guys from DSV and KI help we make this super cool project!</p></div><div class="col-sm-4 wow fadeIn"> <hr class="line purple"> <h3>Super accurate</h3> <p>We use data from hundreds of people to build our prediction model. It would be super accurate!</p></div><div class="col-sm-4 wow fadeIn"> <hr class="line yellow"> <h3>Save more than 1,000 lives</h3> <p>Up until now, 1,000 was saved with our tool!</p></div></div></div></section><section id="test" class="pad-sm"> <div class="container"> <div class="row margin-40"> <div class="col-sm-8 col-sm-offset-2 text-center"> <h2 class="white">Heart Disease Risk Calculator</h2> <p class="white">Please fill in the form below!</p></div></div><div class="row margin-50"> <div class="col-md-8 col-md-offset-2"> <div class="panel panel-default"> <div class="panel-body">'),
  # ---- SHINY FORM STARTS HERE!
  # ---- Fluid row Inputs ---- #
  fluidRow(id = "inputArea", 
           # ---- Left column ----#
           column(6,
                  # ---- SEX ---- #
                  radioButtons(inputId = "sexInput",label = "Please select your gender",choiceNames = list("Male","Female"),choiceValues = list(1,0)),
                  # ---- CP - CHEST PAIN ---- #
                  selectInput("cpInput",label = "Please specify chest pain type",choices = c("Asymptomatic" = 4,"Typical angina" = 1,"Atypical angina" = 2,"Non-anginal pain" = 3)),
                  # ---- fbs ---- #
                  selectInput("fbsInput", label = "Is fasting blood sugar above, or below 120 mg/dl?",choices = c("Below  120 mg/dl" = 0,"Above 120 mg/dl" = 1)),
                  # ---- restecg ---- #
                  selectInput("restecgInput",label = "Enter resting ECG",choices = c("Normal" = 0,"Showing probable or definite left ventricular hypertrophy by Estes' criteria" = 2,"having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV)" =1)),
                  # ---- exang ---- #
                  selectInput("exangInput",label = "Exercise induced angina?",choices = c("No" = 0, "Yes" = 1)),
                  # ---- slope ---- #
                  selectInput("slopeInput",label = "Please specify the slope of the peak exercise ST segment",choices = c("Upsloping" = 1,"Flat" = 2,"Downsloping" = 3)),
                  # ---- thal ---- #
                  selectInput("thalInput",label = "Thalium radioisotope chemical stress test",choices = c("Normal" = "3","Reversable defect" = "7","Fixed defect" = "6"))
           ), # ./Left Column
           # ---- Right column ----#
           column(6,
                  # ---- AGE ---- #
                  sliderInput("ageInput", "Enter your age",min=20, max=100, value=56),
                  # ---- trestbps ---- #
                  sliderInput("trestbpsInput", "Enter your resting blood pressure (in mm Hg)",min=60, max=260, value=130),
                  # ---- chol ---- #
                  sliderInput("cholInput", "Enter your serum cholestoral in mg/dl",min=90, max=700, value=240),
                  # ---- thalach ---- #
                  sliderInput("thalachInput", "Enter your maximum heart rate achieved",min=60, max=260, value=160),
                  # ---- oldpeak ---- #
                  sliderInput("oldpeakInput", "Enter ST depression induced by exercise relative to rest",min=0, max=7, value=0.9, step = 0.1),
                  # ---- ca ---- #
                  sliderInput("caInput", "Enter the number of major vessels, colored by flourosopy",min=0, max=3, value=0)
           ) # ./Right column
  ), #./Fluid row Inputs
  
  # ---- RESULT ONE'S ---- #
  hidden(
    fluidRow(id = "resultsGoodOne", column(12, align="center",HTML('<div class="text-center"> <h2 class="text-success">Congratulation!</h2> </div>'))),
    fluidRow(id = "resultsBadOne",column(12, align="center",HTML('<div class="text-center"> <h2 class="text-danger">Oh no! :(</h2> </div>')))
  ),
  
  # ---- RETURNED STRING WITH VALUES FROM SERVER ---- #
  fluidRow(id = "resultsPage",column(12, align="center",htmlOutput("bambiPredictionTextReturnedFromServer"))),
  
  # ---- RESULT TWO'S ---- #
  hidden(
    fluidRow(id = "resultsGoodTwo",column(12, align="center",HTML('<div> <img src="img/confetti.svg" width="150" class="pad-xs"> <p>Keep exercising, eat good food, control your weight, sleep well!</p></div>'))),
    fluidRow(id = "resultsBadTwo",column(12, align="center",HTML('<div> <img src="img/cardiogram.svg" width="150" class="pad-xs"> <p>Please stop eating kanelbullar, and consult your physician.</p></div>')))
  ),
  
  # ---- SUBMIT BUTTON ---- #
  fluidRow(id = "btnSubmit",column(12, align="center",actionButton("runif", "Submit"))),
  # ---- RESET BUTTON ---- #
  hidden(fluidRow(id = "btnReset",column(12, align="center",actionButton("reset", "Take the test again")))),
  
  # Below the form
  HTML(' </div></div></div></div></div></div></section> <section id="press" class="pad-xs"> <div class="container"> <div class="row margin-40"> <div class="col-sm-8 col-sm-offset-2 text-center"> <h2 class="black">From our users</h2> </div></div><div class="row margin-30 news-container"> <div class="col-sm-10 col-sm-offset-1 col-md-8 col-md-offset-2 wow fadeInLeft"> <img class="news-img pull-left" src="img/press-01.jpg" alt="Tech Crunch"> <p class="black">This app has made my heart great again! Fantastic developers. Really nice people. Tremendous work!<br/> <small><em>Anonymous president - May 18, 2017</em></small></p></div></div></div></section> <footer> <div class="container"> <div class="row"> <div class="col-sm-12 text-right"> <p>HTML adapted from the <a href="http://news.visualsoldiers.com/snow-a-free-bootstrap-landing-page-theme/">"Snow - A Free Bootstrap Landing Page Theme"</a><br/><small>This project is part of VT17 5HI009 Projects in health informatics - from idea to specification <br/>Joint Master’s Programme in Health Informatics Karolinsta Institutet and Stockholm University</a></small></p></div></div></div></footer>')
)#./bootstrapPage() or fluidPage()