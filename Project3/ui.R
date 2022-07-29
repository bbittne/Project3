#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(caret)
library(shinythemes)
library(DT)
library(tree)

#Grab the data from the exported CSV file
stockResults <- read_csv("../MSFTStockData.csv")

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("cerulean"), withMathJax(),

    tabsetPanel(
        
        tabPanel("About",fuild=TRUE,
            # Application title
            titlePanel("ST558 - Project 3"),
            
            h3("About"),
        
            # Sidebar with a slider input for number of bins
            sidebarLayout(
                sidebarPanel(
                    h3("Bryan Bittner"),
                    h3("ST558 - Project 3"),
                    tags$ul(
                        tags$li(a(href = "https://github.com/bbittne/Project3", "REPOSITORY")),
                        tags$li(a(href = "https://bbittne.github.io/", "BLOG")),
                    ),
                ),
        
                # Show a plot of the generated distribution
                mainPanel(
                    h3("Overview"),
                    tags$p("The purpose of this application is so show off my shiny app skills!! I thought about using a 'z' instead of an 's' on skills but I turn 40 in a few days so I think I'm past that stage by a couple decades! In this application we will take a deep dive into one of the world's greatest companies - you guessed it - Microsoft!!"),
                    h3("Data and Source"),
                    tags$p("When you already have something great, you don't re-invent the wheel. Hence I am shamelessly using the same API source and content that I used from project 1. Rather than calling the API, I exported the data to a .csv and read the dataset directly into the application."),
                    tags$ul(
                        tags$li(a(href = "https://polygon.io/docs/stocks/get_v2_aggs_ticker__stocksticker__range__multiplier___timespan___from___to", "API Source"))
                    ),
                    h3("Page Descriptions"),
                    tags$p("Here is where I talk about all of the pages in the application."),
                    tags$ul(
                        tags$li("Page 1 - About"),
                            tags$p("Hopefully you get the gist about this page!"),
                        tags$li("Page 2 - Data Exploration"),
                            tags$p("Here we start to get into the good stuff. On the side panel we start with two plotting options, a Histogram and a Scatter Plot."),
                            tags$p("A histogram is used to show the frequency of a numeric variable's occurance. The variable options for the histogram are Opening Price, Closing Price, and Volume Weighted Average Price. The Closing Price is particularly useful here since it shows where the price is most likely to close at."),
                            tags$p("A scatter plot is a great way to show individual observations. Here there are three available variables, Trading Volume, Number of Transactions, and Closing Price. All of these variables are plotted over the trading date on the x-axis. When you see some of the massive numbers in volume and transactions, it really make you question what was happening on those specific trading days."),
                        tags$p("Also available are 'Numerical Summaries' which give a quick overview of the variables for each year going back to 2017. Seeing the closing price average in 2017 was $78 vs $283 in 2022 makes me wish I had some extra money for investing!"),
                        tags$li("Page 3 - Modeling"),
                            tags$p("Now that we have done some initial analysis, it is easy to see that the most important variable is going to be the Closing Price. This will be our response variable for all of our modeling. This Modeling page contains three separate tabs, each of which correspond to a section on the side panel."),
                            tags$p("The Modeling Info tab has the corresponding Modeling Info section on the side panel. Here be give a basic overview of our three model types, along with some neat mathematical expressions for all the math brains out there."),
                            tags$p("The Model Fitting tab has a corresponding Model Fitting section on the side panel. This is where you have the option to change our Training/Test split percentage, as well as the variables used in our modeling. Once you have the model set up according to your liking, click on the 'Run All Models' button to fit all three models. Note - the Random Forest model will take 5-10 seconds to run."),
                            tags$p("The Prediction tab has a corresponding Predictions section on the side panel. Once the models have been fit, you can click between the model types and see the predicted value. You can also change the variables to you liking and the prediction will automatically be re-evaluated."),
                        tags$li("Page 4 - Data"),
                            tags$p("Like what you see? Well you are in luck! Here is where you can export the data and come up with your own modeling. There are 1257 records in this dataset so feel free to subset the rows and columns however you see fit. Once you are ready, just click on the 'Download Stock Data' and the data will be expoted to a .csv file of your choosing."),
                    ),
                    h3("Microsoft Logo Interpretation"),
                    img(src="MSLogo.png", height='225px',width='362px'),
                )
            )
        ),#End About Section
        
        tabPanel("Data Exploration",fuild=TRUE,
             # Application title
             titlePanel("ST558 - Project 3"),
             h3("Data Exploration"),
             
             # Sidebar with a slider input for number of bins
             sidebarLayout(
                 sidebarPanel(
                     
                     h4("Graphical Summaries"),
                     
                     radioButtons(inputId = "rdoPlotType", label = "Plot Type",
                                  choiceNames = c("Histogram", "Scatter Plot"),
                                  choiceValues = c("histo", "scatter" )),
                         conditionalPanel("input.rdoPlotType == 'histo'",
                            selectInput(inputId = "histoVariable", 
                                      label = "Histogram Variable", 
                                      choices = c("Opening Price" = "o",
                                                  "Closing Price" = "c",
                                                  "Volume Weighted Avg Price" = "vw")),
                            sliderInput("bins",
                                     "Number of bins:",
                                     min = 1,
                                     max = 50,
                                     value = 30)
                         ),#End Histogram Conditional Panel
                        ##Scatterplot Conditional Panel
                         conditionalPanel("input.rdoPlotType == 'scatter'",
                                          selectInput(inputId = "scatVariable", 
                                                      label = "Scatter Plot Variable", 
                                                      choices = c("Trading Volume" = "v",
                                                                  "Number of Transactions" = "n",
                                                                  "Closing Price" = "c")),
                         ),##End Scatter Plot Conditional Panel
                     
                     br(),
                     h4("Numerical Summaries"),
                     selectInput("si_var","Variable to Summarize",
                                 choices = c("Closing Price" = "c",
                                             "Trading Volume" = "v",
                                             "Number of Transactions" = "n"),
                                 selected="c"),
                     radioButtons(inputId = "rdoSumOption", label = "Summary Option",
                                  choiceNames = c("Mean", "Max", "Min"),
                                  choiceValues = c("mean", "max","min")),
                 ),
                 
                 mainPanel(
                     h3("Graphical Summaries"),
                     plotOutput("DEPlot"),
                     br(),
                     h3("Numerical Summaries"),
                     DTOutput('tbl')
                 )
             )
        ),#End Data Exploration Section
        
        tabPanel("Modeling",fuild=TRUE,
             # Application title
            titlePanel("Modeling"),
             
             # Sidebar with a slider input for number of bins
            sidebarLayout(
                sidebarPanel(h4("Modeling Info"),
                                radioButtons("rdoSupModel", "Select model for Description", 
                                choiceNames = c("Multiple Linear Regression","Regression Tree","Random Forest"),
                                choiceValues = c("MLR", "rTree","RF")),
                                br(),
                                h4("Model Fitting"),
                                #"Train/Test Data Split - Adjust the Training Portion as necessary",
                                numericInput("trainSplit", "Train/Test Data Split - Adjust the Training Portion as necessary",
                                             .8, min = 0.1, max = 0.9,step=0.1),
                                checkboxGroupInput(inputId = "modelColumns", label = "Select the model variable(s)",
                                                   choices = c("Volume"="v", "Opening Price"="o", "Date"="tDate"),
                                                   selected = c("v","o","tDate")),
                                actionButton("buttonRunModels", "Run All Models"),
                                br(),
                                br(),
                                h4("Predictions"),
                                radioButtons("rdoPredModel", "Select model for Prediction", 
                                          choiceNames = c("Multiple Linear Regression","Regression Tree","Random Forest"),
                                          choiceValues = c("MLR", "rTree","RF")),
                                numericInput("predValueVolume", "Volume Prediction Value",100000, min = 100000, 
                                            max = 200000,step=10000),
                                numericInput("predValueOpenPrice", "Opening Price Prediction Value",100, min = 10, 
                                            max = 500,step=10),
                                dateInput("predValueTradeDate", "Trading Date Prediction Value",value=Sys.Date()+1, min = Sys.Date(), max = Sys.Date()+30)
                            ),#End sidebarPanel
                
                 # Show a plot of the generated distribution
                 mainPanel(
                     # Output: Tabset w/ plot, summary, and table ----
                     tabsetPanel(type = "tabs",
                            tabPanel("Modeling Info",
                                        h3("Modeling Descriptions"),
                                        uiOutput("modelInfo"),
                                        plotOutput("plot")
                                    ),
                            tabPanel("Model Fitting",
                                        h3("Model Fitting"),
                                        "(Select your options on the left and click the 'Run All Models' button)",
                                        DTOutput('tblTestSplit'),
                                        h5("Multiple Linear Regression - Summary Statistics"),
                                        textOutput("modelFitTextMLR"),
                                        verbatimTextOutput(outputId = "modelSummaryMLR"),
                                        h5("Regression Tree - Summary Statistics"),
                                        textOutput("modelFitTextRT"),
                                        verbatimTextOutput(outputId = "modelSummaryRT"),
                                        plotOutput("modelPlotRTree"),
                                        h5("Random Forest - Summary Statistics"),
                                        textOutput("modelFitTextRF"),
                                        verbatimTextOutput(outputId = "modelSummaryRF")
                                    ),
                            tabPanel("Prediction",
                                        h3("Model Predictions"),
                                        h5("'Any sufficiently advanced technology is indistinguishable from magic' - Arthur C. Clarke"),
                                        verbatimTextOutput(outputId = "modelPrediction")
                                        #uiOutput("modelPrediction")
                                    )
                                )#End tabSetPanel
                          )#End mainPanel
             )
        ),#End Modeling Section
        
        tabPanel("Data",fuild=TRUE,
             # Application title
             titlePanel("Data"),
             
             # Sidebar with a slider input for number of bins
             sidebarLayout(
                 sidebarPanel(
                     h4("Subset the Data"),
                     sliderInput("numRows", label = "Subset the Rows", min = 1, max = 1257, value = c(1, 1257)),
                     h4("Subset the Columns"),
                     checkboxGroupInput(inputId = "cboxColumns", label = "Select the column(s)",
                                        choices = c("Volume"="v", "Volume Weighted Price"="vw", "Opening Price"="o",
                                                    "Closing Price"="c", "High Price"="h", "Low Price"="l",
                                                    "Unix Timestamp"="t","Transactions"="n", "Date"="tDate",
                                                    "Symbol"="Symbol","Name"="Name"),
                                        selected = c("v","vw","o", "c","h","l","t","n","tDate","Symbol","Name")),
                     h4("Download the Data"),
                     downloadButton('downloadSD', "Download Stock Data")
                 ),
                 
                 mainPanel(
                     h3("Complete Data set"),
                     DTOutput('tblAll')
                 )
             )
        )#End Data Section
        
    )
))
