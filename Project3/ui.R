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
#print(stockResults[1:10,])

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
                    tags$p("When you already have something great, you don't re-invent the wheel. Hence I am shamelessly using the same API source and content that I used from project 1. "),
                    tags$ul(
                        tags$li(a(href = "https://polygon.io/docs/stocks/get_v2_aggs_ticker__stocksticker__range__multiplier___timespan___from___to", "API Source"))
                    ),
                    h3("Page Descriptions"),
                    tags$p("Here is where I talk about all of the pages in the application."),
                    tags$ul(
                        tags$li("Page 1 - About"),
                            tags$p("Hopefully you get the gist about this page:"),
                        tags$li("Page 2 - Data Exploration"),
                            tags$p("Here we start to get into the good stuff."),
                        tags$li("Page 3 - Modeling"),
                            tags$p("Here model some data."),
                        tags$li("Page 4 - Data"),
                            tags$p("Here data some data."),
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
            titlePanel("ST558 - Project 3"),
             
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
                                            max = 500,step=10)
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
                                        #DTOutput('tblTestSplit'),
                                        h5("Multiple Linear Regression - Summary Statistics"),
                                        textOutput("modelFitTextMLR"),
                                        verbatimTextOutput(outputId = "modelSummaryMLR"),
                                        h5("Regression Tree - Summary Statistics"),
                                        textOutput("modelFitTextRT"),
                                        verbatimTextOutput(outputId = "modelSummaryRT"),
                                        plotOutput("modelPlotRTree"),
                                        h5("Random Forest - Summary Statistics"),
                                        textOutput("modelFitTextRT"),
                                        verbatimTextOutput(outputId = "modelSummaryRF")
                                    ),
                            tabPanel("Prediction",
                                        h3("Model Prediction tab panel"),
                                        uiOutput("modelPrediction")
                                    )
                                )#End tabSetPanel
                          )#End mainPanel
             )
        ),#End Modeling Section
        
        tabPanel("Data",fuild=TRUE,
             # Application title
             titlePanel("ST558 - Project 3"),
             
             # Sidebar with a slider input for number of bins
             sidebarLayout(
                 sidebarPanel(
                     h4("Subset the Data"),
                     sliderInput("numRows", label = "Subset the Rows", min = 1, max = 2000, value = c(1, 2000)),
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
