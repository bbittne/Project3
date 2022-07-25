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
library(plotly)

#Grab the data from the exported CSV file
stockResults <- read_csv("../MSFTStockData.csv")
#print(stockResults)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("cerulean"),

    tabsetPanel(
        
        tabPanel("About",
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
        
        tabPanel("Data Exploration",
             # Application title
             titlePanel("ST558 - Project 3"),
             h3("Data Exploration"),
             
             # Sidebar with a slider input for number of bins
             sidebarLayout(
                 sidebarPanel(
                     
                     h4("Graphical Summaries"),
                     
                     radioButtons(inputId = "rdoPlotType", label = "Plot Type",
                                  choiceValues = c("histo", "scatter" ),
                                  choiceNames = c("Histogram", "Scatter Plot")),
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
                     h4("Numerical Summaries")
                 ),
                 
                 # # Show a plot of the generated distribution
                 mainPanel(
                     h3("Graphical Summaries"),
                     plotlyOutput("DEPlot")
                     #plotlyOutput("scatterPlot"),
                     #plotlyOutput("histogramPlot")
                 )
                 
                 # mainPanel(h2("Graphical Summary"),
                 #           plotlyOutput("scatterPlot"),
                 #           br(),
                 #           h2("Numerical Summaries"),
                 #           dataTableOutput("exploreSummary"),
                 #           h2("Data Set"),
                 #           dataTableOutput("exploreData")
                 # ) 
                 
                 
             )
        ),#End Data Exploration Section
        
        tabPanel("Modeling",
             # Application title
             titlePanel("ST558 - Project 3"),
             
             # Sidebar with a slider input for number of bins
             sidebarLayout(
                 sidebarPanel(
                     sliderInput("bins",
                                 "Number of bins:",
                                 min = 1,
                                 max = 50,
                                 value = 30)
                 ),
                 
                 # Show a plot of the generated distribution
                 mainPanel(
                     # Output: Tabset w/ plot, summary, and table ----
                     tabsetPanel(type = "tabs",
                                 tabPanel("Modeling Info", plotOutput("plot")),
                                 tabPanel("Model Fitting", verbatimTextOutput("summary")),
                                 tabPanel("Prediction", tableOutput("table"))
                     )
                 )
             )
        ),#End Modeling Section
        
        tabPanel("Data",
             # Application title
             titlePanel("ST558 - Project 3"),
             
             # Sidebar with a slider input for number of bins
             sidebarLayout(
                 sidebarPanel(
                     sliderInput("bins",
                                 "Number of bins:",
                                 min = 1,
                                 max = 50,
                                 value = 30)
                 ),
                 
                 # Show a plot of the generated distribution
                 mainPanel(
                     plotOutput("distPlot")
                 )
             )
        )#End Data Section
        
    )
))
