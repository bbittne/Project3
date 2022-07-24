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
library(caret)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("cerulean"),

    tabsetPanel(
        
        tabPanel("About",
            # Application title
            titlePanel("ST558 - Project 3"),
        
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
        ),
        tabPanel("Data Exploration",
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
        ),
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
        ),
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
        )
    )
))
