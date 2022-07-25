#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(httr)
library(jsonlite)
library(caret)
library(shinythemes)
library(plotly)

#Grab the data from the exported CSV file
stockResults <- read_csv("../MSFTStockData.csv")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    
    #Data Exploration Section
    output$DEPlot <- renderPlotly({
        if(input$rdoPlotType == "histo"){
            ##Histogram
            histVar<-input$histoVariable
            if (histVar=="o"){
                xLabel="Opening Price"
            }else if (histVar=="c") {
                xLabel="Closing Price"
            }else if (histVar=="vw") {
                xLabel="Volume Weighted Avg Price"
            }else {
                xLabel="Aw Snap!"
            }
            x<-stockResults[[histVar]]
            bins <- seq(min(x), max(x), length.out = input$bins + 1)
            g <- ggplot(data = stockResults, aes(x = c))
            ggplotly(g + geom_histogram(breaks = bins) +
                         labs(title = paste0("Histogram of ",xLabel), x = xLabel, y = "Frequency"))
        }else {
            ##Scatter Plot
            scatVar<-input$scatVariable
            if (scatVar=="n"){
                yLabel="Number of Transactions"
            }else if (scatVar=="c") {
                yLabel="Closing Price"
            }else if (scatVar=="v") {
                yLabel="Trading Volume"
            }else {
                yLabel="Aw Snap!"
            }
            y<-stockResults[[scatVar]]
            g <- ggplot(data = stockResults, aes(x = tDate, y = y))
            ggplotly(g + geom_point() +
                         labs(title = paste0("Scatterplot for Trading Date and ", yLabel), x = "Trading Date", y = yLabel))
        }
    })
    

    
    # ##Scatter Plot
    # output$scatterPlot <- renderPlotly({
    #     scatVar<-input$scatVariable
    #     if (scatVar=="n"){
    #         yLabel="Number of Transactions"
    #     }else if (scatVar=="c") {
    #         yLabel="Closing Price"
    #     }else if (scatVar=="v") {
    #         yLabel="Trading Volume"
    #     }else {
    #         yLabel="Aw Snap!"
    #     }
    #     y<-stockResults[[scatVar]]
    #     g <- ggplot(data = stockResults, aes(x = tDate, y = y))
    #         ggplotly(g + geom_point() +
    #             labs(title = paste0("Scatterplot for Trading Date and ", yLabel), x = "Trading Date", y = yLabel))
    # })
    # 
    # ##Histogram
    # output$histogramPlot <- renderPlotly({
    #     histVar<-input$histoVariable
    #     if (histVar=="o"){
    #         xLabel="Opening Price"
    #     }else if (histVar=="c") {
    #         xLabel="Closing Price"
    #     }else if (histVar=="vw") {
    #         xLabel="Volume Weighted Avg Price"
    #     }else {
    #         xLabel="Aw Snap!"
    #     }
    #     x<-stockResults[[histVar]]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #     g <- ggplot(data = stockResults, aes(x = c))
    #     ggplotly(g + geom_histogram(breaks = bins) +
    #                  labs(title = paste0("Histogram of ",xLabel), x = xLabel, y = "Frequency"))
    # })
    


})
