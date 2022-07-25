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
library(caret)
library(shinythemes)
library(DT)

#Grab the data from the exported CSV file
stockResults <- read_csv("../MSFTStockData.csv")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    #Data Exploration Section
    ##Graphical Summaries
    output$DEPlot <- renderPlot({
        if(input$rdoPlotType == "histo"){
            ###Histogram
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
            g + geom_histogram(breaks = bins) +
                labs(title = paste0("Histogram of ",xLabel), x = xLabel, y = "Frequency")
            #ggplotly(g + geom_histogram(breaks = bins) +
            #             labs(title = paste0("Histogram of ",xLabel), x = xLabel, y = "Frequency"))
        }else {
            ###Scatter Plot
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
            g + geom_point() +
                labs(title = paste0("Scatterplot for Trading Date and ", yLabel), x = "Trading Date", y = yLabel)
            #ggplotly(g + geom_point() +
            #         labs(title = paste0("Scatterplot for Trading Date and ", yLabel), x = "Trading Date", y = yLabel))
        }
    })
    
    ##Numerical Summaries
    getData <- reactive({
        var <- input$si_var
        newData <- stockResults[,c("Symbol","Name","tDate",var)]
    })
    
    output$tbl = DT::renderDataTable(
        getData()
    )

})
