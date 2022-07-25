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
        ##For complete dataset
        #newData <- stockResults[,c("Symbol","Name","tDate",var)]
        
        var <- input$si_var
        sumOption <- input$rdoSumOption
        
        stockResultsNew<-stockResults %>% mutate(tYear=as.numeric(format(stockResults$tDate, "%Y")))
        newData <- stockResultsNew[,c("Name","tYear",var)]
        if (var=="c"){
            if(sumOption=="mean"){
                newData<-aggregate(c ~ Name+tYear, data = stockResultsNew, FUN = mean)
            }else if(sumOption=="max"){
                newData<-aggregate(c ~ Name+tYear, data = stockResultsNew, FUN = max)
            }else if(sumOption=="min"){
                newData<-aggregate(c ~ Name+tYear, data = stockResultsNew, FUN = min)
            }
        }else if (var=="v"){
            if(sumOption=="mean"){
                newData<-aggregate(v ~ Name+tYear, data = stockResultsNew, FUN = mean)
            }else if(sumOption=="max"){
                newData<-aggregate(v ~ Name+tYear, data = stockResultsNew, FUN = max)
            }else if(sumOption=="min"){
                newData<-aggregate(v ~ Name+tYear, data = stockResultsNew, FUN = min)
            }
        }else if (var=="n"){
            if(sumOption=="mean"){
                newData<-aggregate(n ~ Name+tYear, data = stockResultsNew, FUN = mean)
            }else if(sumOption=="max"){
                newData<-aggregate(n ~ Name+tYear, data = stockResultsNew, FUN = max)
            }else if(sumOption=="min"){
                newData<-aggregate(n ~ Name+tYear, data = stockResultsNew, FUN = min)
            }
        }

    })
    
    output$tbl = DT::renderDataTable(
        getData()
    )

})
