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


    # Modeling
    ## Model Info
    output$modelInfo <- renderUI({
        if(input$rdoSupModel == "MLR"){
            withMathJax(helpText("A Linear Regression Model is the first model type we will look at. These models are an intuitive way to investigate the linear relation between multiple variables. These models make the estimation procedure simple and easy to understand. Linear Regression models can come in all different shapes and sizes and can be used to model more than just a straight linear relationship. Regression models can be modified with interactive and or higher order terms that will conform to a more complex relationship. Multiple Linear Regression can be expressed with the following mathematical expressions similar to the following:
    $$Yi = \\beta_0 + \\beta_1x_{1i} + \\beta_2x_{2i} + + \\beta_3x_{1i}x_{2i}$$"))
        }else if(input$rdoSupModel == "rTree"){
            withMathJax(helpText("Tree based methods will split up the predictor space into separate regions and the predictions will be different for each region. Here we will be doing a Regression Tree so for any given region, we will use the mean of observations as our prediction. Fore every possible value of each predictor, we find the Residual Sum of Squares and try to minimize it:
    $$R_1(j,s) = \\{x|x_j < s\\} \\, and \\, R_2(j,s) = \\{x|x_j \\ge s\\}$$
We seek the value of j and s that minimize the equation:
    $$\\sum_{i:x_i\\in R_1(j,s)} (y_i - \\bar y_{R_1})^2 + \\sum_{i:x_i\\in R_2(j,s)} (y_i - \\bar y_{R_2})^2$$"))
        }else {
            withMathJax(
                helpText("The Random Forest Model is an example of an ensemble based model. Instead of traditional decision trees, ensemble methods average across the tree. This will greatly increase our prediction power, but it will come at the expense of the easy interpretation from traditional decision trees. The Random Forest based model will not use all available predictors. Instead it will take a random subset of the predictors for each tree fit and calculate the model fit for that subset. It will repeat the process a pre-determined number of times and automatically pick the best predictors for the model. This will end up creating a reduction in the overall model variance. All of our models will minimize prediction error via the MSE calculation:
            $$MSE = \\frac{1}{n}\\sum_{i=1}^{n}(y_i - \\hat{y})^2$$"))
        }
    })##End Modeling INfo
    
})
