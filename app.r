#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Load required dependencies

library(shiny)
library(ggplot2)

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Linear Modeling"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(

            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
            
            #Create action button to plot the scatterplot
            actionButton("scatter", "Plot scatterplot"),
            
            #Create action button to plot linear model and associated data values
            actionButton("lm", "Plot linear model")
        ),
        
           

        # Plotting the scatterplot and associated information
        mainPanel(
           plotOutput("distPlot"),
           plotOutput("lmPlot"),
           textOutput("R-squared Values"),
           textOutput("rsquared"),
           textOutput("intercept"),
           textOutput("slope"),
           textOutput("pvalue"),
           tableOutput("contents")
        )
    )
)


# Define server logic 
server <- function(input, output) {

    
    dataInput <- reactive({
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df)
    })
    
    #Action button for linear model
    lmPlot <- eventReactive(input$lm, {
        lm(y ~ x, data = dataInput())
    })
    
    #Action button for scatterplot
     distPlot <- eventReactive(input$scatter, {
         plot(x = dataInput()$x, y = dataInput()$y, main = "Scatterplot")
         lm(y ~ x, data = dataInput())
    })   

     output$distPlot <- renderPlot({
         ggplot(distPlot()$model, aes_string(x = names(distPlot()$model)[2], y = names(distPlot()$model)[1])) + 
             geom_point() +
             ggtitle("Scatterplot") +
             theme(plot.title = element_text(hjust = 0.5, face = "bold"))
     })
    


    
    # Create outputs for slope, intercept, correlation coefficient, and p-value for the linear model
    output$lmPlot <- renderPlot({
        ggplot(lmPlot()$model, aes_string(x = names(lmPlot()$model)[2], y = names(lmPlot()$model)[1])) + 
            geom_point() +
            stat_smooth(method = "lm", col = "blue") +
            ggtitle("Linear Model") +
            theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
            geom_label(aes(x = 0, y = 7.5), hjust = 0, 
                       label = paste("Adj R2 = ",signif(summary(lmPlot())$adj.r.squared, 5),
                                     "\nIntercept =",signif(lmPlot()$coef[[1]],5 ),
                                     " \nSlope =",signif(lmPlot()$coef[[2]], 5),
                                     " \nP =",signif(summary(lmPlot())$coef[2,4], 5)))
    })
    
    # Label the graph with the generated values for slope, intercept, correlation coeffcient, and p-value
    output$rsquared <- renderText({
        print(paste("Adj R2 = ",signif(summary(lmPlot())$adj.r.squared, 5)))
    })
    
    output$intercept <- renderText({
        print(paste("Intercept =",signif(lmPlot()$coef[[1]],5)))
    })
    
    output$slope <- renderText({
        print(paste("Slope =",signif(lmPlot()$coef[[2]], 5)))
    })
    
    output$pvalue <- renderText({
        print(paste("P =",signif(summary(lmPlot())$coef[2,4], 5)))
    })
    
    # Show the table of plotted x and y values
    output$contents <- renderTable({
        
        if(input$disp == "head") {
            return(head(dataInput()))
        }
        else {
            return(dataInput())
        }
        
    })
        
}

# Run the application 
shinyApp(ui = ui, server = server)