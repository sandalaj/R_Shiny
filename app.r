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
            
            
            #Create action button to plot linear model and associated data values
            actionButton("plotData", "Plot scatterplot"),
            actionButton("addModel", "Plot linear model")
        ),
        
        
        
        # Plotting the scatterplot and associated information
        mainPanel(
            plotOutput("scatter"),
            plotOutput("lmod"),
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
    
    scatterplot <- eventReactive(input$plotData, {
        lm(y ~ x, data = dataInput())
        
    })
    
    output$scatter <- renderPlot( {
        ggplot(scatterplot()$model, aes_string(x = names(scatterplot()$model)[2], y = names(scatterplot()$model)[1])) + 
            theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
            geom_point() +
            ggtitle("Scatterplot") 
        
    })
    
    
    linmod <- eventReactive(input$addModel, {
        lm(y ~ x, data = dataInput())
        
    })
    
    output$lmod <- renderPlot( {
        ggplot(linmod()$model, aes_string(x = names(linmod()$model)[2], y = names(linmod()$model)[1])) + 
            theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
            geom_point() +
            ggtitle("Scatterplot with Linear Model") +
            stat_smooth(method = "lm", col = "blue") +
            geom_label(aes(x = 0, y = 7.5), hjust = 0,
                label = paste("Adj R2 = ",signif(summary(linmod())$adj.r.squared, 5),
                              " \nIntercept =",signif(linmod()$coef[[1]],5 ),
                              " \nSlope =",signif(linmod()$coef[[2]], 5),
                              " \nP =",signif(summary(linmod())$coef[2,4], 5)))
            
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