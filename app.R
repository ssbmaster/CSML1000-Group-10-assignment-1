#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Load the model here, once per R session; most efficient outside of server function


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NYC Crash Predictor"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("crashmonth",
                        "Pick a month:",
                        min = 1,
                        max = 12,
                        value = 6,
                        step = 1),
            radioButtons("crashdayofweekorday",
                         "Predict by day of the week or a specific day:",
                         c("Day of the week" = "dayofweek",
                           "Specific day" = "day")),
            selectInput("crashdayofweek",
                        "Pick a day of the week:",
                        c("Monday" = 1,
                          "Tuesday" = 2,
                          "Wednesday" = 3,
                          "Thursday" = 4,
                          "Friday" =  5,
                          "Saturday" = 6,
                          "Sunday" = 7)),
            sliderInput("crashday",
                        "Pick a day:",
                        min = 1,
                        max = 31,
                        value = 15),
            sliderInput("crashhour",
                        "Pick a time:",
                        min = 0,
                        max = 24,
                        value = 12,
                        step = 0.5),
            actionButton("predictbutton",
                         "Predict!")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            
            # Can use tags$xxx() to represent xxx html tags, HTML("html stuff") to interpret HTML
            tags$p("Check out our:",
                   tags$a(href = "https://github.com/patrick-osborne/CSML1000-Group-10-assignment-1", "Github")),
            tags$img(height = 300,
                     width = 400,
                     src = "xxx.png"),
            plotOutput("map")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    crashmonth <- reactive({input$crashmonth})
    crashday <- isolate({input$crashday})
    
    # run code with eventReactive() or observeEvent()? hmmmm.
    observeEvent(input$predictbutton, {print(input$crashmonth)}) #, input$crashday, input$crashdayofweek, input$crashdayofweekorday, input$crashhour)})
    
    output$map <- renderPlot({print(crashmonth())})

}

# Run the application 
shinyApp(ui = ui, server = server)
