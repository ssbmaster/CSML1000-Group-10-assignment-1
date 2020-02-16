#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rpart)
library(leaflet)
library(ggplot2)

# Load the once per session stuff here; most efficient outside of server/ui functions
load("fittedRegTreeModel.rda")
precinctNum <- read.csv("precinct.csv")
precinctMap <- read_sf('geo_export_32d06294-3e95-408c-86e3-7a17a84f9c0e.shp', stringsAsFactors=FALSE)

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
            
            #pal <- colorNumeric(palette = "Rd", domain = range(1:77, na.rm=T)),
            
            # Use leaflet to map the precincts onto NYC.
            # leaflet(precinctMap) %>%
            #     addTiles() %>%
            #     setView(-74.00, 40.71, zoom = 10)%>%
            #     addPolygons(color = "#666666", weight = 1, smoothFactor = 0.5,
            #                 opacity = 1.0, fillOpacity = 0.5,
            #                 fillColor = ~colorBin("YlOrRd",c(1,100), bins = 3, pretty = TRUE),
            #                 highlightOptions = highlightOptions(color = "white", weight = 2,
            #                                                     bringToFront = TRUE)) %>%
            #     addProviderTiles("CartoDB.Positron"),
            #tags$h3("predictedvals in console right now"),
            leafletOutput("mymap")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # use reactive() to have immediate update from UI interaction, with caching
    # use isolate() to not update from UI interaction
    crashMonth <- reactive({input$crashmonth})
    crashDay <- reactive({input$crashday})
    crashHour <- reactive({input$crashhour})
    crashDayOfWeek <- reactive({input$crashdayofweek})
    
   
    # run code with eventReactive() or observeEvent()? hmmmm.
    #output$predictedval <- 
        observeEvent(input$predictbutton, {
                                # Make a dataframe from the inputs. Week has no consequence to this test model I think, using 1. 
                                # Precinct we iterate starting at 1.
                                precinct <- as.character(precinctNum$Precinct.No)
                                month <- as.integer(crashMonth())
                                week <- as.integer(1)
                                day <- as.integer(crashDay())
                                weekday = as.integer(crashDayOfWeek())
                                hour = as.integer(crashHour())
                                predictMe <- data.frame(precinct, month, week, day, weekday, hour, stringsAsFactors = FALSE)
                                predictedValue <- predict(regTreeModel, predictMe)
                                print(as.data.frame(predictedValue))
                                output$mymap <- renderLeaflet({
                                    leaflet(precinctMap) %>%
                                    addTiles() %>%
                                    setView(-74.00, 40.71, zoom = 10)%>%
                                    addPolygons(color = "#666666", weight = 1, smoothFactor = 0.5,
                                                opacity = 1.0, fillOpacity = 0.5,
                                                fillColor = ~colorBin("YlOrRd",c(1,100), bins = 3, pretty = TRUE),
                                                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                                    bringToFront = TRUE)) %>%
                                    addProviderTiles("CartoDB.Positron")}
                                )
                                })
    
    # The prediction logic and output to UI.
}

# Run the application 
shinyApp(ui = ui, server = server)
