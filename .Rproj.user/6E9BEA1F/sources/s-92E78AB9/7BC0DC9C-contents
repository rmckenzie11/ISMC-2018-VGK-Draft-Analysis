#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

## Load necessary libraries

## Load libraries for mapping

library(leaflet)
library(raster)
library(rgeos)

## Load libraries for data tidying

library(tidyverse)
library(data.table)
library(fs)
library(rgdal)
library(chron)

## Load libraries for Shiny

library(shiny)
library(shinythemes)
library(stargazer)

## Import Taxi_Data, pre-filtered to a random sample of 500,000 cab rides

cab_data18 <- read_rds("taxi_data")

## Select Cerulean Theme for simplistic blue and white theme

ui <- fluidPage(
  theme = shinytheme("cerulean"),

  ## For ease of navigation, create navbar

  navbarPage(
    "NYC Taxi Commission Analysis",

    ## Create first tab, Leaflet, as first and most interesting visualization

    tabPanel(
      "Distribution Map of NYC",

      sidebarLayout(
        sidebarPanel(

          ## Create Filters for the Map by Pickup and Dropoff zone

          selectInput("pick", "Pickup/Dropoff Zone: ", choices = c("Pickup", "Dropoff"), selected = "Pickup")
        ),




        # Show a plot of the generated leaflet
        
        mainPanel(
          leafletOutput("taxiMap")
        )
      )
    ),

    ## Create tab of visualizations of tips

    tabPanel(
      "Tipping Analysis",

      sidebarLayout(
        sidebarPanel(

          ## Allow users to select between three selected variables, chosen because they create effective visualizations

          selectInput("model", "Independent Variable: ", choices = c("Ride Distance", "Pickup Time", "Number of Passengers"), selected = "Ride Distance"),

          ## Help Text to clarify variables

          tags$h5(helpText("Please select a factor to analyze relationship with the amount passengers tip their cab driver.")),
          tags$h6(helpText(
            "Distance refers to the length, in miles, of the ride,",
            "Pickup Time refers to how much time the ride took from pickup to dropoff,",
            "Number of Passengers refers to whether the passenger hailed a cab alone or with another person/group of people."
          )),
          
          # br() element to introduce extra vertical spacing
          
          br()
        ),

        ## Plot Tipping Visualizations in Main Panel
        
        mainPanel(
          plotOutput("tipPlot")
        )
      )
    ),
    
    ## Create third tab, analysis of the tipping variables
    
    tabPanel(
      "Stat Analysis",

      ## Show Selection Variables
      
      sidebarLayout(
        sidebarPanel(
          selectInput("reg", "Independent Variable: ", choices = c("Ride Distance", "Pickup Time", "Number of Passengers"), selected = "Ride Distance")
        ),
        mainPanel(
          
          ##Display Title first
          
          h2("Summary of the Independent Variable's Relationship with Tip Percentage"),
          
          ##Then display HTML-format summary statistics
          
          htmlOutput("sum"),
          
          ## Credit creator of stargazer package
          
          h6("Hlavac, Marek (2018). stargazer: Well-Formatted Regression and Summary Statistics Tables.
 R package version 5.2.2. https://CRAN.R-project.org/package=stargazer")
        )
      )
    )
  )
)


# Define server logic 

server <- function(input, output) {
  
  ## Render map for Tab 1
  
  output$taxiMap <- renderLeaflet({
    
    ##Import tiles from provided data
    
    tiles <- readOGR("taxi_zones", "taxi_zones")
    
    ##Center tiles on NYC 

    projection(tiles) <- "+init=epsg:2263"
    
    ##Transform tiles so they work in leaflet
    
    tiles2 <- spTransform(tiles, "+init=epsg:4326")

    ##Generate vector of zone labels, to be displayed later
    
    labels <- tiles2$zone
    
    ## Introduce reactive filter to select either pickup or dropoff

    ride_type <- reactive({
      
      ## Filter by Selection, then complete and count the number of rides by Taxi Zone
      
      if (input$pick == "Pickup") {
        zone_count <- cab_data18 %>%
          dplyr::select(V8, V9) %>%
          complete(V9 = 1:263) %>%
          count(V9)
      } else if (input$pick == "Dropoff") {
        zone_count <- cab_data18 %>%
          dplyr::select(V8, V9) %>%
          complete(V9 = 1:263) %>%
          count(V9)
      }
      
      ##Make reactive a vector, instead of a dataframe, so it can be easily joined with tiles data
      
      return(zone_count$n)
    })
    
    ## Create reactive color pallete to use as count representation  

    col <- reactive({
      
      ##Differentiate between Pickup and Dropoff palettes for more effective visualization
      
      if (input$pick == "Pickup") {
        col <- "Purples"
      } else if (input$pick == "Dropoff") {
        col <- "Oranges"
      }
      return(col)
    })
    
    ## Turn count observations into logarithmic observations for easier visualization

    zone_count <- log(ride_type(), base = 10)

    ## Add count character vector to main tiles data
    
    tiles2@data <- tiles2@data %>%
      mutate(count = zone_count)

    ##Make color palette based off of reactive col
    
    pal <- colorNumeric(
      palette = col(),
      domain = tiles2$count
    )

    ## Create leaflet of taxi_data
    
    leaflet(tiles2) %>%
      addTiles() %>%
      addPolygons(
        
        ##Set opacity to 0.75 so base layer of map can still be seen for high volume areas, introduce zone labels, set color to reactive palette, set highlight options to make map effective
        
        fillOpacity = 0.75, weight = 3, label = labels, color = ~pal(count),
        highlightOptions = highlightOptions(color = "white", weight = 5, bringToFront = TRUE)
      )
  })

  
  ##Create tips datac from main data frame for easy manipulation

  tips <- cab_data18 %>%
    
    ##Rename columns to be clear and create a numerical time variable
    
    mutate(V3 = as.POSIXct(V3, tz = "", format = "%Y-%m-%d %H:%M:%OS")) %>%
    separate("V3", into = c("date", "time"), sep = " ") %>%
    rename("Time of Pickup" = time, "Number of Passengers" = V4, "Ride Distance" = V5, "Payment Type" = V10, "tip" = V14, "total" = V17) %>%
    mutate(tip_per = tip / total) %>%
    filter(!is.na(tip_per)) %>%
    mutate(`Time of Pickup` = as.numeric(chron(times = `Time of Pickup`)))

  ##Determine mean tip_per
  
  mean_tip_per <- tips %>%
    na.omit(tip_per) %>%
    summarize(mean(tip_per))
  
  ## Determine mean tip amount

  mean_tip <- tips %>%
    summarize(mean(tip))
  
  ##Create Plot Output of Data

  output$tipPlot <- renderPlot({
    
    ##Filter plot data by selected variable
    
    if (input$model == "Number of Passengers") {
      
      ##Filter Out Variable Specific False Data
      
      tips %>%
        filter(
          !is.na(`tip_per`),
          `Number of Passengers` != "0"
        ) %>%
        group_by(`Number of Passengers`) %>%
        
        ##For bar graph visualization, create summary variable for each passenger number
        
        summarize(n = mean(tip_per)) %>%
        ggplot(aes(x = `Number of Passengers`, y = n)) +
        geom_col() +
        labs(x = "Number of Passengers", y = "Average Tip Percent", title = "Tip Percent by Number of Passengers per Ride", caption = "Data from NYc Taxi Commission")
    }
    else if (input$model == "Ride Distance") {
      
      ##Create scatterplot and smoothed line of Ride Distance Variable and Tip Percentage
      
      tips %>%
        ggplot(aes(x = `Ride Distance`, y = tip_per)) +
        geom_point() +
        geom_smooth(method = "lm") +
        labs(x = "Ride Distance", y = "Average Tip Percent", title = "Tip Percent by Ride Distance", caption = "Data from NYc Taxi Commission")
    }
    else if (input$model == "Pickup Time") {
      
      ##Create scatterplot and smoothed line of Pickup Time and Tip Percentage
      
      tips %>%
        ggplot(aes(x = `Time of Pickup`, y = tip_per)) +
        geom_point() +
        labs(x = "Time of Pickup", y = "Average Tip Percent", title = "Tip Percent by Pickup Time", caption = "Data from NYc Taxi Commission") +
        geom_smooth(method = "lm")
    }
  })
  
  ##Create third output, summary statistics
  
  output$sum <- renderUI({
    
    ##Create a reactive model of the selected variable
    
    model <- reactive(
      if (input$reg == "Number of Passengers") {
        model <- lm(formula = V14 ~ V4, data = cab_data18)
      }
      else if (input$reg == "Ride Distance") {
        model <- lm(tip_per ~ `Ride Distance`, tips)
      }
      else if (input$reg == "Pickup Time") {
        model <- lm(formula = tip_per ~ `Time of Pickup`, data = tips)
      }
    )

    ##HTMl Output of Summary Statistics
    
    HTML(stargazer(model(),
      type = "html"
    ))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
