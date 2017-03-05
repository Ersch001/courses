
library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Which group of cars to choose from"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      helpText("Interested in knowing which group of cars would be interesting for you? 
          Please select from the sliders below the minimum horsepower you would like to have and how many clusters should be created"),
      sliderInput("Horses",
                  "Minimum Horsepower:",
                  min = 0,
                  max = 350,
                  value = c(0,350)),
      checkboxGroupInput("transmission", "Transmission",
                         c("Automatic" = 0,
                           "Manual" = 1),selected = c(0,1)),
      sliderInput("cluster",
                  "Number of clusters:",
                  min = 1,
                  max = 7,
                  value = 3)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("mtCarsPlot")
    )
  )
))
