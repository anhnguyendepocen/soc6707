library(tidyverse)
d <- read_rds("data/ON_mortality.RDS")

# Use a fluid Bootstrap layout
fluidPage(    
  
  # Give the page a title
  titlePanel("Ontario mortality"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    sidebarPanel(
      sliderInput("year",
                  "Year:",
                  value = 1960,
                  min = min(d$year),
                  max = max(d$year), sep = ""),
      checkboxInput("addGompertz", "Add Gompertz fit", FALSE)
      ),
    
    # Create a spot for the plot
    mainPanel(
      plotOutput("hazardPlot")  
    )
    
  )
)