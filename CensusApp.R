library(shiny)
library(tidyverse)
library(tidycensus)
library(ggplot2)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("stateID", "Choose a state:",
                  list("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL",
                       "IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT",
                       "NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI",
                       "SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY"),
                  selected = "NJ"),
      
      radioButtons("metricID", "Choose a plot Metric:", 
                   choices = c("Median household income", "Median gross rent",
                               "Median gross rent / Median household income"),
                   selected = "Median gross rent / Median household income")),
    mainPanel(
      plotOutput("main_plot")
    )
  ),
  titlePanel("American Community Survey Map")
)
server <- function(input, output, session) {
  reduced_df <- reactive ({
#    print("Printign estimate value: ")
#    print(estimate)
    get_acs(geography = "county", 
            variables = c(medincome = "B19013_001", medrent = "B25064_001"), 
            state = input$stateID, output = "wide") %>% 
      mutate("Gross rent/Household income" = medrentE / medincomeE) %>%
      mutate(region = as.integer(GEOID))
  })
  
  output$main_plot <- renderPlot({
    state_sh <- switch(input$stateID, 
                       "AL" = "ALABAMA", "AK" = "ALASKA", "AZ" = "ARIZONA", "AR" = "ARKANSAS", "CA" = "CALIFORNIA",
                       "CO" = "COLORADO", "CT" = "CONNECTICUT", "DE" = "DELAWARE", "FL" = "FLORIDA", "GA" = "GEORGIA",
                       "HI" = "HAWAII", "ID" = "IDAHO", "IL" = "ILLINOIS", "IN" = "INDIANA", "IA" = "IOWA", "KS" = "KANSAS",
                       "KY" = "KENTUCKY", "LA" = "LOUISIANA", "ME" = "MAINE", "MD" = "MARYLAND", "MA" = "MASSACHUSETTS",
                       "MI" = "MICHIGAN", "MN" = "MINNESOTA", "MS" = "MISSISSIPPI", "MO" = "MISSOURI", "MT" = "MONTANA",
                       "NE" = "NEBRASKA", "NV" = "NEVADA", "NH" = "NEW HAMPSHIRE", "NJ" = "new jersey", "NM" = "NEW MEXICO",
                       "NY" = "NEW YORK", "NC" = "NORTH CAROLINA", "ND" = "NORTH DAKOTA", "OH" = "OHIO", "OK" = "OKLAHOMA",
                       "OR" = "OREGON", "PA" = "PENNSYLVANIA", "RI" = "RHODE ISLAND", "SC" = "SOUTH CAROLINA", "SD" = "SOUTH DAKOTA",
                       "TN" = "TENNESSEE", "TX" = "TEXAS", "UT" = "UTAH", "VT" = "VERMONT", "VA" = "VIRGINIA", "WA" = "WASHINGTON",
                       "WV" = "WEST VIRGINIA", "WI" = "WISCONSIN", "WY" = "WYOMING"
    )
    
    
    estimator <- switch(input$metricID,
                       "Median household income" = "medincomeE",
                       "Median gross rent" = "medrentE",
                       "Median gross rent / Median household income" = "Gross rent/Household income"
    )
    
    print(paste0("value of estimate is: ", estimator))
    print(paste0("value of state_sh is: ", state_sh))
    #    print(reduced_df(), n = Inf)
    
    reduced_df() %>%
    select(value = estimator, region)  %>%
      choroplethr::county_choropleth(title = paste0(input$metricID, " distribution"), num_colors = 1, state_zoom = tolower(state_sh))
  })
}
shinyApp(ui = ui, server = server)
