

# https://unc-libraries-data.github.io/R-Open-Labs/Extras/shiny/shiny.html

library(shiny)
library(tidyverse)

alc <- read_csv("Alcohol_Consumption_US.csv")

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("US Alcohol Consumption"),
  
  
  # Type of Alcohol Menu
  selectInput("style", "Style", c("Beer",
                                  "Wine",
                                  "Spirits",
                                  "All Types"
                                  )),
  
  # State Menu
  selectInput("state", "State", c("All",
                                  "Alabama",
                                  "Alaska",
                                  "Arizona",
                                  "Arkansas",
                                  "California",
                                  "Colorado",
                                  "Connecticut",
                                  "Delaware",
                                  "Florida",
                                  "Georgia",
                                  "Hawaii",
                                  "Idaho",
                                  "Illinois",
                                  "Indiana",
                                  "Iowa",
                                  "Kansas",
                                  "Kentucky",
                                  "Louisiana",
                                  "Maine",
                                  "Maryland",
                                  "Massachusetts",
                                  "Michigan",
                                  "Minnesota",
                                  "Mississippi",
                                  "Missouri",
                                  "Montana",
                                  "Nebraska",
                                  "Nevada",
                                  "New Hampshire",
                                  "New Jersey",
                                  "New Mexico",
                                  "New York",
                                  "North Carolina",
                                  "North Dakota",
                                  "Ohio",
                                  "Oklahoma",
                                  "Oregon",
                                  "Pennsylvania",
                                  "Rhode Island",
                                  "South Carolina",
                                  "South Dakota",
                                  "Tennessee",
                                  "Texas",
                                  "Utah",
                                  "Vermont",
                                  "Virginia",
                                  "Washington",
                                  "West Virginia",
                                  "Wisconsin",
                                  "Wyoming"
                                  )),
  
  
  # Bar Chart
  plotOutput("US_alc_Consumption", width = "100%", height = "600px", hover = hoverOpts(id = "plot_hover"))
  
  
)

# Define server logic
server <- function(input, output) {
  
  # Create plot. 
  output$US_alc_Consumption <- renderPlot({
    
    # for all states
    if (input$state == "All") {
      # Filtering based on selected beverage style
      # switch() function in R is used for vectorized conditional branching
      # it allows you to select one of several expressions
      filtered_data <- switch(input$style,
                              "Beer" = alc %>% group_by(State) %>% summarise(total = mean(`Beer (Per capita consumption)`)),
                              "Wine" = alc %>% group_by(State) %>% summarise(total = mean(`Wine (Per capita consumption)`)),
                              "Spirits" = alc %>% group_by(State) %>% summarise(total = mean(`Spirits (Per capita consumption)`)),
                              "All Types" = alc %>% group_by(State) %>% summarise(total = mean(`All beverages (Per capita consumption)`)))
      
      # Plotting for all states
      plt <- ggplot(filtered_data, aes(x = State, y = total)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        labs(x = "State", y = input$style) +
        coord_flip()
        
    } else {
      # Filtering based on selected beverage style and state
      filtered_data <- switch(input$style,
                              "Beer" = alc %>% filter(State == input$state) %>% select(Year, `Beer (Per capita consumption)`) %>% rename(total = `Beer (Per capita consumption)`),
                              "Wine" = alc %>% filter(State == input$state) %>% select(Year, `Wine (Per capita consumption)`) %>% rename(total = `Wine (Per capita consumption)`),
                              "Spirits" = alc %>% filter(State == input$state) %>% select(Year, `Spirits (Per capita consumption)`) %>% rename(total = `Spirits (Per capita consumption)`),
                              "All Types" = alc %>% filter(State == input$state) %>% select(Year, `All beverages (Per capita consumption)`) %>% rename(total = `All beverages (Per capita consumption)`))
      
      # Plotting for individual state as a timeline
      plt <- ggplot(filtered_data, aes(x = Year, y = total)) +
        geom_line(color = "red") +
        theme_minimal() +
        # add a title that includes the State name and the selected beverage style
        labs(title = paste(input$state, input$style, "Consumption"), x = "Year", y = input$style) +
        coord_cartesian() +
        theme(axis.text.x = element_text(size = 7)) +
        theme(axis.text.y = element_text(size = 7)) +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(axis.title = element_text(size = 20))
    }
    
    plt
  })
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)









