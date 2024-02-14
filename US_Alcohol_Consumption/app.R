

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
  selectInput("state", "State", c("All", #
                                  "Alabama", #
                                  "Alaska", #
                                  "Arizona", #
                                  "Arkansas", #
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
  
  # Create plot
  output$US_alc_Consumption <- renderPlot({
    
    # for all states
    if(input$state == "All"){
      # for Beer
      if(input$style == "Beer"){
        statesALC <- 
          alc %>% 
          group_by(State) %>% 
          summarise(
            totalBeer = mean(`Beer (Per capita consumption)`)
            )
        
        plt = ggplot(statesALC, aes(x = State, y = totalBeer)) +
          geom_bar(stat = "identity") +
          theme(axis.text.y.left = element_text(angle = 45, vjust = 1)) +
          
          theme_minimal() +
          labs(x = "State", y = "Beer Consumption") +
          coord_flip()
      }
      
      # for Wine
      if(input$style == "Wine"){
        statesALC <- 
          alc %>% 
          group_by(State) %>% 
          summarise(
            totalWine = mean(`Wine (Per capita consumption)`)
            )
        
        plt = ggplot(statesALC, aes(x = State, y = totalWine)) +
          geom_bar(stat = "identity") +
          theme_minimal() +
          labs(x = "State", y = "Wine Consumption") +
          coord_flip()
      }
      
      # for spirits
      if(input$style == "Spirits"){
        statesALC <- 
          alc %>% 
          group_by(State) %>% 
          summarise(
            totalSpirits = mean(`Spirits (Per capita consumption)`)
            )
        
        plt = ggplot(statesALC, aes(x = State, y = totalSpirits)) +
          geom_bar(stat = "identity") +
          theme_minimal() +
          labs(x = "State", y = "Spirits Consumption") +
          coord_flip()
      }
      
      # for all types
      if(input$style == "All Types"){
      
      statesALC <- 
        alc %>% 
        group_by(State) %>% 
        summarise(
          totalAll = mean(`All beverages (Per capita consumption)`)
          )
      
      plt = ggplot(statesALC, aes(x = State, y = totalAll)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        labs(x = "State", y = "All Alcohol Consumption") +
        coord_flip()
      }
    }else{

    # for Alabama
    if(input$state == "Alabama"){
      # for Beer
      if(input$style == "Beer"){
        alabamaALC <- 
          alc %>% 
          filter(State == "Alabama") %>% 
          summarise(
            totalBeer = mean(`Beer (Per capita consumption)`)
            )
        
        plt = ggplot(alabamaALC, aes(x = Year, y = totalBeer)) +
          geom_point() +
          theme_minimal() +
          labs(x = "State", y = "Beer Consumption") +
          coord_flip()
      }
      
      # for Wine
      if(input$style == "Wine"){
        alabamaALC <- 
          alc %>% 
          filter(State == "Alabama") %>% 
          summarise(
            totalWine = mean(`Wine (Per capita consumption)`)
            )
        
        plt = ggplot(alabamaALC, aes(x = Year, y = totalWine)) +
          geom_point() +
          theme_minimal() +
          labs(x = "State", y = "Wine Consumption") +
          coord_flip()
      }
      
      # for spirits
      if(input$style == "Spirits"){
        alabamaALC <- 
          alc %>% 
          filter(State == "Alabama") %>% 
          summarise(
            totalSpirits = mean(`Spirits (Per capita consumption)`)
            )
        
        plt = ggplot(alabamaALC, aes(x = Year, y = totalSpirits)) +
          geom_point() +
          theme_minimal() +
          labs(x = "State", y = "Spirits Consumption") +
          coord_flip()
      
      }
      
    }
    }
    plt
  })
}

# Run the application 
shinyApp(ui = ui, server = server)









