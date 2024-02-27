

library(shiny)
library(tidyverse)

alc <- read_csv("Alcohol_Consumption_US.csv")

states <- c("Alabama",
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
            "Wyoming")


# Define UI for application
ui <- fluidPage(
  tags$style(HTML("body {background-color: #FFB6C1; }")),
  titlePanel(tags$h1("US Alcohol Consumption (Per Capita)", style = "font-weight: bold; text-align: center;")),
  tags$br(),
  tags$p("Author: Matt Maslow", style = "font-size: 16px;  text-align: center; color: #555;"),
  selectInput("style", "Style of Alcohol", c("Beer", "Wine", "Spirits", "All Types")),
  selectInput("state", "Select State(s)", c("All", states)),
  
  radioButtons("organization", label = h3("Type of Organization (For BarPlot of All States)"),
               choices = list("by States", "by Consumption"), 
               selected = 1),
  
  selectInput("order", "Sort Order", c("asc", "desc")),  # Added order input
  
  hr(),
  fluidRow(column(3, verbatimTextOutput("value"))),
  plotOutput("US_alc_Consumption", width = "auto", height = "800px")
)


# Define server logic
server <- function(input, output) {
  output$US_alc_Consumption <- renderPlot({
    
    if (input$state == "All") {
      filtered_data <- switch(input$style,
                              "Beer" = alc %>% group_by(State) %>% summarise(total = mean(`Beer (Per capita consumption)`)),
                              "Wine" = alc %>% group_by(State) %>% summarise(total = mean(`Wine (Per capita consumption)`)),
                              "Spirits" = alc %>% group_by(State) %>% summarise(total = mean(`Spirits (Per capita consumption)`)),
                              "All Types" = alc %>% group_by(State) %>% summarise(total = mean(`All beverages (Per capita consumption)`))
      )
      
      # how to order the data (either the State or the total consumption of selected style of alcohol)
      if (input$organization == "by States") {
        if (input$order == "asc") {
          filtered_data <- filtered_data %>% mutate(State = factor(State, levels = rev(unique(State)))) %>% arrange(State)
        } else {
          filtered_data <- filtered_data %>% mutate(State = factor(State, levels = rev(unique(State)))) %>% arrange(desc(State))
        }
      } else {
        if (input$order == "asc") {
          filtered_data <- filtered_data %>% arrange(total)
        } else {
          filtered_data <- filtered_data %>% arrange(desc(total))
        }
      }
      
      top_states <- filtered_data %>% slice_max(n = 5, order_by = total)
      
      plt <- ggplot(filtered_data, aes(x = State, y = total, fill = ifelse(State %in% top_states$State, "Top 5", "Other"))) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = c("Top 5" = "red", "Other" = "gray")) +
        theme_minimal() +
        labs(x = "State", y = input$style) +
        coord_flip() +
        labs(title = paste("US", input$style, "Consumption")) +
        theme(axis.title = element_text(size = 35)) +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(plot.title = element_text(size = 45)) +
        theme(axis.title = element_text(size = 35)) +
        guides(fill = guide_legend(title = "Top 5 States")) +
        geom_text(aes(label = ifelse(State %in% top_states$State, round(total, 2), "")), hjust = -0.1, size = 5, color = "black")
      
    } else {
      filtered_data <- switch(input$style,
                              "Beer" = alc %>% filter(State == input$state) %>% select(Year, `Beer (Per capita consumption)`) %>% rename(total = `Beer (Per capita consumption)`),
                              "Wine" = alc %>% filter(State == input$state) %>% select(Year, `Wine (Per capita consumption)`) %>% rename(total = `Wine (Per capita consumption)`),
                              "Spirits" = alc %>% filter(State == input$state) %>% select(Year, `Spirits (Per capita consumption)`) %>% rename(total = `Spirits (Per capita consumption)`),
                              "All Types" = alc %>% filter(State == input$state) %>% select(Year, `All beverages (Per capita consumption)`) %>% rename(total = `All beverages (Per capita consumption)`)
      )
      
      order_direction <- ifelse(input$order == "asc", "asc", "desc")
      filtered_data <- filtered_data %>% arrange(desc(total))
      
      plt <- ggplot(filtered_data, aes(x = Year, y = total)) +
        geom_line(color = "red") +
        theme_minimal() +
        labs(title = paste(input$state, input$style, "Consumption"), x = "Year", y = input$style) +
        coord_cartesian() +
        theme(axis.title = element_text(size = 20)) +
        theme(axis.text = element_text(size = 15)) +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(plot.title = element_text(size = 25)) +
        geom_text(aes(label = ifelse(total == max(total) & total != lag(total), round(total, 2), "")), hjust = -0.1, size = 5, color = "black") +
        geom_text(aes(label = ifelse(total == min(total) & total != lag(total), round(total, 2), "")), hjust = -0.1, size = 5, color = "black")
    }
    
    plt
  })
}

# Run the application 
shinyApp(ui = ui, server = server)








