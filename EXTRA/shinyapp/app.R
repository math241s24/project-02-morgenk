library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(readr)

ui <- fluidPage(
  titlePanel("Avatar: The Last Airbender Character Count"),
  sidebarLayout(
    sidebarPanel(
      selectInput("character", "Select Character", choices = unique(atla$character)),
      selectInput("director", "Select Director", choices = c("All Directors", unique(atla$director))),
      submitButton("Update Results!"),
    ),
    mainPanel(
      plotlyOutput("plot")
    )
  )
)

server <- function(input, output) {
  
  book_colors <- c("Water" = "#1f77b4", 
                   "Earth" = "#2ca02c", 
                   "Fire" = "#ad0000") 
  
  output$plot <- renderPlotly({
    selected_characters <- if(is.character(input$character)) input$character else as.character(input$character)
    # Filter the data for the selected characters and director
    filtered_data <- atla %>%
      filter(character %in% selected_characters,
             if (input$director != "All Directors") director == input$director else TRUE)
    
    # Create ggplot object
    p <- ggplot(filtered_data, aes(x = ep, y = character_count, color = book)) +
      geom_point() + 
      geom_line() +
      scale_color_manual(values = book_colors)
    
    # Convert ggplot to plotly
    ggplotly(p)
  })
}

shinyApp(ui = ui, server = server)
