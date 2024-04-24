library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(readr)
library(RColorBrewer)
library(tidyverse)
library(stringr)
library(DT)
library(rsconnect)

atla <- read_csv("atla.csv")

ui <- fluidPage(
  titlePanel("Avatar: The Last Airbender Character Line Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("character", "Select Character(s)", choices = as.vector(unique(atla$character)),  multiple = TRUE, selected = "Aang"),
      selectInput("director", "Select Director", choices = c("All Directors", unique(atla$director)), multiple = TRUE, selected = "All Directors"),
      submitButton("Update Results!")
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
  
  plot_data <- reactive({
    filtered_data <- atla %>%
      filter(character %in% input$character,
             if ("All Directors" %in% input$director) TRUE else director %in% input$director)
    
    palette_name <- "Set1"
    character_palette <- brewer.pal(n = 10, name = palette_name)
    
    validate(
      need(nrow(filtered_data) > 0, "No data available for the selected filters."),
      need(length(input$character) <= 10, "Please only select up to 10 characters.")
    )  
    
    plot <- ggplot(filtered_data %>%
                     mutate(Book = factor(Book, levels = c("Water", "Earth", "Fire"))),
                   aes(x = Episode, y = Lines))
    
    if (length(input$character) == 1) {
      plot <- plot +
        geom_point(aes(color = Book, text = paste("IMDB:", IMDB, "<br>", "Director:", director))) +
        scale_color_manual(values = book_colors, breaks = c("Water", "Earth", "Fire"), limits = c("Water", "Earth", "Fire")) +
        geom_line(aes(color = Book))
    } else {  
      plot <- plot +
        geom_point(aes(color = character, text = paste("IMDB:", IMDB, "<br>", "Director:", director))) +
        scale_color_manual(values = character_palette) +  
        geom_line(aes(color = character))
    }
    
    plot <- plot +
      labs(x = "Episode Number",
           y = "Number of Lines",
           title = "Number of Lines Per Episode",
           color = if (length(input$character) == 1) "Book (season)" else "Character") +
      coord_cartesian(xlim = c(0, max(filtered_data$Episode)), ylim = c(0, max(filtered_data$Lines))) + 
      scale_x_continuous(breaks = seq(0, 61, by = 10)) + 
      scale_y_continuous(breaks = seq(0, max(filtered_data$Lines), by = 10))
    
    return(plot)
  })
  
  output$plot <- renderPlotly({
    plot_data_value <- plot_data()
    plotly::ggplotly(plot_data_value)
  }) 
}

shinyApp(ui = ui, server = server)

