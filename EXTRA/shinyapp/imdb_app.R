library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(readr)
library(RColorBrewer)
library(tidyverse)
library(stringr)

atla <- read_csv("atla.csv")

ui <- fluidPage(
  titlePanel("Avatar: The Last Airbender IMDB Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("director", "Select Director", choices = c("All Directors", unique(atla$director)), multiple = TRUE, selected = "All Directors"),
      submitButton("Update Results!")),
    mainPanel(
      plotlyOutput("plot")
      )
    )
  )

server <- function(input, output, session) {
  
  book_colors <- c("Water" = "#1f77b4", 
                   "Earth" = "#2ca02c", 
                   "Fire" = "#ad0000") 
  
  plot_data <- reactive({
    
    filtered_data <- atla %>%
      filter(if ("All Directors" %in% input$director) TRUE else director %in% input$director)
    
    palette_name <- "Set1"
    director_palette <- brewer.pal(n = 10, name = palette_name)
    
    plot <- ggplot(filtered_data %>%
                     mutate(Book = factor(Book, levels = c("Water", "Earth", "Fire"))),
                   aes(x = Episode, y = IMDB)) 
    
    if (length(input$director) == 1) {
      plot <- plot +
        geom_point(aes(color = Book,  text = paste("Director:", director))) +
        scale_color_manual(values = book_colors, breaks = c("Water", "Earth", "Fire"), limits = c("Water", "Earth", "Fire")) +
        geom_line(aes(color = Book)) +
        geom_smooth(aes(color = "Smooth"), size = 0.5, alpha = 0.2)
    } else {  
      plot <- plot +
        geom_point(aes(color = director, text = paste("Director:", director))) +
        scale_color_manual(values = director_palette) +  
        geom_line(aes(color = director)) 
    }
    
    plot <- plot +
      labs(x = "Episode Number",
           y = "Episode IMDB Score",
           color = if (length(input$director) == 1) "Book (season)" else "Character",
           title = "IMDB Rating per Episode") +
      coord_cartesian(xlim = c(0, max(filtered_data$Episode)), ylim = c(5, 10)) + 
      scale_x_continuous(breaks = seq(0, 61, by = 10)) + 
      scale_y_continuous(breaks = seq(5, 10, by = 1))
    
    return(plot)
  })
  
  output$plot <- renderPlotly({
    plot_data_value <- plot_data()
    plotly::ggplotly(plot_data_value)
  })
}


shinyApp(ui = ui, server = server)
