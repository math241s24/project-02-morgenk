library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(readr)

atla <- read_csv("EXTRA/data/atla.csv")

ui <- fluidPage(
  titlePanel("Title"),
  sidebarLayout(
    sidebarPanel(
      selectInput("director", "Select Director", choices = c("All Directors", unique(atla$director)), multiple = TRUE, selected = "All Directors"),
      actionButton("updateBtn", "Update Results!")
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
    filtered_data <- atla %>%
      filter(if ("All Directors" %in% input$director) TRUE else director %in% input$director)
    
    plot <- ggplot(filtered_data %>%
                  mutate(Book = factor(Book, levels = c('Water', 'Earth', 'Fire'))), 
                  aes(x = Episode, y = imdb_rating, color = Book, text = director)) +
      geom_point() + 
      geom_line() +
      labs(x = "Episode Number",
           y = "Number of Per Episode",
           color = "Book (season)") +
      scale_color_manual(values = book_colors, breaks = c("Water", "Earth", "Fire"), limits = c("Water", "Earth", "Fire")) +
      coord_cartesian(xlim = c(0, max(atla$Episode)), ylim = c(5, 10)) + 
      scale_x_continuous(breaks = seq(0, 60, by = 10)) + 
      scale_y_continuous(breaks = seq(5, max(10, by = 1)))
      
      ggplotly(plot)

  })
}

shinyApp(ui = ui, server = server)
