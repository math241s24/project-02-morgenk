library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(readr)
library(RColorBrewer)
library(tidyverse)
library(stringr)

atla <- read_csv("atla.csv")


ui <- dashboardPage(
  dashboardHeader(title = "Wan Shi Tong's Library"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Character Line Analysis", tabName = "character_analysis", icon = icon("heart")),
      menuItem("IMDB Analysis", tabName = "imdb_analysis", icon = icon("heart"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "character_analysis",
              fluidPage(
                titlePanel("Character Line Analysis"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("character_char", "Select Character(s)", choices = as.vector(unique(atla$character)),  multiple = TRUE, selected = "Aang"),
                    selectInput("director_char", "Select Director", choices = c("All Directors", unique(atla$director)), multiple = TRUE, selected = "All Directors"),
                    submitButton("Update Results!")
                  ),
                  mainPanel(
                    plotlyOutput("plot_char")
                  )
                )
              )
      ),
      tabItem(tabName = "imdb_analysis",
              fluidPage(
                titlePanel("IMDB Analysis"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("director_imdb", "Select Director", choices = c("All Directors", unique(atla$director)), multiple = TRUE, selected = "All Directors"),
                    submitButton("Update Results!")
                  ),
                  mainPanel(
                    plotlyOutput("plot_imdb")
                  )
                )
              )
      ),
  
      tabItem(tabName = "Credits",
              fluidPage(
                titlePanel("Credits and Copyright Information"),
                mainPanel(
                  h3("Copyright Information"),
                  p("test")
                )
              )
    
      )
    )
  ),
  skin = "purple" 
)


server <- function(input, output, session) {
  
  book_colors <- c("Water" = "#1f77b4", 
                   "Earth" = "#2ca02c", 
                   "Fire" = "#ad0000") 
  
  palette_name <- "Set1"
  character_palette <- brewer.pal(n = 10, name = palette_name)
  
  plot_data_char <- reactive({
    filtered_data_line <- atla %>%
      filter(character %in% input$character_char,
             if ("All Directors" %in% input$director_char) TRUE else director %in% input$director_char)
    
    palette_name <- "Set1"
    director_palette <- brewer.pal(n = 10, name = palette_name)
    
    plot_line <- ggplot(filtered_data_line %>%
                          mutate(Book = factor(Book, levels = c("Water", "Earth", "Fire"))),
                        aes(x = Episode, y = Lines))
    validate(
      need(nrow(filtered_data_line) > 0, "No data available for the selected filters."),
      need(length(input$character_char) <= 5, "Please only select up to 10 characters.")
    )  
    
    if (length(input$character_char) == 1) {
      plot_line <- plot_line +
        geom_point(aes(color = Book, text = paste("IMDB:", IMDB, "<br>", "Director:", director))) +
        scale_color_manual(values = book_colors, breaks = c("Water", "Earth", "Fire"), limits = c("Water", "Earth", "Fire")) +
        geom_line(aes(color = Book))
    } else {  
      plot_line <- plot_line +
        geom_point(aes(color = character, text = paste("IMDB:", IMDB, "<br>", "Director:", director))) +
        scale_color_manual(values = character_palette) +  
        geom_line(aes(color = character))
    }
    
    plot_line <- plot_line +
      labs(x = "Episode Number",
           y = "Number of Lines",
           title = "Number of Lines per Episode",
           color = if (length(input$character_char) == 1) "Book (season)" else "Character")+
      coord_cartesian(xlim = c(0, max(filtered_data_line$Episode)), ylim = c(0, max(filtered_data_line$Lines))) + 
      scale_x_continuous(breaks = seq(0, 61, by = 10)) + 
      scale_y_continuous(breaks = seq(0, max(filtered_data_line$Lines), by = 10))
    
    
    return(plot_line)
  })
  
plot_data <- reactive({
  
  filtered_data <- atla %>%
    filter(if ("All Directors" %in% input$director_imdb) TRUE else director %in% input$director_imdb)
  
  validate(
    need(nrow(filtered_data) > 0, "No data available for the selected filters."))

  
  palette_name <- "Set1"
  director_palette <- brewer.pal(n = 10, name = palette_name)
  
  plot <- ggplot(filtered_data %>%
                   mutate(Book = factor(Book, levels = c("Water", "Earth", "Fire"))),
                 aes(x = Episode, y = IMDB)) 
  
  if (length(input$director_imdb) == 1) {
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
         y = "Episode IMDB Rating",
         title = "IMDB Rating per Episode",
         color = if (length(input$director_imdb) == 1) "Book (season)" else "Character") +
    coord_cartesian(xlim = c(0, max(filtered_data$Episode)), ylim = c(5, 10)) + 
    scale_x_continuous(breaks = seq(0, 61, by = 10)) + 
    scale_y_continuous(breaks = seq(5, 10, by = 1))
  
  return(plot)
})

output$plot_char <- renderPlotly({
  plot_data_char_value <- plot_data_char()
  plotly::ggplotly(plot_data_char_value)
})

output$plot_imdb <- renderPlotly({
  plot_data_value <- plot_data()
  plotly::ggplotly(plot_data_value)
})
}

shinyApp(ui = ui, server = server)
