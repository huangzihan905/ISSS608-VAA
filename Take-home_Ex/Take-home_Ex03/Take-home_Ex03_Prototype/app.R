pacman::p_load(tidyverse, jsonlite, SmartEDA, tidygraph, ggraph, plotly, treemapify, visNetwork, RColorBrewer, DT)


# Load and prepare graph data
kg <- fromJSON("MC1_graph.json")

nodes_tbl <- as_tibble(kg$nodes)
edges_tbl <- as_tibble(kg$links)

id_map <- tibble(id = nodes_tbl$id, index = seq_len(nrow(nodes_tbl)))

edges_tbl <- edges_tbl %>%
  left_join(id_map, by = c("source" = "id")) %>%
  rename(from = index) %>%
  left_join(id_map, by = c("target" = "id")) %>%
  rename(to = index) %>%
  filter(!is.na(from), !is.na(to))

of_songs <- nodes_tbl %>%
  filter(`Node Type` == "Song", genre == "Oceanus Folk")

influence_types <- c("InStyleOf", "CoverOf", "LyricalReferenceTo", "InterpolatesFrom", "DirectlySamples")

influence_edges <- edges_tbl %>%
  filter(`Edge Type` %in% influence_types)

# Precompute influence over time
influence_over_time <- influence_edges %>%
  filter(source %in% of_songs$id) %>%
  left_join(nodes_tbl, by = c("target" = "id")) %>%
  filter(!is.na(release_date)) %>%
  mutate(release_year = as.integer(substr(release_date, 1, 4))) %>%
  count(release_year, name = "n") %>%
  filter(!is.na(release_year))

# UI
ui <- fluidPage(
  titlePanel("Oceanus Music Influence Dashboard"),
  tabsetPanel(id = "main_tab",
              
              tabPanel("Data Preparation", 
                       h4("Upload and clean your data here.")
              ),
              
              tabPanel("Exploratory Data Analysis", 
                       h4("Visualize and summarize dataset.")
              ),
              
              tabPanel("Sailor Shift Influence", 
                       h4("Network graph and influence tracking for Sailor Shift.")
              ),
              tabPanel("Oceanus Folk Influence",
                       navlistPanel(widths = c(3, 9),
                                    "Oceanus Folk Influence Analysis",
                                    tabPanel("Oceanus Folk Influence on Others",
                                             sliderInput("year_range", "Select Year Range:",
                                                         min = min(influence_over_time$release_year, na.rm = TRUE),
                                                         max = max(influence_over_time$release_year, na.rm = TRUE),
                                                         value = range(influence_over_time$release_year),
                                                         sep = "", step = 1),
                                             plotlyOutput("of_influence_plot")),
                                    tabPanel("Others Influence on Oceanus Folk",
                                             h4("TBD: Visualizing how others influenced Oceanus Folk")))),
              tabPanel("Rising Star Forecasting", 
                       h4("Predictions of future rising stars."))))


# Server
server <- function(input, output, session) {
  filtered_data <- reactive({
    influence_over_time %>%
      filter(release_year >= input$year_range[1],
             release_year <= input$year_range[2])
  })
  
  output$of_influence_plot <- renderPlotly({
    plot_ly(filtered_data(),
            x = ~release_year,
            y = ~n,
            type = 'scatter',
            mode = 'lines+markers',
            line = list(color = 'steelblue'),
            marker = list(size = 6, color = 'black'),
            text = ~paste("Year:", release_year, "<br>Influenced Songs:", n),
            hoverinfo = 'text') %>%
      layout(title = "Spread of Oceanus Folk Influence Over Time",
             xaxis = list(title = "Release Year of Influenced Work"),
             yaxis = list(title = "Number of Influenced Songs"),
             hoverlabel = list(bgcolor = "white"))
  })
}

shinyApp(ui, server)
