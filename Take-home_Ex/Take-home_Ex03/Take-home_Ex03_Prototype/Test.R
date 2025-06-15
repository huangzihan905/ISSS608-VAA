# --- Load Required Packages ---
pacman::p_load(tidyverse, jsonlite, SmartEDA, tidygraph, ggraph,
               plotly, treemapify, visNetwork, RColorBrewer, DT, ggthemes)

# --- Load Data ---
kg <- fromJSON("MC1_graph.json")

# --- Extract Nodes and Edges ---
nodes_tbl <- as_tibble(kg$nodes)
edges_tbl <- as_tibble(kg$links)

# --- Map Node IDs to Indices ---
id_map <- tibble(id = nodes_tbl$id, index = seq_len(nrow(nodes_tbl)))
edges_tbl <- edges_tbl %>%
  left_join(id_map, by = c("source" = "id")) %>% rename(from = index) %>%
  left_join(id_map, by = c("target" = "id")) %>% rename(to = index) %>%
  filter(!is.na(from), !is.na(to))

# --- Prepare Oceanus Folk Songs ---
of_songs <- nodes_tbl %>%
  filter(`Node Type` == "Song", genre == "Oceanus Folk")
of_song_ids <- of_songs$id

# --- Prepare Sailor Shift Songs ---
sailor_shift_id <- nodes_tbl %>%
  filter(`Node Type` == "Person", name == "Sailor Shift") %>% pull(id)
ss_song_ids <- edges_tbl %>%
  filter(`Edge Type` == "PerformerOf", source == sailor_shift_id) %>%
  pull(target)
sailor_shift_songs <- nodes_tbl %>%
  filter(id %in% ss_song_ids, `Node Type` == "Song")

# --- Influence Edge Types ---
influence_types <- c("InStyleOf", "CoverOf", "LyricalReferenceTo",
                     "InterpolatesFrom", "DirectlySamples")
influence_edges <- edges_tbl %>% filter(`Edge Type` %in% influence_types)

# --- Prepare Inward and Outward Influences ---
outward_influences <- influence_edges %>% filter(source %in% of_song_ids)
outward_nodes <- nodes_tbl %>% filter(id %in% outward_influences$target)

inward_influences <- influence_edges %>% filter(target %in% of_song_ids)
inward_nodes <- nodes_tbl %>% filter(id %in% inward_influences$source)

# --- Influence Over Time Summary ---
influence_over_time <- influence_edges %>%
  filter(source %in% of_song_ids) %>%
  left_join(nodes_tbl, by = c("target" = "id")) %>%
  filter(!is.na(release_date)) %>%
  mutate(release_year = as.integer(substr(release_date, 1, 4))) %>%
  count(release_year, name = "n") %>%
  filter(!is.na(release_year))

# UI
ui <- fluidPage(
  titlePanel("Oceanus Music Influence Dashboard"),
  tabsetPanel(id = "main_tab",
              
              tabPanel("Exploratory Data Analysis", 
                       fluidRow(
                         column(width = 3,
                                h4("5. Exploratory Data Analysis"),
                                navlistPanel(id = "eda_subtab", widths = c(12, 1),
                                             tabPanel("5.1 Edge Type Count", ""),
                                             tabPanel("5.2 Node Type Count", ""),
                                             tabPanel("5.3 Genre Distribution of Songs", ""),
                                             tabPanel("5.4 Oceanus Folk Song Release Trend", ""),
                                             tabPanel("5.5 Genres Influenced by Oceanus Folk", "")
                                ),
                                br(),
                                sliderInput("eda_year_range", "Select Year Range:",
                                            min = min(influence_over_time$release_year, na.rm = TRUE),
                                            max = max(influence_over_time$release_year, na.rm = TRUE),
                                            value = range(influence_over_time$release_year),
                                            sep = "", step = 1),
                                numericInput("top_n_genres", "Select Top N Genres:", value = 10, min = 5, max = 50)
                         ),
                         column(width = 9,
                                uiOutput("eda_plot_output")  # Render selected sub-tab’s plot here
                         )
                       )
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
                                             plotlyOutput("of_influence_plot"),
                                             br(),
                                             h4("Influenced Songs and Performing Artists"),
                                             DT::dataTableOutput("of_influenced_table")
                                             ),
               tabPanel("Others Influence on Oceanus Folk",
                        h4("Genres That Influenced Oceanus Folk"),
                        visNetworkOutput("of_inward_genre_plot", height = "300px"),
                        br(),
                        h4("Influence Network Diagram"),
                        visNetworkOutput("of_network_plot", height = "500px")))),
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
  output$of_network_plot <- renderVisNetwork({
    # Step 1: Filter influential works (songs & albums only)
    influential_works <- inward_nodes %>%
      filter(`Node Type` %in% c("Song", "Album")) %>%
      mutate(work_label = name,
             work_id = paste0("work_", row_number()),
             group = ifelse(`Node Type` == "Song", "song", "album"))
    
    # Step 2: Get performing artists for those works
    influencing_artists <- edges_tbl %>%
      filter(`Edge Type` == "PerformerOf", target %in% influential_works$id) %>%
      rename(work_real_id = target, artist_real_id = source) %>%
      left_join(nodes_tbl, by = c("artist_real_id" = "id")) %>%
      filter(`Node Type` == "Person") %>%
      distinct(artist_real_id, name) %>%
      mutate(artist_label = name,
             artist_id = paste0("artist_", row_number()),
             group = "person")
    
    # Step 3: Build nodes
    center_node <- tibble(id = "Oceanus Folk", label = "Oceanus Folk", group = "center")
    work_nodes <- influential_works %>%
      select(id = work_id, label = work_label, group)
    artist_nodes <- influencing_artists %>%
      select(id = artist_id, label = artist_label, group)
    nodes_viz <- bind_rows(center_node, work_nodes, artist_nodes)
    
    # Step 4: Build edges
    edges_to_of <- influential_works %>%
      transmute(from = work_id, to = "Oceanus Folk")
    
    edges_artist_to_work <- edges_tbl %>%
      filter(`Edge Type` == "PerformerOf", target %in% influential_works$id) %>%
      rename(work_real_id = target, artist_real_id = source) %>%
      left_join(influential_works, by = c("work_real_id" = "id")) %>%
      left_join(influencing_artists, by = c("artist_real_id")) %>%
      transmute(from = artist_id, to = work_id) %>%
      filter(!is.na(from), !is.na(to))
    
    edges_viz <- bind_rows(edges_artist_to_work, edges_to_of)
    
    # Step 5: Visualize
    visNetwork(nodes_viz, edges_viz, height = "700px", width = "100%") %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visLayout(randomSeed = 999) %>%
      visNodes(shape = "dot", size = 15) %>%
      visGroups(groupname = "center", color = "tomato") %>%
      visGroups(groupname = "song", color = "lightblue") %>%
      visGroups(groupname = "album", color = "khaki") %>%
      visGroups(groupname = "person", color = "orchid") %>%
      visLegend()
  })
  
  output$eda_plot_output <- renderUI({
    switch(input$eda_subtab,
           "5.1 Edge Type Count" = plotOutput("edge_type_plot"),
           "5.2 Node Type Count" = plotOutput("node_type_plot"),
           "5.3 Genre Distribution of Songs" = plotOutput("genre_distribution_plot"),
           "5.4 Oceanus Folk Song Release Trend" = plotOutput("of_release_trend_plot"),
           "5.5 Genres Influenced by Oceanus Folk" = plotOutput("of_influence_genre_plot")
    )
  })
  
  output$of_influenced_table <- DT::renderDataTable({
    # Step 1: Get songs influenced by Oceanus Folk
    of_influenced_songs <- influence_edges %>%
      filter(source %in% of_songs$id) %>%
      left_join(nodes_tbl, by = c("target" = "id")) %>%
      filter(`Node Type` == "Song") %>%
      rename(song_id = target, song_name = name)
    
    # Step 2: Get performers of those songs
    performers <- edges_tbl %>%
      filter(`Edge Type` == "PerformerOf", target %in% of_influenced_songs$song_id) %>%
      rename(song_id = target, artist_id = source) %>%
      left_join(nodes_tbl, by = c("artist_id" = "id")) %>%
      select(song_id, artist_id, artist_name = name)
    
    # Step 3: Combine with many-to-many relationship handling
    influenced_song_details <- of_influenced_songs %>%
      left_join(performers, by = "song_id", relationship = "many-to-many") %>%
      select(song_id, song_name, artist_id, artist_name, genre, release_date, notoriety_date, notable, single) %>%
      filter(!is.na(release_date)) %>%
      mutate(release_year = as.integer(release_date)) %>%
      filter(release_year >= input$year_range[1],
             release_year <= input$year_range[2]) %>%
      select(-release_year)  # Remove helper column
    
    # Step 4: Return filtered table
    DT::datatable(
      influenced_song_details,
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        scrollX = TRUE
      ),
      rownames = FALSE,
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: left;",
        "Table: Songs influenced by Oceanus Folk and their performing artists"
      )
    )
  })
  
  output$of_inward_genre_plot <- renderVisNetwork({
    # Step 1: Filter inward-influencing nodes to songs with known genre
    inward_genre_edges <- inward_influences %>%
      left_join(nodes_tbl, by = c("source" = "id")) %>%
      filter(`Node Type` == "Song", !is.na(genre)) %>%
      transmute(from = genre, to = "Oceanus Folk")  # genre → Oceanus Folk
    
    # Step 2: Count influence strength by genre
    inward_genre_strength <- inward_genre_edges %>%
      count(from, name = "influence_count")
    
    # Step 3: Identify top 5 genres and assign colors
    top5_genres <- inward_genre_strength %>%
      arrange(desc(influence_count)) %>%
      slice(1:5) %>%
      mutate(color = rev(brewer.pal(5, "Blues")))
    
    # Step 4: Build nodes
    inward_genre_nodes <- unique(c(inward_genre_edges$from, inward_genre_edges$to)) %>%
      tibble(id = .) %>%
      left_join(inward_genre_strength, by = c("id" = "from")) %>%
      left_join(top5_genres %>% select(id = from, top5_color = color), by = "id") %>%
      mutate(
        influence_count = replace_na(influence_count, 1),
        label = id,
        value = influence_count * 2,
        color = case_when(
          id == "Oceanus Folk" ~ "#FF6347",
          !is.na(top5_color) ~ top5_color,
          TRUE ~ "#9ECAE1"
        ),
        title = paste0("Genre: ", id, "<br>Influence Count: ", influence_count)
      )
    
    # Step 5: Visualize
    visNetwork(inward_genre_nodes, inward_genre_edges) %>%
      visEdges(arrows = "to") %>%
      visOptions(highlightNearest = TRUE) %>%
      visLayout(randomSeed = 321) %>%
      visPhysics(enabled = FALSE)
  })
  
  # 5.1 Edge Type Count
  output$edge_type_plot <- renderPlot({
    ggplot(data = edges_tbl, aes(y = `Edge Type`)) +
      geom_bar(fill = "steelblue") +
      labs(title = "Edge Type Distribution", y = "Edge Type", x = "Count") +
      theme_minimal()
  })
  
  # 5.2 Node Type Count
  output$node_type_plot <- renderPlot({
    ggplot(data = nodes_tbl, aes(y = `Node Type`)) +
      geom_bar(fill = "orchid") +
      labs(title = "Node Type Distribution", y = "Node Type", x = "Count") +
      theme_minimal()
  })
  
  # 5.3 Genre Distribution of Songs
  output$genre_distribution_plot <- renderPlot({
    nodes_tbl %>%
      filter(`Node Type` == "Song") %>%
      count(genre, sort = TRUE) %>%
      ggplot(aes(x = reorder(genre, n), y = n)) +
      geom_bar(stat = "identity", fill = "tomato") +
      coord_flip() +
      labs(title = "Genre Distribution of Songs", x = "Genre", y = "Count") +
      theme_minimal()
  })
  
  # 5.4 Temporal Distribution of Oceanus Folk Songs
  output$of_release_trend_plot <- renderPlot({
    of_songs %>%
      filter(!is.na(release_date)) %>%
      count(release_date) %>%
      ggplot(aes(x = as.integer(release_date), y = n)) +
      geom_line(color = "purple") +
      labs(title = "Oceanus Folk Songs Release Trend", x = "Year", y = "Number of Songs") +
      theme_minimal()
  })
  
  # 5.5 Influenced Genre Distribution
  output$of_influence_genre_plot <- renderPlot({
    outward_nodes %>%
      count(genre, sort = TRUE) %>%
      top_n(10, n) %>%
      ggplot(aes(x = reorder(genre, n), y = n)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      coord_flip() +
      labs(title = "Top 10 Genres Influenced by Oceanus Folk", x = "Genre", y = "Count") +
      theme_minimal()
  })
  
}

shinyApp(ui, server)
