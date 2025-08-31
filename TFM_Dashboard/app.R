# Libraries
library(shiny)
library(tidyverse)
library(tidytext)
library(bslib)
library(DT)
library(ggiraph)


# Load data
load("parsed_df_clean.RData")
load("stm_model_6.RData")
load("parsed_df_topics.RData")
afinn_es <- read_csv("lexico_afinn.csv")
nrc_es <- read_csv("lexico_nrc_tran.csv")

# Define color palette
party_colors <- c(
  "PSOE" = "#E30613",
  "PNV" = "#2A8343",
  "PP" = "#0093D8",
  "Cs" = "#FA4F00",
  "CUP-EC-EM" = "#693065",
  "ER" = "#FFBF41",
  "DL" = "#2E307A",
  "MX" = "#848484"
)


## USER INTERFACE

ui <- navbarPage("TFM Dashboard",
                 
                 tags$style(HTML("
    #sa-scope .bslib-card { 
    border: 1px solid #d9d9d9 !important;
    box-shadow: 0 .125rem .25rem rgba(0,0,0,.075) !important;
    border-radius: .5rem !important;
  }
  #sa-scope .bslib-card .card-header {
    background-color: #f8f9fa !important;
    border-bottom: 1px solid #e9ecef !important;
    padding: 0.9rem 1rem !important;
    white-space: normal !important;
    line-height: 1.3;
    font-size: 1.3rem;     
    font-weight: bold;     
  }
  ")),
                 tags$div(
                   style = "position: absolute; top: 10px; right: 20px; z-index: 9999;",
                   actionButton("info_button", 
                                label = NULL, 
                                icon = icon("circle-info"), 
                                class = "btn btn-link", 
                                style = "font-size: 22px; color: #555;")
                 ),
                 
                 
                 
 tabPanel("TF-IDF",
  sidebarLayout(
    sidebarPanel(
      checkboxInput("global_by_year", "Compare all interventions by year", FALSE),
      helpText("Otherwise, select up to 3 combinations of year and party."),
      
      fluidRow(
        column(6, selectInput("year1", "Year 1", 
                              choices = c("", sort(unique(parsed_df_clean$year))), 
                              selected = "")),
        column(6, selectInput("party1", "Party 1", 
                              choices = c("", sort(unique(parsed_df_clean$party))), 
                              selected = ""))
      ),
      fluidRow(
        column(6, selectInput("year2", "Year 2", 
                              choices = c("", sort(unique(parsed_df_clean$year))), 
                              selected = "")),
        column(6, selectInput("party2", "Party 2", 
                              choices = c("", sort(unique(parsed_df_clean$party))), 
                              selected = ""))
      ),
      fluidRow(
        column(6, selectInput("year3", "Year 3", 
                              choices = c("", sort(unique(parsed_df_clean$year))), 
                              selected = "")),
        column(6, selectInput("party3", "Party 3", 
                              choices = c("", sort(unique(parsed_df_clean$party))), 
                              selected = ""))
      ),
      helpText("TF-IDF highlights words that are important within a specific document relative to a collection of documents. 
It helps to identify the most distinctive terms used by each party or year.")
    ),
    
    mainPanel(
      uiOutput("tfidf_plots_ui")
    )
  )
),

  tabPanel("Topic Modelling",
           navset_card_tab(
             nav_panel(
               "Top Terms Table",
               h3("Top Words per Topic (STM, K = 6)"),
               helpText("Structural Topic Modeling (STM) is an unsupervised machine learning technique used to discover topics in a collection of texts. 
It also allows for the inclusion of metadata (e.g. party) to analyze topic prevalence across different groups."),
               DTOutput("topic_table", 
                        width = "95%", height = "2000px")
             ),
             nav_panel(
               "Topic Distribution by Party",
               girafeOutput("topic_distribution_plot", 
                            height = "515px", width = "97%")
             )
           )
  ),

  tabPanel("Word Network Analysis",
           sidebarLayout(
             sidebarPanel(
               selectInput("wn_year", "Year", 
                           choices = sort(unique(parsed_df_clean$year))),
               selectInput("wn_party", "Party", 
                           choices = sort(unique(parsed_df_clean$party))),
               
               conditionalPanel(
                 condition = "input.wn_tab === 'Pairwise correlation analysis'",
                 sliderInput("cor_threshold",
                            "Minimum correlation:",
                            min = 0.5,
                            max = 1,
                            value = 0.7,
                            step = 0.05)
                 ),
                 width = 2
               ),
             mainPanel(
               navset_card_tab(
                 id = "wn_tab",
                 nav_panel(
                   "Bigram network analysis",
                   plotOutput("bigram_network_plot", height = "600px")
                 ),
                 nav_panel(
                   "Pairwise correlation analysis",
                   plotOutput("pairwise_network_plot", height = "600px")
                 )
               ),
               width = 10
             )
           )
  ),
tabPanel("Sentiment Analysis",
         navset_card_tab(id = "sentiment_tab",
                         
                         nav_panel(
                           "AFINN",
                           sidebarLayout(
                             sidebarPanel(
                               selectInput("sa_party", "Party",
                                           choices = sort(unique(parsed_df_clean$party)),
                                           selected = "Cs"),
                               helpText("The AFINN lexicon assigns sentiment scores to words ranging from -5 (very negative) to +5 (very positive)."),
                               width = 2
                             ),
                             mainPanel(
                               div(class = "pt-5", id = "sa-scope",
                                   layout_column_wrap(
                                     width = 1,                 
                                     heights_equal = "all",
                                     gap = "1.25rem",
                                     card(
                                       class = "shadow-sm border rounded-3",
                                       card_header(uiOutput("header_sentiment_time")),
                                       card_body(
                                         girafeOutput("sentiment_party_time", 
                                                      height = "600px", width = "100%")
                                       )
                                     ),
                                     card(
                                       class = "shadow-sm border rounded-3 mb-4",
                                       card_header(uiOutput("header_top_words")),
                                       card_body(
                                         girafeOutput("sentiment_top_words", 
                                                      height = "600px")
                                       )
                                     )
                                   )
                               )
                             )
                           )
                         ),
                         
                         nav_panel(
                           "NRC",
                           sidebarLayout(
                             sidebarPanel(
                               selectizeInput("nrc_selected_parties", 
                                              "Select up to 3 parties:",
                                              choices = sort(unique(parsed_df_clean$party)),
                                              selected = c("PP", "PSOE", "ER"),
                                              multiple = TRUE,
                                              options = list(maxItems = 3)),
                               helpText("The NRC Emotion Lexicon associates words with eight basic emotions. 
                It helps to understand the emotional tone of a text by identifying the prevalence of these emotions."),
                               width = 2
                             ),
                               
                             mainPanel(
                               div(class = "pt-5", id = "sa-scope",
                                   card(
                                     class = "shadow-sm border rounded-3",
                                     card_header(strong("Emotion prevalence across parties")),
                                     card_body(
                                       girafeOutput("nrc_topics_by_party", height = "450px", width = "100%")
                                     )
                                   )
                               )
                             )
                           )
                         )
         )
)

)


server <- function(input, output, session) {
  
  tfidf_data_all <- reactive({
    
    # Global: all parties grouped by year
    if (input$global_by_year) {
      return(
        parsed_df_clean %>%
          count(year, word, sort = TRUE) %>%
          bind_tf_idf(word, year, n) %>%
          group_by(year) %>%
          slice_max(tf_idf, n = 10) %>%
          ungroup() %>%
          mutate(document = year)
      )
    }
    
    # Combinations selected
    selected <- tibble(
      year = c(input$year1, input$year2, input$year3),
      party = c(input$party1, input$party2, input$party3)
    ) %>%
      filter(year != "" & party != "") %>%
      mutate(document = paste(party, year, sep = " - "))
    
    if (nrow(selected) == 0) return(NULL)
    if (nrow(selected) > 3) return("too_many")
    
    # One combination → compare against same year or same party
    if (nrow(selected) == 1) {
      y <- selected$year[1]
      p <- selected$party[1]
      df <- parsed_df_clean %>%
        filter(year == y | party == p) %>%
        mutate(document = paste(party, year, sep = " - ")) %>%
        select(document, word, party)
      
      tfidf <- df %>%
        count(document, word, party, sort = TRUE) %>%
        bind_tf_idf(word, document, n) %>%
        filter(document == paste(p, y, sep = " - "))
      return(tfidf)
    }
    
    # Multiple combinations
    df <- parsed_df_clean %>%
      filter(paste(party, year, sep = " - ") %in% selected$document) %>%
      mutate(document = paste(party, year, sep = " - ")) %>%
      select(document, word, party)
    
    df %>%
      count(document, word, party, sort = TRUE) %>%
      bind_tf_idf(word, document, n)
  })
  
  output$tfidf_plots_ui <- renderUI({
    tfidf_df <- tfidf_data_all()
    
    if (is.character(tfidf_df) && tfidf_df == "too_many") {
      return(h4("Please select a maximum of 3 combinations."))
    }
    
    if (is.null(tfidf_df) || nrow(tfidf_df) == 0) {
      return(h4("No data available for selected filters."))
    }
    
    # Global by year: facet view
    if (input$global_by_year) {
      output$globalPlot <- renderPlot({
        ggplot(tfidf_df, aes(x = reorder(word, tf_idf), y = tf_idf)) +
          geom_col(fill = "#4B4B4B") +
          coord_flip() +
          facet_wrap(~ year, scales = "free_y") +
          labs(
            title = "Top 10 TF-IDF words per year (all interventions)",
            x = "Word",
            y = "TF-IDF"
          ) +
          theme_minimal(base_size = 13) +
          theme(axis.text.x = element_text(angle = 35, hjust = 1))
      })
      return(plotOutput("globalPlot"))
    }
    
    # Individual combinations
    docs <- unique(tfidf_df$document)
    col_width <- floor(12 / length(docs))
    
    plot_output_list <- lapply(docs, function(doc) {
      tfidf_top <- tfidf_df %>%
        filter(document == doc) %>%
        slice_max(tf_idf, n = 10) %>%
        arrange(tf_idf)
      
      plotname <- paste0("plot_", gsub(" ", "_", doc))
      
      output[[plotname]] <- renderPlot({
        ggplot(tfidf_top, aes(x = reorder(word, tf_idf), 
                              y = tf_idf, 
                              fill = party)) +
          geom_col() +
          scale_fill_manual(values = party_colors) +
          coord_flip() +
          labs(
            title = doc,
            x = "Word",
            y = "TF-IDF"
          ) +
          theme_minimal(base_size = 13) +
          theme(axis.text.x = element_text(angle = 35, hjust = 1),
                legend.position = "none")
      })
      
      column(col_width, plotOutput(plotname))
    })
    
    do.call(fluidRow, plot_output_list)
  })
  
  # TOPIC MODELLING: total topics
  output$topic_table <- DT::renderDataTable({
    DT::datatable(
      frex_df_6,
      options = list(
        pageLength = 10,
        dom = 'tp',
        scrollY = FALSE,
        columnDefs = list(
          list(className = 'dt-center', targets = "_all")
        )
      ),
      rownames = FALSE
    )
  })
  
  # TOPIC MODELLING: topics per party
  theta_df <- as.data.frame(stm_model_6$theta)
  theta_df$party <- out$meta$party
  
  theta_avg <- theta_df %>%
    group_by(party) %>%
    summarise(across(everything(), mean), .groups = "drop")
  
  theta_long <- theta_avg %>%
    pivot_longer(
      cols = -party,
      names_to = "topic",
      values_to = "proportion"
    ) %>%
    mutate(topic = as.integer(gsub("V", "", topic))) %>%
    left_join(
      frex_df_6 %>%
        mutate(topic = as.integer(topic)) %>%
        select(topic, label) %>%
        distinct(),
      by = "topic"
    )
  
  output$topic_distribution_plot <- renderGirafe({
    gg0 <- ggplot(theta_long, aes(x = party, y = proportion, 
                             fill = party,
                             tooltip = paste0(
                               "Party: ", party, "\n",
                               "Proportion: ", round(proportion, 3))
                                 )
                  ) +
      geom_col_interactive(
        aes(data_id = party),
        show.legend = FALSE) +
      facet_wrap(~ label, scales = "free_y",
                 labeller = label_wrap_gen(width = 28)) +
      labs(y = "avg proportion") +
      scale_fill_manual(values = party_colors) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold", lineheight = 1.1),
        strip.text.x = element_text(margin = margin(t = 0.3, b = 0.5,
                                                    l = 0.01, r = 0.01, 
                                                    unit = "cm")),
        panel.spacing = unit(1.5, "cm"),   
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
      )
    
    girafe(
      ggobj = gg0,
      width_svg  = 12,   
      height_svg = 6,
      options = list(
        opts_hover(css = "fill-opacity:0.85;"),
        opts_tooltip(css = "
        background:#f0f0f0; color:#000;
        padding:6px; border:1px solid #ddd; border-radius:4px;
        font-size:12px;"
        )
      )
    )
    
  })
  
  # WORD NETWORK ANALYSIS
  wn_data <- reactive({
    req(input$wn_year, input$wn_party)
    parsed_df_clean %>%
      filter(year == input$wn_year, party == input$wn_party)
  })
  
  # Bigram network
  output$bigram_network_plot <- renderPlot({
    library(igraph)
    library(ggraph)
    
        data_filtered <- wn_data()
    
    # 1: Create bigrams
    bigrams <- data_filtered %>%
      group_by(doc_id) %>%
      summarise(text = paste(word, collapse = " "), .groups = "drop") %>%
      unnest_tokens(bigram, text, token = "ngrams", n = 2)
    
    # 2: Separate bigrams
    bigrams_separated <- bigrams %>%
      separate(bigram, into = c("word1", "word2"), sep = " ") %>%
      filter(!is.na(word1), !is.na(word2), word1 != "", word2 != "")
    
    # 3: Count bigrams
    bigram_counts <- bigrams_separated %>%
      count(word1, word2, sort = TRUE)
    
    top_bigrams <- bigram_counts %>%
      slice_max(n, n = 30)
    
    if (nrow(top_bigrams) == 0) return(NULL)
    
    # 4: Create  and visualize graph
    set.seed(1234) 
    
    graph <- top_bigrams %>%
      graph_from_data_frame(directed = TRUE)
    
    ggraph(graph, layout = "fr") +
      geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, edge_colour = "gray50") +
      geom_node_point(color = party_colors[input$wn_party], size = 6) +
      geom_node_text(aes(label = name), repel = TRUE, size = 5) +
      theme_void() +
      ggtitle(paste("Bigram Network -", input$wn_party, input$wn_year)) +
      theme(plot.title = element_text(size = 16, face = "bold"))
  })
  
  
  # Pairwise correlation network
  output$pairwise_network_plot <- renderPlot({
    library(widyr)
    library(igraph)
    library(ggraph)
    
    data_filtered <- wn_data()
    
    if (nrow(data_filtered) < 100) return(NULL)
    
    # 1: Compute correlation by doc_id
    correlation <- data_filtered %>%
      select(doc_id, word) %>%
      group_by(word) %>%
      filter(n() >= 10) %>%
      ungroup() %>%
      pairwise_cor(item = word, feature = doc_id, sort = TRUE)
    
    cor_table <- correlation %>%
      filter(correlation >= input$cor_threshold) %>%
      mutate(correlation = round(correlation, 4)) %>%
      slice_max(correlation, n = 50)
    
    if (nrow(cor_table) == 0) return(NULL)
    
    # 2: Create and visualize graph
    set.seed(1234)
    
    graph_cor <- graph_from_data_frame(cor_table, directed = FALSE)
    
    ggraph(graph_cor, layout = "fr") +
      geom_edge_link(aes(edge_alpha = correlation), edge_colour = "gray50") +
      scale_edge_alpha_continuous(range = c(0.2, 1),
                                  guide = guide_legend(title = "Correlation")) +
      geom_node_point(color = party_colors[input$wn_party], size = 6) +
      geom_node_text(aes(label = name), repel = TRUE, size = 5) +
      theme_void() +
      ggtitle(sprintf("Word Correlation Network (≥%.2f): %s %s",
                      input$cor_threshold, input$wn_party, input$wn_year)) +
      theme(plot.title = element_text(size = 16, face = "bold"))
  })
  
  # SENTIMENT ANALYSIS (AFINN)
  
  ## Sentiment over time
  output$sentiment_party_time <- renderGirafe({
    req(input$sa_party)
    
    sentiment_month <- parsed_df_clean %>%
      filter(party == input$sa_party) %>%
      mutate(month = floor_date(date, "month")) %>%  
      inner_join(afinn_es, by = c("word" = "palabra")) %>%
      group_by(month, party) %>%
      summarise(sentiment_score = sum(puntuacion), .groups = "drop") |> 
      mutate(sentiment = factor(
        ifelse(sentiment_score > 0, "Positive", "Negative"),
        levels = c("Negative", "Positive")
      ))
    
    first_month <- min(sentiment_month$month, na.rm = TRUE)
    last_month  <- max(sentiment_month$month, na.rm = TRUE)
    
    gg1 <- ggplot(sentiment_month, aes(x = month, y = sentiment_score, 
                                       fill = sentiment)) +
      geom_col_interactive(
        aes(tooltip = paste0("Month: ", format(month, "%b-%Y"), "\n",
                             "Sentiment score: ", sentiment_score),
            data_id = month)) +
      scale_fill_manual(name = "Sentiment",                      
                        values = c("Negative" = "#F8766D", "Positive" = "#00BCC1")) +
      scale_x_date(limits = c(first_month - days(15),   
                              last_month  + days(15)),
                   expand = c(0.05, 0.05),
                   date_labels = "%b-%Y", 
                   date_breaks = "1 month") +
      labs(x = "Month", y = "Total sentiment score") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    girafe(
      ggobj = gg1,
      width_svg  = 10,   
      height_svg = 6,
      options = list(
        opts_hover(css = "fill-opacity:0.9;"),
        opts_tooltip(css = "background:#f0f0f0;color:#000;
                           padding:6px;border:1px solid #ddd;border-radius:4px;
                           font-size:12px;")
      )
    )
      
    
  })
  
  output$header_sentiment_time <- renderUI({
    req(input$sa_party)
    strong(
      paste("Sentiment over time -", input$sa_party)
    )
  })
  
  ## Top contributing words
  output$sentiment_top_words <- renderGirafe({
    req(input$sa_party)
    
    df_party <- parsed_df_clean %>%
      filter(party == input$sa_party) %>%
      inner_join(
        afinn_es %>% distinct(palabra, puntuacion),
        by = c("word" = "palabra")
      )
    
    if (nrow(df_party) == 0) return(NULL)
    
    contrib <- df_party %>%
      count(word, puntuacion, name = "n") %>%
      mutate(
        total_contribution = n * puntuacion,
        sentiment = ifelse(puntuacion > 0, "Positive", "Negative"),
        abs_contrib = abs(total_contribution)
      )
    
    top_n <- 10
    top_words <- contrib %>%
      group_by(sentiment) %>%
      slice_max(abs_contrib, n = top_n, with_ties = FALSE) %>%
      ungroup() %>%
      arrange(total_contribution)
    
    if (nrow(top_words) == 0) return(NULL)
    
    top_words <- top_words %>%
      mutate(hjust_lab = ifelse(total_contribution > 0, -0.1, 1.1))
    
    gg2 <- ggplot(top_words,
           aes(x = reorder(word, total_contribution),
               y = total_contribution,
               fill = sentiment)) +
      geom_col_interactive(aes(tooltip = paste0("Word: ", word, "\n",
                                                "Total contribution: ", total_contribution, "\n",
                                                "AFINN value: ", puntuacion),
                               data_id = word)) +
      coord_flip() +
      scale_fill_manual(values = c("Positive" = "#00BCC1", "Negative" = "#F8766D")) +
      geom_text(aes(label = puntuacion, hjust = hjust_lab),
                color = "black", size = 3) +
      labs(x = NULL,
           y = "Total contribution sentiment score",
           fill = "Sentiment") +
      theme_minimal(base_size = 14) 
    
    girafe(
      ggobj = gg2,
      options = list(
        opts_hover(css = "fill-opacity:0.9;"),
        opts_tooltip(css = "background:#f0f0f0;color:#000;
                           padding:6px;border:1px solid #ddd;border-radius:4px;
                           font-size:12px;")
      )
    )
  })
  
  output$header_top_words <- renderUI({
    req(input$sa_party)
    strong(
      paste("Top contributing words -", input$sa_party, "(2016-2018)")
    )
  })
  
  # SENTIMENT ANALYSIS (NRC)
  
  emotion_colors <- c(
    "anger"        = "#d62728",
    "anticipation" = "#bcbd22",
    "disgust"      = "#8c564b",
    "fear"         = "#9467bd",
    "joy"          = "#2ca02c",
    "sadness"      = "#1f77b4",
    "surprise"     = "#17becf",
    "trust"        = "#ff7f0e"
  )
  
  output$nrc_topics_by_party <- renderGirafe({
    req(input$nrc_selected_parties)
    selected_parties <- input$nrc_selected_parties
    
    nrc_topic_df <- parsed_df_topics %>%
      filter(party %in% selected_parties) %>%
      inner_join(nrc_es, by = "word") %>%
      filter(!sentiment_eng %in% c("positive", "negative")) %>%
      count(party, topic, sentiment_eng, name = "n") %>%
      rename(emotion = sentiment_eng)
    
    party_topic_total <- nrc_topic_df %>%
      group_by(party, topic) %>%
      summarise(total = sum(n), .groups = "drop")
    
    nrc_prop <- nrc_topic_df %>%
      left_join(party_topic_total, by = c("party", "topic")) %>%
      mutate(percent = n / total)
    
    nrc_prop <- nrc_prop %>%
      left_join(
        frex_df_6 %>%
          distinct(topic, label) %>%
          mutate(topic = as.integer(topic)),
        by = "topic"
      )
    
    gg <- ggplot(
      nrc_prop,
      aes(x = factor(topic), y = percent, fill = emotion,
          tooltip = paste0("Party: ", party,
                           "\nTopic: ", label,
                           "\nEmotion: ", emotion,
                           "\nPct: ", round(percent * 100, 1), "%"),
          data_id = paste(party, topic, emotion, sep = "_"))
    ) +
      geom_col_interactive(position = "stack", color = "white", size = 0.2) +
      scale_fill_manual(values = emotion_colors, name = "Emotion") +
      facet_wrap(~ party) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(x = "Topic", y = "% of emotion words") +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        strip.text = element_text(face = "bold")
      )
    
    girafe(
      ggobj = gg,
      width_svg = 11,
      height_svg = 6,
      options = list(
        opts_hover(css = "filter:brightness(1.05);"),
        opts_tooltip(css = "background:#f0f0f0;color:#000;
                     padding:6px;border:1px solid #ddd;
                     border-radius:4px;font-size:12px;")
      )
    )
  })
  
}

shinyApp(ui = ui, server = server)