# ==============================================================================
# modules/mod_sentiment.R
# XploreQuran - Sentiment Analysis Module
# ==============================================================================

# --- UI -----------------------------------------------------------------------
mod_sentiment_ui <- function(id) {
  ns <- NS(id)

  # Wrap everything in a scrollable div at page level
  tags$div(
    class = "sentiment-page-scroll",

    # Row 1: Configuration Card (Top Left) & Value Boxes (1st Section)
    layout_columns(
      col_widths = c(3, 9),
      fill = FALSE,
      style = "z-index: 10; position: relative;",
      card(
        card_header(
          div(
            class = "d-flex justify-content-between align-items-center w-100",
            span(tagList(bsicons::bs_icon("sliders"), " Configuration")),
            actionButton(
              ns("btn_lexicon_guide"), 
              label = NULL,
              icon = bsicons::bs_icon("book"),
              class = "btn btn-sm btn-link p-0 text-muted",
              title = "View Lexicon Guide",
              style = "text-decoration: none;"
            )
          )
        ),
        selectInput(
          ns("sel_lexicon"), 
          label = tags$span(
            "Sentiment Lexicon ",
            bslib::tooltip(
              bsicons::bs_icon("info-circle", class = "text-muted", style = "font-size: 0.8rem; cursor: pointer;"),
              "Default selection is bing. NRC and AFINN may prompt downloading if run for the first time.",
              placement = "right"
            )
          ),
          choices  = c("Bing" = "bing", "AFINN" = "afinn", "NRC" = "nrc"),
          selected = "bing"
        )
      ),
      # 1st section: Card with count of each Lexicon label categorization
      uiOutput(ns("vb_ui"))
    ),

    # Row 2: Words vs Count by Lexicon Category (2nd Section)
    card(
      card_header(tagList(bsicons::bs_icon("bar-chart-line"), " Words vs Count by Lexicon Category")),
      card_body(
        # Height is generous; page-level scroll handles overflow — no per-card scroll
        plotlyOutput(ns("plot_category_words"), height = "700px")
      )
    ),

    # Row 3: Sentiment Score (3rd Section)
    card(
      card_header(
        div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          span(tagList(bsicons::bs_icon("graph-up"), " Sentiment Score Analysis")),
          div(
            style = "width: 200px;",
            selectInput(
              ns("sel_index"), NULL,
              choices = c("Per Surah" = "surah", "Per Ayah" = "ayah"),
              selected = "surah",
              width = "100%"
            )
          )
        )
      ),
      card_body(
        plotlyOutput(ns("plot_sentiment_score"), height = "500px")
      )
    )
  )
}

# --- Server -------------------------------------------------------------------
mod_sentiment_server <- function(id, tokens_df, top_n, is_dark) {
  moduleServer(id, function(input, output, session) {

    # Lexicon Guide Modal
    observeEvent(input$btn_lexicon_guide, {
      showModal(modalDialog(
        title = tagList(bsicons::bs_icon("book"), " Sentiment Lexicon Guide"),
        size = "m",
        easyClose = TRUE,
        fade = TRUE,
        tags$div(
          tags$h6(bsicons::bs_icon("emoji-smile"), " Bing Lexicon", class = "text-primary mt-2"),
          tags$p("Categorizes words in a binary fashion into either positive or negative categories. Best for determining straightforward net sentiment (positive - negative) tone of a text.", style = "font-size: 0.9rem;"),
          
          tags$h6(bsicons::bs_icon("bar-chart-steps"), " AFINN Lexicon", class = "text-primary mt-4"),
          tags$p("Assigns words a numerical score ranging from -5 (very negative) to +5 (very positive). Best for analyzing granular sentiment intensity.", style = "font-size: 0.9rem;"),
          
          tags$h6(bsicons::bs_icon("emoji-heart-eyes"), " NRC Lexicon", class = "text-primary mt-4"),
          tags$p("Categorizes words into eight basic emotions (anger, fear, anticipation, trust, surprise, sadness, joy, and disgust) as well as two broad sentiments (negative and positive). Best for creating a rich emotional profile of the text.", style = "font-size: 0.9rem;")
        ),
        footer = modalButton("Close")
      ))
    })

    # Reactive for selected lexicon data frame
    lexicon_df <- reactive({
      req(input$sel_lexicon)
      lex <- input$sel_lexicon
      
      tryCatch({
        if (lex == "nrc") {
          readRDS("data/nrc.rds")
        } else if (lex == "afinn") {
          readRDS("data/afinn.rds")
        } else {
          suppressMessages(tidytext::get_sentiments(lex))
        }
      }, error = function(e) {
        showNotification(
          sprintf("Error loading '%s' lexicon: %s.", toupper(lex), e$message),
          type = "error"
        )
        NULL
      })
    })

    # Reactive for joined tokens and sentiment labels/scores
    joined_data <- reactive({
      req(tokens_df(), lexicon_df())
      
      tokens_df() |>
        inner_join(lexicon_df(), by = "word", relationship = "many-to-many")
    })

    # 1st Section: Dynamically render Value Boxes / Cards based on Lexicon
    output$vb_ui <- renderUI({
      req(input$sel_lexicon)
      lexicon <- input$sel_lexicon
      
      jd <- joined_data()
      if (is.null(jd) || nrow(jd) == 0) {
        return(
          card(
            card_body(
              class = "text-center py-4",
              p(class = "text-muted mb-0", "No words matched the selected lexicon in the current selection.")
            )
          )
        )
      }
      
      if (lexicon == "bing") {
        pos_cnt   <- sum(jd$sentiment == "positive", na.rm = TRUE)
        neg_cnt   <- sum(jd$sentiment == "negative", na.rm = TRUE)
        net_score <- pos_cnt - neg_cnt
        
        layout_columns(
          col_widths = c(4, 4, 4),
          fill = FALSE,
          value_box(
            title    = "Positive Words",
            value    = format(pos_cnt, big.mark = ","),
            showcase = bsicons::bs_icon("emoji-smile"),
            theme    = "success"
          ),
          value_box(
            title    = "Negative Words",
            value    = format(neg_cnt, big.mark = ","),
            showcase = bsicons::bs_icon("emoji-frown"),
            theme    = "danger"
          ),
          value_box(
            title    = "Net Sentiment",
            value    = paste0(if (net_score >= 0) "+" else "", format(net_score, big.mark = ",")),
            showcase = bsicons::bs_icon("graph-up-arrow"),
            theme    = "primary"
          )
        )
      } else if (lexicon == "afinn") {
        pos_cnt   <- sum(jd$value > 0, na.rm = TRUE)
        neg_cnt   <- sum(jd$value < 0, na.rm = TRUE)
        net_score <- sum(jd$value, na.rm = TRUE)
        
        layout_columns(
          col_widths = c(4, 4, 4),
          fill = FALSE,
          value_box(
            title    = "Positive Words (Score > 0)",
            value    = format(pos_cnt, big.mark = ","),
            showcase = bsicons::bs_icon("emoji-smile"),
            theme    = "success"
          ),
          value_box(
            title    = "Negative Words (Score < 0)",
            value    = format(neg_cnt, big.mark = ","),
            showcase = bsicons::bs_icon("emoji-frown"),
            theme    = "danger"
          ),
          value_box(
            title    = "Net Sentiment Score",
            value    = paste0(if (net_score >= 0) "+" else "", format(net_score, big.mark = ",")),
            showcase = bsicons::bs_icon("graph-up-arrow"),
            theme    = "primary"
          )
        )
      } else { # nrc
        categories <- c("positive", "negative", "trust", "joy", "anticipation", "fear", "sadness", "anger", "surprise", "disgust")
        
        counts <- sapply(categories, function(cat) {
          sum(jd$sentiment == cat, na.rm = TRUE)
        })
        
        themes <- c(
          "positive"     = "success",
          "negative"     = "danger",
          "trust"        = "primary",
          "joy"          = "info",
          "anticipation" = "warning",
          "fear"         = "secondary",
          "sadness"      = "dark",
          "anger"        = "danger",
          "surprise"     = "warning",
          "disgust"      = "secondary"
        )
        
        icons <- c(
          "positive"     = "emoji-smile",
          "negative"     = "emoji-frown",
          "trust"        = "shield-check",
          "joy"          = "brightness-high",
          "anticipation" = "hourglass-split",
          "fear"         = "eye-slash",
          "sadness"      = "cloud-rain",
          "anger"        = "fire",
          "surprise"     = "lightning",
          "disgust"      = "hand-thumbs-down"
        )
        
        boxes <- lapply(categories, function(cat) {
          value_box(
            title    = tools::toTitleCase(cat),
            value    = format(counts[[cat]], big.mark = ","),
            showcase = bsicons::bs_icon(icons[[cat]]),
            theme    = themes[[cat]]
          )
        })
        
        layout_columns(
          col_widths = c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3), # wrapped automatically by layout_columns
          fill = FALSE,
          !!!boxes
        )
      }
    })

    # 2nd Section: Words vs Count Bar Chart by Lexicon Category
    output$plot_category_words <- renderPlotly({
      req(joined_data(), input$sel_lexicon)
      jd <- joined_data()
      lexicon <- input$sel_lexicon
      
      validate(
        need(nrow(jd) > 0, "No sentiment words matched. Try adjusting your translation or selection.")
      )
      
      if (lexicon == "bing") {
        df_plot <- jd |>
          count(sentiment, word, sort = TRUE) |>
          group_by(sentiment) |>
          slice_max(n, n = top_n(), with_ties = FALSE) |>
          ungroup() |>
          mutate(category = tools::toTitleCase(sentiment))
      } else if (lexicon == "afinn") {
        df_plot <- jd |>
          mutate(sentiment = ifelse(value > 0, "Positive", "Negative")) |>
          count(sentiment, word, sort = TRUE) |>
          group_by(sentiment) |>
          slice_max(n, n = top_n(), with_ties = FALSE) |>
          ungroup() |>
          mutate(category = sentiment)
      } else { # nrc
        df_plot <- jd |>
          count(sentiment, word, sort = TRUE) |>
          group_by(sentiment) |>
          slice_max(n, n = top_n(), with_ties = FALSE) |>
          ungroup() |>
          mutate(category = tools::toTitleCase(sentiment))
      }
      
      validate(
        need(nrow(df_plot) > 0, "No word occurrences found for category plot.")
      )
      
      # Correctly order within categories
      df_plot <- df_plot |>
        mutate(word_ord = tidytext::reorder_within(word, n, category))
      
      # Styling colors matching category
      color_palette <- c(
        "Positive"     = "#2ca25f",
        "Negative"     = "#de2d26",
        "Anger"        = "#de2d26",
        "Anticipation" = "#fe9929",
        "Disgust"      = "#a1dab4",
        "Fear"         = "#636363",
        "Joy"          = "#ec7014",
        "Sadness"      = "#2b8cbe",
        "Surprise"     = "#fec44f",
        "Trust"        = "#3182bd"
      )
      
      p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = n, y = word_ord, fill = category)) +
        ggplot2::geom_col(show.legend = FALSE, alpha = 0.85) +
        tidytext::scale_y_reordered() +
        ggplot2::facet_wrap(~ category, scales = "free_y", ncol = if (lexicon == "nrc") 5 else 2) +
        ggplot2::labs(x = "Word Count", y = NULL)
      
      # Apply custom colors if matches
      unique_cats <- unique(df_plot$category)
      available_colors <- color_palette[names(color_palette) %in% unique_cats]
      if (length(available_colors) > 0) {
        p <- p + ggplot2::scale_fill_manual(values = color_palette)
      } else {
        p <- p + ggplot2::scale_fill_brewer(palette = "Set2")
      }
      
      # Apply Light/Dark styling theme
      is_dark_val <- if (is.function(is_dark)) is_dark() else is_dark
      if (isTRUE(is_dark_val)) {
        bg_color <- "#161b22"
        theme_mode <- ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.background  = ggplot2::element_rect(fill = bg_color, color = NA),
            panel.background = ggplot2::element_rect(fill = bg_color, color = NA),
            text             = ggplot2::element_text(color = "#e6edf3", family = "Inter"),
            axis.text        = ggplot2::element_text(color = "#8b949e"),
            panel.grid       = ggplot2::element_line(color = "#30363d"),
            strip.text       = ggplot2::element_text(color = "#e6edf3", face = "bold"),
            axis.title       = ggplot2::element_text(color = "#e6edf3")
          )
      } else {
        bg_color <- "#ffffff"
        theme_mode = ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.background  = ggplot2::element_rect(fill = bg_color, color = NA),
            panel.background = ggplot2::element_rect(fill = bg_color, color = NA),
            text             = ggplot2::element_text(color = "#212529", family = "Inter"),
            axis.text        = ggplot2::element_text(color = "#495057"),
            panel.grid       = ggplot2::element_line(color = "#dee2e6"),
            strip.text       = ggplot2::element_text(color = "#212529", face = "bold"),
            axis.title       = ggplot2::element_text(color = "#212529")
          )
      }
      p <- p + theme_mode
      
      plotly::ggplotly(p, tooltip = c("x", "y")) |>
        plotly::layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)"
        )
    })

    # Reactive for aggregating sentiment score based on selection index (Surah vs Ayah)
    score_data <- reactive({
      req(joined_data(), input$sel_lexicon, input$sel_index)
      jd <- joined_data()
      lexicon <- input$sel_lexicon
      index <- input$sel_index
      
      if (nrow(jd) == 0) return(NULL)
      
      # Grouping columns
      group_cols <- if (index == "surah") {
        c("surah_id", "surah_title_en")
      } else {
        c("ayah_id", "surah_id", "surah_ayah_id", "surah_title_en")
      }
      
      if (lexicon == "afinn") {
        jd |>
          group_by(across(all_of(group_cols))) |>
          summarise(sentiment_score = sum(value, na.rm = TRUE), .groups = "drop")
      } else {
        # Bing & NRC are calculated based on positive - negative count
        jd |>
          group_by(across(all_of(group_cols))) |>
          summarise(
            positive = sum(sentiment == "positive", na.rm = TRUE),
            negative = sum(sentiment == "negative", na.rm = TRUE),
            sentiment_score = positive - negative,
            .groups = "drop"
          )
      }
    })

    # 3rd Section: Plot Sentiment Score per Surah or per Ayah
    output$plot_sentiment_score <- renderPlotly({
      req(score_data())
      df <- score_data()
      
      validate(
        need(nrow(df) > 0, "No sentiment score data available.")
      )
      
      index <- input$sel_index
      colors <- ifelse(df$sentiment_score >= 0, "#3fb950", "#f85149")
      
      if (index == "surah") {
        df <- df |> arrange(surah_id)
        
        plotly::plot_ly(
          data   = df,
          x      = ~surah_id, y = ~sentiment_score,
          type   = "bar",
          marker = list(color = colors),
          hovertemplate = "<b>%{customdata}</b><br>Score: %{y}<extra></extra>",
          customdata = ~surah_title_en
        ) |>
          plotly::layout(
            paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
            font  = list(color = "var(--cq-fg)", family = "Inter"),
            xaxis = list(title = "Surah ID",          gridcolor = "var(--cq-card-border)"),
            yaxis = list(title = "Sentiment Score", gridcolor = "var(--cq-card-border)",
                         zerolinecolor = "#58a6ff"),
            hoverlabel = list(bgcolor = "var(--cq-card-bg)", font = list(color = "var(--cq-fg)"))
          )
      } else {
        df <- df |> arrange(ayah_id)
        df$ayah_label <- paste0(df$surah_id, ":", df$surah_ayah_id)
        df$ayah_label <- factor(df$ayah_label, levels = df$ayah_label)
        
        plotly::plot_ly(
          data   = df,
          x      = ~ayah_label, y = ~sentiment_score,
          type   = "bar",
          marker = list(color = colors),
          hovertemplate = "<b>Ayah %{x} (%{customdata})</b><br>Score: %{y}<extra></extra>",
          customdata = ~surah_title_en
        ) |>
          plotly::layout(
            paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
            font  = list(color = "var(--cq-fg)", family = "Inter"),
            xaxis = list(title = "Ayah (Surah:Ayah)", gridcolor = "var(--cq-card-border)", tickangle = -45),
            yaxis = list(title = "Sentiment Score", gridcolor = "var(--cq-card-border)",
                         zerolinecolor = "#58a6ff"),
            hoverlabel = list(bgcolor = "var(--cq-card-bg)", font = list(color = "var(--cq-fg)"))
          )
      }
    })

    # Download handler: Export aggregated sentiment score table
    output$dl_sentiment <- downloadHandler(
      filename = function() {
        paste0("sentiment_", input$sel_lexicon, "_", input$sel_index, "_", Sys.Date(), ".csv")
      },
      content  = function(file) {
        req(score_data())
        write.csv(score_data(), file, row.names = FALSE)
      }
    )

  })
}
