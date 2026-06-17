#' Perform Sentiment Analysis on Quran Translation
#'
#' @description
#' Performs sentiment analysis on a Quran translation using specified lexicons.
#' Supports Bing, NRC, and AFINN lexicons. Ensures text is analyzed at the unigram level.
#' Returns word counts by sentiment and net sentiment scores across the specified index.
#'
#' @param tanzil_trans_object A \code{translationList} object created with \code{tanzil_translation()}.
#' @param config A \code{TransAnalyticConfig} object. If \code{ngram} is not "unigram",
#'   it will be overridden with a warning.
#' @param lexicon Character. Lexicon to use: "bing" (default), "nrc", or "afinn".
#' @param index Character. Grouping index for sentiment scores over the text: "ayah" (default) or "surah".
#'
#' @return An object of class \code{TransSentiment} containing word counts and sentiment scores.
#'
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @importFrom rlang .data
#'
#' @export
sentiment_trans <- function(
  tanzil_trans_object,
  config = trans_analytic_config(),
  lexicon = c("bing", "nrc", "afinn"),
  index = c("ayah", "surah")
) {
  if (!"translationList" %in% class(tanzil_trans_object)) {
    stop("Input must be a translationList object created with tanzil_translation().")
  }
  if (!"TransAnalyticConfig" %in% class(config)) {
    stop("config must be a TransAnalyticConfig object. Create one with trans_analytic_config().")
  }

  lexicon <- match.arg(lexicon)
  index <- match.arg(index)

  if (config$ngram != "unigram") {
    warning("[XploreQuran] Sentiment analysis only supports unigrams. Overriding config$ngram to 'unigram'.")
    config$ngram <- "unigram"
  }

  # Map index argument to data column
  idx_col <- if (index == "ayah") "ayah_id" else "surah_id"

  # Preprocess text to unigrams
  tokens <- preprocess_tokens(tanzil_trans_object, config)

  # Fetch lexicon safely
  lex_df <- tryCatch({
    suppressMessages(tidytext::get_sentiments(lexicon))
  }, error = function(e) {
    stop(sprintf("Failed to load '%s' lexicon. Please ensure you have downloaded it. You may need to run textdata::lexicon_%s() once interactively. Error: %s", lexicon, lexicon, e$message))
  })

  # Join tokens with lexicon
  sentiment_data <- tokens %>%
    inner_join(lex_df, by = "word", relationship = "many-to-many")

  # If empty, return gracefully
  if (nrow(sentiment_data) == 0) {
    warning("No words matched the lexicon. Returning NULL.")
    return(NULL)
  }

  # 1. Word counts based on sentiment
  if (lexicon == "afinn") {
    word_counts <- sentiment_data %>%
      count(word, value, sort = TRUE)
  } else {
    word_counts <- sentiment_data %>%
      count(word, sentiment, sort = TRUE)
  }

  # 2. Sentiment scores across the index
  if (lexicon == "afinn") {
    scores <- sentiment_data %>%
      group_by(.data[[idx_col]]) %>%
      summarise(sentiment_score = sum(value, na.rm = TRUE), .groups = "drop")
  } else if (lexicon == "bing") {
    scores <- sentiment_data %>%
      count(.data[[idx_col]], sentiment) %>%
      pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
      mutate(sentiment_score = positive - negative) %>%
      select(.data[[idx_col]], positive, negative, sentiment_score)
  } else if (lexicon == "nrc") {
    # NRC has many categories, but we calculate score based on positive - negative
    scores_wide <- sentiment_data %>%
      count(.data[[idx_col]], sentiment) %>%
      pivot_wider(names_from = sentiment, values_from = n, values_fill = 0)

    # Ensure positive and negative columns exist
    if (!"positive" %in% names(scores_wide)) scores_wide$positive <- 0
    if (!"negative" %in% names(scores_wide)) scores_wide$negative <- 0

    scores <- scores_wide %>%
      mutate(sentiment_score = positive - negative)
  }

  res <- list(
    word_counts = word_counts,
    sentiment_scores = scores,
    lexicon = lexicon,
    index = index
  )
  class(res) <- "TransSentiment"

  return(res)
}

#' Plot Sentiment Scores
#'
#' @param x A TransSentiment object
#' @param ... Additional arguments
#' @export
plot.TransSentiment <- function(x, ...) {
  df <- x$sentiment_scores
  idx_col <- if (x$index == "ayah") "ayah_id" else "surah_id"

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[idx_col]], y = sentiment_score, fill = sentiment_score > 0)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::scale_fill_manual(values = c("TRUE" = "#2ca25f", "FALSE" = "#de2d26")) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = sprintf("Sentiment Score across %s (%s lexicon)", tools::toTitleCase(x$index), toupper(x$lexicon)),
      x = tools::toTitleCase(x$index),
      y = "Net Sentiment Score"
    )
  return(p)
}

#' Summary of Sentiment Scores
#'
#' @param object A TransSentiment object
#' @param ... Additional arguments
#' @export
summary.TransSentiment <- function(object, ...) {
  scores <- object$sentiment_scores$sentiment_score
  idx <- object$index
  lex <- object$lexicon

  cat(sprintf("== XploreQuran Sentiment Analysis Summary ==\n"))
  cat(sprintf(" Lexicon : %s\n", toupper(lex)))
  cat(sprintf(" Index   : %s\n", tools::toTitleCase(idx)))
  cat(sprintf(" Total   : %d %ss analyzed\n", nrow(object$sentiment_scores), idx))
  cat("\n-- Score Distribution --\n")
  print(summary(scores))

  cat("\n-- Top 5 Positive Words --\n")
  if (lex == "afinn") {
    pos_w <- head(object$word_counts[object$word_counts$value > 0, ], 5)
  } else {
    pos_w <- head(object$word_counts[object$word_counts$sentiment == "positive", ], 5)
  }
  print(pos_w)

  cat("\n-- Top 5 Negative Words --\n")
  if (lex == "afinn") {
    neg_w <- head(object$word_counts[object$word_counts$value < 0, ], 5)
  } else {
    neg_w <- head(object$word_counts[object$word_counts$sentiment == "negative", ], 5)
  }
  print(neg_w)

  invisible(object)
}

#' Print TransSentiment Object
#'
#' @param x A TransSentiment object
#' @param ... Additional arguments
#' @export
print.TransSentiment <- function(x, ...) {
  cat(sprintf("TransSentiment Object (Lexicon: %s, Index: %s)\n", toupper(x$lexicon), tools::toTitleCase(x$index)))
  cat("\nAvailable components:\n")
  cat("  $word_counts      : data.frame of word frequencies by sentiment\n")
  cat("  $sentiment_scores : data.frame of net sentiment score per index\n")
  cat("\nUse summary() for detailed stats, or plot() to visualize.\n")
  invisible(x)
}
