#' Jeopardy EDA Utilities
#'
#' Functions for performing exploratory data analysis on the Jeopardy dataset.
#' Includes functions for plotting distributions and generating word clouds.
#'
#' @name jeopardyNLP-eda-internal
#' @keywords internal
#' @import dplyr
#' @import ggplot2
#' @importFrom grDevices dev.off png
#' @importFrom RColorBrewer brewer.pal brewer.pal.info
#' @importFrom wordcloud wordcloud
#' @importFrom stringr str_count str_replace_all
#' @importFrom tm VCorpus VectorSource TermDocumentMatrix content_transformer removePunctuation removeNumbers removeWords stopwords stripWhitespace

library(readr)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(stringr)
library(tm) # Needed for basic word cloud cleaning
library(RColorBrewer) # For word cloud palettes

#' Plot Word Cloud from Text Column
#'
#' Generates and displays/saves a word cloud from a specified text column
#' in a data frame.
#' Note: Performs basic internal text cleaning (lowercase, punctuation, numbers,
#' standard English stopwords) unless `perform_cleaning` is FALSE.
#' For more control or advanced cleaning, pre-process the text using functions
#' from `text_cleaner.R` before calling this function.
#'
#' @param df Input data frame (tibble). Defaults to the package's internal
#'   `jeopardy_data`.
#' @param col Name of the column containing the text data.
#' @param perform_cleaning Logical. If TRUE (default), performs basic `tm` cleaning.
#' @param color_palette Name of the RColorBrewer palette to use.
#' @param max_words Maximum number of words to include in the cloud.
#' @param save Logical. If TRUE, saves the plot to a file instead of displaying.
#' @param output_dir Directory path to save the plot if `save = TRUE`.
#' @param filename Name for the saved file (defaults based on column name).
#'
#' @return Invisibly returns the word frequency data frame used for the plot.
#'   Displays or saves the word cloud plot.
#' @export
plot_word_cloud <- function(df = jeopardyNLP::jeopardy_data,
                            col,
                            perform_cleaning = TRUE,
                            color_palette = "Dark2",
                            max_words = 150,
                            save = FALSE,
                            output_dir = NULL,
                            filename = NULL) {
    # --- Input Checks ---
    if (!col %in% names(df)) {
        stop(paste("Column", col, "not found in the dataframe."))
    }
    # Check if palette exists
    available_palettes <- rownames(RColorBrewer::brewer.pal.info)
    if (!color_palette %in% available_palettes) {
        warning(paste("Palette", color_palette, "not found, using 'Dark2' instead."))
        color_palette <- "Dark2"
    }

    # --- Data Preparation ---
    text_data <- df[[col]]

    # Basic internal cleaning if requested
    if (perform_cleaning) {
        print("Performing basic internal text cleaning for word cloud...")
        corpus <- tm::VCorpus(tm::VectorSource(text_data))
        corpus_clean <- tm::tm_map(corpus, tm::content_transformer(tolower))
        corpus_clean <- tm::tm_map(corpus_clean, tm::removePunctuation)
        corpus_clean <- tm::tm_map(corpus_clean, tm::removeNumbers)
        corpus_clean <- tm::tm_map(corpus_clean, tm::removeWords, tm::stopwords("english"))
        corpus_clean <- tm::tm_map(corpus_clean, tm::stripWhitespace)
        text_data <- sapply(corpus_clean, as.character)
        text_data <- text_data[text_data != ""] # Remove empty strings
    }

    # Create Term Frequencies
    corpus_final <- tm::VCorpus(tm::VectorSource(text_data))
    tdm <- tm::TermDocumentMatrix(corpus_final)
    m <- as.matrix(tdm)
    word_freqs <- sort(rowSums(m), decreasing = TRUE)
    word_freqs_df <- data.frame(word = names(word_freqs), freq = word_freqs)

    if (nrow(word_freqs_df) == 0) {
        warning(paste("No words remaining for column:", col, "after processing. Skipping word cloud."))
        return(invisible(NULL))
    }

    # --- Plotting ---
    # Determine filename if saving
    filepath <- NULL
    if (save) {
        if (is.null(output_dir) || is.null(filename)) {
            stop("If save=TRUE, output_dir and filename must be provided.")
        }
        # Check if the custom filename was provided, otherwise generate default
        if (is.null(filename)) {
            filename <- paste0(stringr::str_replace_all(col, "[^a-zA-Z0-9]", "_"), "_wordcloud.png")
        }
        filepath <- file.path(output_dir, filename)
        # Ensure directory exists if saving
        if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
        message(paste("Attempting to save word cloud to:", filepath))
    }

    # Generate word cloud plot
    message(paste("Generating word cloud for column:", col))
    set.seed(1234) # for reproducibility

    # Handle color palette length
    num_colors <- max(3, min(nrow(word_freqs_df), 8))
    colors_to_use <- RColorBrewer::brewer.pal(num_colors, color_palette)

    if (save) {
        if (is.null(filepath)) stop("Filepath not set for saving.") # Safety check
        png(filepath, width = 800, height = 800)
    }

    wordcloud::wordcloud(
        words = word_freqs_df$word,
        freq = word_freqs_df$freq,
        min.freq = 1,
        max.words = max_words,
        random.order = FALSE,
        rot.per = 0.35,
        colors = colors_to_use
    )

    if (save) {
        dev.off()
        message(paste("Word cloud saved to:", filepath))
    }

    return(invisible(word_freqs_df))
}

#' Calculate Word Counts
#'
#' Calculates the number of words in the 'Answer' and 'Question' columns.
#'
#' @param df Input data frame (tibble) containing 'Clue Difficulty', 'Answer',
#'   and 'Question' columns. This data should typically be the result of
#'   processing the raw data using `create_regular_episodes_df` with
#'   `add_difficulty = TRUE`.
#'
#' @return A tibble with 'Clue Difficulty', 'Answer Word Count', and
#'   'Question Word Count' columns.
#' @export
calculate_word_counts <- function(df) {
    # Requires columns: "clue_difficulty", "answer", "question" (lowercase internal names)
    required_cols <- c("clue_difficulty", "answer", "question")
    if (!all(required_cols %in% names(df))) {
        stop(paste("Input dataframe must contain columns:", paste(required_cols, collapse = ", ")))
    }

    # Calculate counts
    df_with_counts <- df %>%
        dplyr::mutate(
            `Answer Word Count` = stringr::str_count(.data$answer, "\\S+"), # Count non-space sequences
            `Question Word Count` = stringr::str_count(.data$question, "\\S+")
        )

    # Select and rename for output
    df_final <- df_with_counts %>%
        dplyr::select(
            `Clue Difficulty` = clue_difficulty,
            `Answer Word Count` = `Answer Word Count`,
            `Question Word Count` = `Question Word Count`
        )

    return(df_final)
}

#' Plot Word Counts vs. Clue Difficulty
#'
#' Creates a bar chart showing the average word count (for either Answer or Question)
#' for each clue difficulty level.
#'
#' @param df_word_counts Data frame output from `calculate_word_counts`.
#' @param count_col Name of the word count column to plot (e.g., "Answer Word Count").
#' @param bar_color Color for the bars.
#' @param save Logical. If TRUE, saves the plot to a file.
#' @param output_dir Directory path to save the plot if `save = TRUE`.
#' @param filename Name for the saved file (defaults based on count column name).
#'
#' @return Invisibly returns the ggplot object. Displays or saves the plot.
#' @export
plot_word_counts_difficulty <- function(df_word_counts,
                                        count_col,
                                        bar_color = "steelblue",
                                        save = FALSE,
                                        output_dir = NULL,
                                        filename = NULL) {
    # --- Input Checks ---
    if (!count_col %in% names(df_word_counts)) {
        stop(paste("Column", count_col, "not found in the input dataframe."))
    }
    if (!"Clue Difficulty" %in% names(df_word_counts)) {
        stop("'Clue Difficulty' column not found in the input dataframe.")
    }

    # --- Data Preparation ---
    # Calculate average counts per difficulty
    avg_counts <- df_word_counts %>%
        dplyr::group_by(.data$`Clue Difficulty`) %>%
        dplyr::summarise(AvgCount = mean(.data[[count_col]], na.rm = TRUE), .groups = "drop") %>% # Ensure ungroup
        # Order factor levels for plot
        dplyr::mutate(`Clue Difficulty` = factor(.data$`Clue Difficulty`, levels = c("easy", "average", "hard")))

    # --- Plotting ---
    plot_title <- paste("Average", count_col, "vs. Clue Difficulty")
    y_label <- paste("Average", count_col)

    p <- ggplot2::ggplot(avg_counts, ggplot2::aes(x = .data$`Clue Difficulty`, y = .data$AvgCount)) +
        ggplot2::geom_col(fill = bar_color, na.rm = TRUE) +
        ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", .data$AvgCount)), vjust = -0.5, size = 3.5, na.rm = TRUE) +
        ggplot2::labs(
            title = plot_title,
            x = "Clue Difficulty",
            y = y_label
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    # Optional: Set y-limit like in Python example
    # ggplot2::ylim(0, 16)

    # Determine filename if saving
    filepath <- NULL
    if (save) {
        if (is.null(output_dir) || is.null(filename)) {
            stop("If save=TRUE, output_dir and filename must be provided.")
        }
        # Check if the custom filename was provided, otherwise generate default
        if (is.null(filename)) {
            filename <- paste0(stringr::str_replace_all(count_col, "[^a-zA-Z0-9]", "_"), "_counts_bar.png")
        }
        filepath <- file.path(output_dir, filename)
        message(paste("Attempting to save plot to:", filepath))
    }

    # Save or print plot
    if (save) {
        if (is.null(filepath)) stop("Filepath not set for saving.") # Safety check
        if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
        ggplot2::ggsave(filepath, plot = p, width = 6, height = 4, dpi = 150)
        message(paste("Plot saved to:", filepath))
    } else {
        print(p)
    }
    return(invisible(p))
}

#' Calculate Top N Categories
#'
#' Determines the top N most frequent J-Categories and estimates the number
#' of episodes they appeared in (assuming 5 clues per category per episode).
#'
#' @param df Input data frame (tibble) containing the 'J-Category' column.
#'   Defaults to the package's internal `jeopardy_data`.
#' @param n Number of top categories to return.
#'
#' @return A tibble with columns 'J-Category' and 'Estimated Episodes',
#'   sorted by frequency.
#' @export
calculate_top_categories <- function(df = jeopardyNLP::jeopardy_data, n = 10) {
    # Check column name - internal data uses 'category', not 'J-Category'
    category_col <- "category"
    if (!category_col %in% names(df)) {
        stop(paste("Input dataframe must contain '", category_col, "' column.", sep = ""))
    }

    df %>%
        dplyr::count(.data[[category_col]], name = "Raw Counts", sort = TRUE) %>% # Count and sort
        dplyr::slice_head(n = n) %>% # Take top N
        dplyr::mutate(`Estimated Episodes` = .data$`Raw Counts` / 5) %>%
        # Select using the dynamic col name and rename output column
        dplyr::select(category = dplyr::all_of(category_col), `Estimated Episodes` = .data$`Estimated Episodes`)
}


#' Plot Top N Categories
#'
#' Creates a bar chart showing the top N J-Categories based on their
#' estimated episode appearances.
#'
#' @param top_cats_df Data frame output from `calculate_top_categories`.
#' @param bar_color Color for the bars.
#' @param save Logical. If TRUE, saves the plot to a file.
#' @param output_dir Directory path to save the plot if `save = TRUE`.
#' @param filename Name for the saved file.
#'
#' @return Invisibly returns the ggplot object. Displays or saves the plot.
#' @export
plot_top_categories <- function(top_cats_df,
                                bar_color = "midnightblue",
                                save = FALSE,
                                output_dir = NULL,
                                filename = NULL) {
    # Ensure J-Category is treated as a factor ordered by frequency for plotting
    # The input df should already be sorted, use `fct_inorder`
    plot_df <- top_cats_df %>%
        dplyr::mutate(category = forcats::fct_inorder(.data$category))

    plot_title <- paste("Top", nrow(plot_df), "J-Categories by Estimated Episode Appearance")

    p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$`Estimated Episodes`, y = stats::reorder(.data$category, .data$`Estimated Episodes`))) +
        ggplot2::geom_col(fill = bar_color) +
        ggplot2::scale_x_continuous(expand = expansion(mult = c(0, 0.05))) + # Ensure bars start at 0
        ggplot2::labs(
            title = plot_title,
            x = "Number of Episodes (Estimated)",
            y = "J-Category"
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
            plot.title = ggplot2::element_text(hjust = 0.5),
            panel.grid.major.y = ggplot2::element_blank(), # Clean y-axis
            panel.grid.minor.y = ggplot2::element_blank()
        )
    # Note: Removed coord_flip, reordered aes y=reorder(...) instead for horizontal bars

    # Determine filename if saving
    filepath <- NULL
    if (save) {
        if (is.null(output_dir) || is.null(filename)) {
            stop("If save=TRUE, output_dir and filename must be provided.")
        }
        # If filename is provided, use it. Otherwise could stop or generate one.
        # Let's require it for saving.
        filepath <- file.path(output_dir, filename)
        message(paste("Attempting to save plot to:", filepath))
    }

    # Save or print plot
    if (save) {
        if (is.null(filepath)) stop("Filepath not set for saving.") # Safety check
        if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
        ggplot2::ggsave(filepath, plot = p, width = 7, height = 5, dpi = 200)
        message(paste("Plot saved to:", filepath))
    } else {
        print(p)
    }
    return(invisible(p))
}
