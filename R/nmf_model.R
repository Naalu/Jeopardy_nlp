#' Non-Negative Matrix Factorization (NMF) for Topic Modeling
#'
#' Functions for applying NMF to the Jeopardy dataset to discover underlying topics
#' within the clues. Includes text preparation specific to NMF and visualization
#' of results.
#'
#' @name jeopardyNLP-nmf-internal
#' @keywords internal
#' @importFrom NMF nmf basis residuals
#' @importFrom tm VCorpus VectorSource DocumentTermMatrix weightTfIdf
#' @importFrom Matrix t
#' @importFrom slam row_sums col_sums
#' @importFrom wordcloud wordcloud
#' @importFrom RColorBrewer brewer.pal brewer.pal.info
#' @importFrom grDevices png dev.off

library(NMF) # For NMF algorithm
library(tm) # For DocumentTermMatrix, TF-IDF
library(dplyr)
library(ggplot2)
library(wordcloud)
library(readr)
library(purrr) # For mapping functions
library(tibble) # For data frame creation
library(tidyr) # For unnesting (if needed elsewhere)
library(RColorBrewer) # For word cloud palettes
library(slam) # For sparse matrix operations

# --- Functions --- #

#' Create TF-IDF Matrix
#'
#' Creates a Term-Document Matrix (TDM) with TF-IDF weighting from a vector of
#' cleaned text documents. Assumes input text is already processed (lowercase,
#' punctuation removed, stopwords removed, etc.). The final matrix has terms as rows
#' and documents as columns, as typically expected by the NMF package.
#'
#' @param text_vector A character vector where each element represents a document.
#' @param control A list of control parameters passed to `tm::DocumentTermMatrix`.
#'   Defaults include TF-IDF weighting. See `?TermDocumentMatrix` for options
#'   (e.g., `list(weighting = weightTfIdf, bounds = list(global = c(5, Inf)))`).
#'
#' @return A sparse TermDocumentMatrix (dgTMatrix) with TF-IDF weights.
#'   Terms are rows, documents are columns.
#' @export
create_tfidf_matrix <- function(text_vector, control = list(weighting = tm::weightTfIdf)) {
    # Create corpus
    corpus <- tm::VCorpus(tm::VectorSource(text_vector))

    # Create Document-Term Matrix first
    dtm_tfidf <- tm::DocumentTermMatrix(corpus, control = control)

    # Transpose to get Term-Document Matrix (terms as rows)
    # NMF package typically works best with terms/features as rows
    tdm_tfidf <- Matrix::t(dtm_tfidf)

    # Remove rows (terms) with sum zero (e.g., if resulted from cleaning/bounds)
    # Uses slam::row_sums for efficiency with sparse matrices
    term_sums <- slam::row_sums(tdm_tfidf)
    tdm_tfidf <- tdm_tfidf[term_sums > 0, ]

    # Remove columns (documents) with sum zero
    doc_sums <- slam::col_sums(tdm_tfidf)
    tdm_tfidf <- tdm_tfidf[, doc_sums > 0]

    return(tdm_tfidf)
}

#' Run NMF Model
#'
#' Performs Non-negative Matrix Factorization on a given matrix (typically TF-IDF TDM).
#'
#' @param term_doc_matrix A numeric matrix where rows are terms/features and columns
#'   are documents (e.g., output from `create_tfidf_matrix`). Matrix must be non-negative.
#' @param k The desired number of topics (rank for factorization).
#' @param seed Random seed for reproducibility.
#' @param method The NMF algorithm to use (e.g., "brunet", "lee", "nsNMF"). See `?nmf`.
#' @param ... Additional arguments passed to `NMF::nmf`.
#'
#' @return An object of class `NMF` resulting from the factorization.
#' @export
run_nmf_model <- function(term_doc_matrix, k, seed = 43, method = "brunet", ...) {
    message("  -> Entering run_nmf_model function...") # DEBUG
    # Check for non-negativity
    if (any(term_doc_matrix < 0)) {
        warning("  Input matrix contains negative values. NMF requires non-negative matrices. Attempting to set negative values to 0.")
        term_doc_matrix[term_doc_matrix < 0] <- 0
    }
    # Set seed for reproducibility
    set.seed(seed)

    # Perform NMF
    message(paste("    About to call NMF::nmf with method:", method, "for k =", k)) # DEBUG
    nmf_result <- NMF::nmf(term_doc_matrix, rank = k, method = method, seed = seed, ...)
    message(paste("    NMF::nmf call completed for k=", k)) # DEBUG
    message("  <- Exiting run_nmf_model function.") # DEBUG
    return(nmf_result)
}

#' Get NMF Residuals
#'
#' Extracts the Frobenius norm of the residuals (||X - WH||) from a fitted NMF model object.
#' This serves as a proxy for reconstruction error.
#'
#' @param nmf_result An object of class `NMF` (output from `run_nmf_model` or `NMF::nmf`).
#'
#' @return The Frobenius norm of the residuals.
#' @export
get_nmf_residuals <- function(nmf_result) {
    # Use the residuals() extractor function for NMF objects
    # s <- NMF::summary(nmf_result)
    # return(s$residuals) # Incorrect access
    return(NMF::residuals(nmf_result))
}

#' Get Top Words Per Topic
#'
#' Extracts the top N most heavily weighted words for each topic from the
#' basis matrix (W) of a fitted NMF model.
#'
#' @param nmf_result An object of class `NMF`.
#' @param n_top_words The number of top words to retrieve for each topic.
#'
#' @return A list where each element corresponds to a topic and contains a
#'   character vector of the top `n_top_words` for that topic.
#' @export
get_top_words_per_topic <- function(nmf_result, n_top_words = 10) {
    # W = basis matrix (terms x topics)
    w_matrix <- NMF::basis(nmf_result)

    top_words_list <- list()
    feature_names <- rownames(w_matrix)

    if (is.null(feature_names)) {
        stop("Term names (rownames) not found in the NMF basis matrix.")
    }

    num_topics <- ncol(w_matrix)
    n_top_words <- min(n_top_words, nrow(w_matrix)) # Ensure n_top doesn't exceed available terms

    for (i in 1:num_topics) { # Iterate through topics (columns)
        topic_scores <- w_matrix[, i]
        top_indices <- order(topic_scores, decreasing = TRUE)[1:n_top_words]
        top_words <- feature_names[top_indices]
        top_words_list[[paste0("Topic_", i)]] <- top_words
    }
    return(top_words_list)
}

#' Get Word Weights for Topic Word Cloud
#'
#' Extracts the top N words and their corresponding weights (scores) from the
#' basis matrix for a specific topic. Applies square root scaling to weights
#' similar to the original Python implementation for word cloud frequency scaling.
#'
#' @param nmf_result An object of class `NMF`.
#' @param topic_index The index (1-based) of the topic to extract weights for.
#' @param n_top_words The number of top words/weights to return.
#'
#' @return A named numeric vector where names are the top words and values are
#'   their scaled weights (sqrt(score)). Returns NULL if topic_index is invalid
#'   or no valid weights are found.
#' @export
get_word_weights_for_cloud <- function(nmf_result, topic_index, n_top_words = 15) {
    w_matrix <- NMF::basis(nmf_result)
    feature_names <- rownames(w_matrix)

    if (is.null(feature_names)) {
        stop("Term names (rownames) not found in the NMF basis matrix.")
    }
    if (topic_index > ncol(w_matrix) || topic_index < 1) {
        warning(paste("Invalid topic_index:", topic_index))
        return(NULL)
    }

    topic_scores <- w_matrix[, topic_index]
    names(topic_scores) <- feature_names

    # Sort scores and take top N
    n_top_words <- min(n_top_words, length(topic_scores))
    top_scores <- sort(topic_scores, decreasing = TRUE)[1:n_top_words]

    # Apply scaling for word cloud frequency (sqrt)
    # Ensure non-negative before sqrt; NMF should guarantee this, but check.
    top_scores[top_scores < 0] <- 0
    word_freqs <- sqrt(top_scores)

    # Ensure no NA/NaN/Inf values and finite scores > 0
    word_freqs <- word_freqs[is.finite(word_freqs) & word_freqs > 0]

    if (length(word_freqs) == 0) {
        return(NULL)
    }

    return(word_freqs) # Returns a named vector (word = scaled_weight)
}

#' Plot Topic Word Cloud
#'
#' Generates and displays/saves a word cloud for a specific NMF topic.
#'
#' @param nmf_result An object of class `NMF`.
#' @param topic_index The index (1-based) of the topic to visualize.
#' @param n_top_words The number of top words to include in the cloud.
#' @param color_palette Name of the RColorBrewer palette to use.
#' @param save Logical. If TRUE, saves the plot to a file.
#' @param output_dir Directory path to save the plot if `save = TRUE`.
#' @param filename Optional filename. Defaults to `_topic_X_wordcloud.png` (using 1-based X).
#'
#' @return Nothing. Displays or saves the word cloud plot.
#' @export
plot_topic_word_cloud <- function(nmf_result,
                                  topic_index,
                                  n_top_words = 15,
                                  color_palette = "Dark2",
                                  save = FALSE,
                                  output_dir = NULL,
                                  filename = NULL) {
    word_freqs <- get_word_weights_for_cloud(nmf_result, topic_index, n_top_words)

    # Ensure there are words to plot
    if (is.null(word_freqs) || length(word_freqs) == 0) {
        warning(paste("No valid words/frequencies found for topic", topic_index, ". Skipping word cloud."))
        return()
    }

    # Select a valid brewer palette
    num_colors <- max(3, min(length(word_freqs), 8))
    available_palettes <- rownames(RColorBrewer::brewer.pal.info)
    if (!color_palette %in% available_palettes) {
        warning(paste("Palette", color_palette, "not found, using 'Set2' instead."))
        color_palette <- "Set2"
    }
    colors_to_use <- RColorBrewer::brewer.pal(num_colors, color_palette)

    # Determine filename
    filepath <- NULL
    if (save) {
        if (is.null(output_dir) || is.null(filename)) {
            stop("If save=TRUE, output_dir and filename must be provided.")
        }
        filepath <- file.path(output_dir, filename)
        message(paste("Attempting to save word cloud to:", filepath))
    }

    # Save or display
    if (save) {
        if (is.null(filepath)) stop("Filepath not set for saving.") # Safety check
        if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
        grDevices::png(filepath, width = 800, height = 800)
    }

    wordcloud::wordcloud(
        words = names(word_freqs),
        freq = word_freqs,
        scale = c(4, 0.5), # Adjust scale as needed
        min.freq = 0, # Show all words passed
        max.words = n_top_words,
        random.order = FALSE,
        rot.per = 0.35,
        colors = colors_to_use
    )
    if (save) {
        grDevices::dev.off()
        message(paste("Word cloud saved to:", filepath))
    }
}

#' Plot All Topic Word Clouds
#'
#' Generates word clouds for all topics in a fitted NMF model.
#'
#' @param nmf_result An object of class `NMF`.
#' @param n_top_words Number of top words per topic cloud.
#' @param color_palette RColorBrewer palette name.
#' @param save Logical. If TRUE, saves plots to files.
#' @param output_dir Directory path for saving plots.
#' @param filename_prefix Allow custom prefix, but no full default filename
#'
#' @return Nothing. Generates multiple word clouds.
#' @export
plot_all_topic_clouds <- function(nmf_result,
                                  n_top_words = 15,
                                  color_palette = "Dark2",
                                  save = FALSE,
                                  output_dir = NULL,
                                  filename_prefix = "topic_") {
    num_topics <- NMF::nbasis(nmf_result)
    for (i in 1:num_topics) {
        message(paste("Generating word cloud for Topic", i))

        # Construct filename if saving
        current_filename <- NULL
        if (save) {
            if (is.null(output_dir)) {
                stop("If save=TRUE, output_dir must be provided.")
            }
            # Create filename like "topic_1_wordcloud.png"
            current_filename <- paste0(filename_prefix, i, "_wordcloud.png")
        }

        # Use tryCatch to prevent one failure from stopping the loop
        tryCatch(
            {
                plot_topic_word_cloud(
                    nmf_result = nmf_result,
                    topic_index = i,
                    n_top_words = n_top_words,
                    color_palette = color_palette,
                    save = save,
                    output_dir = output_dir,
                    filename = current_filename
                ) # Pass constructed filename
            },
            error = function(e) {
                warning(paste("Failed to generate word cloud for Topic", i, ":", e$message))
            }
        )
    }
}


#' Evaluate NMF Ranks (Find Best k)
#'
#' Runs NMF for a range of ranks (k) and collects a specified metric
#' (defaulting to Frobenius residuals) to help determine an optimal number of topics.
#'
#' @param term_doc_matrix Input term-document matrix.
#' @param ranks A numeric vector of ranks (k values) to evaluate.
#' @param seed Random seed for reproducibility.
#' @param method NMF algorithm method.
#' @param metric Function to extract the evaluation metric from the NMF result object.
#'   Defaults to `get_nmf_residuals`.
#' @param n_runs Number of NMF runs per rank (if using methods that benefit from multiple runs).
#' @param ... Additional arguments passed to `NMF::nmf`.
#'
#' @return A tibble with columns 'k' (rank) and 'metric' (the evaluated metric value).
#' @export
evaluate_nmf_ranks <- function(term_doc_matrix,
                               ranks = 7:15,
                               seed = 43,
                               method = "brunet",
                               metric = get_nmf_residuals,
                               n_runs = 1,
                               ...) {
    message("-> Entering evaluate_nmf_ranks function.") # DEBUG
    message(paste("  Input matrix dimensions:", paste(dim(term_doc_matrix), collapse = " x "))) # DEBUG
    message("  Input matrix summary:") # DEBUG
    print(summary(as.vector(term_doc_matrix))) # DEBUG

    # Check if matrix is suitable (non-negative)
    if (any(term_doc_matrix < 0)) {
        warning("Input matrix contains negative values. NMF requires non-negative matrices.")
        # Optionally attempt to fix or stop
        # term_doc_matrix[term_doc_matrix < 0] <- 0
    }

    # Use NMF::nmfEstimateRank if n_runs > 1 or want more metrics, otherwise simple loop
    if (n_runs > 1) {
        # This provides more robust evaluation metrics like RSS, cophenetic correlation
        message(paste("Evaluating ranks using nmfEstimateRank with nrun=", n_runs))
        set.seed(seed)
        rank_estimation <- NMF::nmfEstimateRank(term_doc_matrix, ranks, method = method, nrun = n_runs, seed = seed, ...)
        # Extract desired metric (e.g., residuals or cophenetic) - depends on what metric function expects
        # For now, let's assume we still want residuals, we might need to adapt if using cophenetic
        # This part needs refinement based on the desired metric vs. nmfEstimateRank output structure
        # As a placeholder, let's just run it once per k for simplicity matching the loop below
        warning("n_runs > 1 requested, but evaluation logic currently uses single run per k. Set n_runs=1 or modify logic.")
        n_runs <- 1
    }

    # Simple loop for single run or if nmfEstimateRank isn't desired
    results_list <- purrr::map(ranks, function(k) {
        message(paste("-- Evaluating k =", k, "--")) # DEBUG
        metric_value <- NA
        nmf_res <- NULL # Initialize nmf_res
        # Use tryCatch in case NMF fails for some k
        nmf_res <- tryCatch(
            {
                message(paste("  Attempting NMF run for k=", k)) # DEBUG
                # Single run
                res <- run_nmf_model(term_doc_matrix, k, seed = seed, method = method, ...)
                message(paste("  NMF run successful for k=", k)) # DEBUG
                res # Return the result
            },
            error = function(e) {
                warning(paste("  NMF failed for k=", k, ":", e$message)) # DEBUG
                NULL # Return NULL on error
            }
        )

        if (!is.null(nmf_res)) {
            message(paste("  Calculating metric for k=", k)) # DEBUG
            metric_value <- tryCatch(
                {
                    val <- metric(nmf_res)
                    message(paste("    Metric value:", val)) # DEBUG
                    val
                },
                error = function(e) {
                    warning(paste("    Metric calculation failed for k=", k, ":", e$message))
                    NA # Return NA if metric calculation fails
                }
            )
        } else {
            message(paste("  Skipping metric calculation for k=", k, "due to NMF failure.")) # DEBUG
        }
        # Return named list for map_dfr
        list(k = k, metric = metric_value)
    })

    # Convert results list to a tibble
    results_df <- dplyr::bind_rows(results_list)
    message("-- Final evaluation results data frame: --") # DEBUG
    print(results_df) # DEBUG
    return(results_df)
}


#' Plot NMF Rank Evaluation Metric
#'
#' Plots the chosen evaluation metric (e.g., residuals) against the number of topics (k).
#'
#' @param k_results_df A data frame/tibble from `evaluate_nmf_ranks` with columns 'k' and 'metric'.
#' @param metric_name A string label for the y-axis (e.g., "Frobenius Residuals").
#' @param save Logical. If TRUE, saves the plot to a file.
#' @param output_dir Directory path for saving.
#' @param filename Filename for the saved plot.
#'
#' @return Invisibly returns the ggplot object. Displays or saves the plot.
#' @export
plot_nmf_evaluation <- function(k_results_df,
                                metric_name = "Residuals",
                                save = FALSE,
                                output_dir = NULL,
                                filename = NULL) {
    # Filter out NA values if NMF failed for some k
    plot_data <- k_results_df %>% dplyr::filter(!is.na(metric))

    if (nrow(plot_data) == 0) {
        stop("No valid NMF results to plot.")
    }

    plot_title <- paste("Number of Topics (k) vs.", metric_name)

    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$k, y = .data$metric)) +
        ggplot2::geom_line(color = "darkmagenta") +
        ggplot2::geom_point(color = "darkmagenta") +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) + # Nicer x-axis breaks
        ggplot2::labs(
            title = plot_title,
            x = "Number of Topics (k)",
            y = metric_name
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

    # Determine filename if saving
    filepath <- NULL
    if (save) {
        if (is.null(output_dir) || is.null(filename)) {
            stop("If save=TRUE, output_dir and filename must be provided.")
        }
        filepath <- file.path(output_dir, filename)
        message(paste("Attempting to save plot to:", filepath))
    }

    # Save or print plot
    if (save) {
        if (is.null(filepath)) stop("Filepath not set for saving.") # Safety check
        if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
        ggplot2::ggsave(filepath, plot = p, width = 8, height = 6, dpi = 150)
        message(paste("Plot saved to:", filepath))
    } else {
        print(p)
    }
    return(invisible(p))
}
