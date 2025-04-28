#' Text Cleaning Utilities
#'
#' Functions for cleaning text data, primarily designed for preparing
#' Jeopardy clues for NLP tasks. Uses the 'tm' package for efficient
#' corpus-based cleaning.
#'
#' @name jeopardyNLP-textcleaner-internal
#' @keywords internal
#' @importFrom tm content_transformer

library(tm)
library(stringr)
library(readr)
library(dplyr) # Needed for piping if used
# library(SnowballC) # Optional: Needed if using stemming
# library(textstem)  # Optional: Alternative for lemmatization

#' Extract Text Vector from DataFrame Column
#'
#' Extracts a specific column from a data frame and returns it as a character vector.
#'
#' @param df The input data frame (tibble).
#' @param col The name of the column to extract (string).
#'
#' @return A character vector containing the text data from the specified column.
#' @export
extract_text_vector <- function(df, col = "Question And Answer") {
    if (!col %in% names(df)) {
        stop(paste("Column", col, "not found in the dataframe."))
    }
    return(df[[col]])
}

#' Load Stopwords
#'
#' Loads stopwords from the standard English list provided by the `tm` package
#' and extends it with custom stopwords read from a specified file.
#' The custom stopwords file can be either newline-separated (one word per line)
#' or a single line of comma-separated words.
#'
#' @param custom_stopwords_path Path to the file containing custom stopwords.
#'   If NULL (default), attempts to load the stopwords.txt file included
#'   with the package from `inst/extdata/stopwords.txt`.
#'
#' @return A character vector containing the combined list of unique stopwords.
#' @export
load_stopwords <- function(custom_stopwords_path = NULL) {
    # Get standard English stopwords from tm package
    tm_stopwords <- tm::stopwords("en")

    # Determine path to custom stopwords
    # If path not provided, use the one included in the package
    effective_stopwords_path <- custom_stopwords_path
    if (is.null(effective_stopwords_path)) {
        pkg_stopwords_path <- system.file("extdata", "stopwords.txt", package = "jeopardyNLP")
        if (pkg_stopwords_path == "") {
            warning("Included stopwords.txt not found in jeopardyNLP package. Using only standard English stopwords.")
            effective_stopwords_path <- "" # Set to non-existent path
        } else {
            effective_stopwords_path <- pkg_stopwords_path
        }
    }

    # Read custom stopwords if file exists
    custom_sw <- character(0) # Initialize empty vector
    if (effective_stopwords_path != "" && file.exists(effective_stopwords_path)) {
        # Read the entire file content
        file_content <- readr::read_file(effective_stopwords_path)
        # Split by newline first, then by comma for each resulting line
        lines <- stringr::str_split(file_content, "\n")[[1]]
        words_list <- lapply(lines, function(line) stringr::str_split(line, ",")[[1]])
        custom_sw <- unlist(words_list)

        # Remove potential leading/trailing whitespace from each word
        custom_sw <- stringr::str_trim(custom_sw)
        # Filter out any remaining empty strings after trimming/splitting
        custom_sw <- custom_sw[custom_sw != ""]

        message(paste("Loaded", length(custom_sw), "custom stopwords from:", effective_stopwords_path))
    } else if (!is.null(custom_stopwords_path) && custom_stopwords_path != "") {
        # Only warn if a specific *user-provided* path was given but not found
        warning(paste(
            "Custom stopwords file not found:", custom_stopwords_path,
            "Using only standard English stopwords."
        ))
    }

    # Combine lists and ensure uniqueness
    all_stopwords <- unique(c(tm_stopwords, tolower(custom_sw))) # Ensure custom are lowercase
    return(all_stopwords)
}

#' Replace Hyphens with Spaces
#'
#' Replaces hyphens connecting word characters with spaces.
#' Handles patterns like word-word and word-word-word.
#'
#' @param text_vector A character vector.
#'
#' @return A character vector with hyphens replaced by spaces.
#' @export
replace_hyphens <- function(text_vector) {
    # Iteratively replace word-word hyphens until no more changes occur
    cleaned_vector <- text_vector
    old_vector <- "" # Ensure loop runs at least once
    # Use identical() for safe vector comparison
    while (!identical(cleaned_vector, old_vector)) {
        old_vector <- cleaned_vector
        # Replace only one hyphen between word characters at a time
        cleaned_vector <- stringr::str_replace_all(old_vector, "(\\w+)-(\\w+)", "\\1 \\2")
    }
    return(cleaned_vector)
}


#' Remove Short Tokens
#'
#' Removes tokens (words) from a character vector that are shorter than
#' a specified minimum length.
#'
#' @param tokens A character vector of tokens.
#' @param min_length The minimum number of characters a token must have to be kept.
#'   Defaults to 3 (keeps tokens with length > 3).
#'
#' @return A character vector containing only tokens longer than `min_length`.
#' @export
remove_short_tokens <- function(tokens, min_length = 3) {
    tokens[nchar(tokens) > min_length]
}


#' Clean Text Corpus using tm
#'
#' Performs a series of cleaning operations on a character vector of text documents
#' using the `tm` package framework. This includes lowercasing, hyphen replacement,
#' punctuation removal, number removal, stopword removal, and whitespace stripping.
#' Optionally includes stemming or lemmatization.
#' Also applies a post-tm step to remove short tokens.
#'
#' @param text_vector A character vector where each element is a document.
#' @param custom_stopwords_path Path to a custom stopwords file (one per line).
#'   If NULL (default), attempts to load the stopwords.txt file included with the package.
#' @param stem_words Logical. If TRUE, applies Porter stemming using `tm::stemDocument`.
#' @param lemmatize_words Logical. If TRUE, applies lemmatization using `textstem::lemmatize_strings`.
#'   Requires the `textstem` package to be installed.
#' @param remove_short Logical. If TRUE (default), removes tokens with 3 or fewer characters
#'   after main tm cleaning.
#' @param min_token_length Minimum length for `remove_short_tokens`.
#'
#' @return A character vector of cleaned documents, with tokens joined by spaces.
#' @export
clean_text_corpus <- function(text_vector,
                              custom_stopwords_path = NULL, # Default to package stopwords
                              stem_words = FALSE,
                              lemmatize_words = FALSE,
                              remove_short = TRUE,
                              min_token_length = 3) {
    # Handle empty input gracefully
    if (length(text_vector) == 0) {
        return(character(0))
    }

    # Input validation
    if (stem_words && lemmatize_words) {
        warning("Both stemming and lemmatization requested. Applying only stemming.")
        lemmatize_words <- FALSE
    }
    if (lemmatize_words && !requireNamespace("textstem", quietly = TRUE)) {
        warning("Package 'textstem' needed for lemmatization but is not installed. Skipping lemmatization.")
        lemmatize_words <- FALSE
    }
    if (stem_words && !requireNamespace("SnowballC", quietly = TRUE)) {
        warning("Package 'SnowballC' needed for stemming but is not installed. Skipping stemming.")
        stem_words <- FALSE
    }

    # Create a Volatile Corpus
    corpus <- tm::VCorpus(tm::VectorSource(text_vector))

    # Load stopwords
    all_stopwords <- load_stopwords(custom_stopwords_path)

    # Define tm transformation sequence
    # Order can matter (e.g., remove punctuation before stopwords)
    corpus <- tm::tm_map(corpus, tm::content_transformer(tolower))
    corpus <- tm::tm_map(corpus, tm::content_transformer(replace_hyphens)) # Custom hyphen function
    corpus <- tm::tm_map(corpus, tm::removePunctuation)
    corpus <- tm::tm_map(corpus, tm::removeNumbers)
    corpus <- tm::tm_map(corpus, tm::removeWords, all_stopwords)
    corpus <- tm::tm_map(corpus, tm::stripWhitespace)

    # Optional: Stemming
    if (stem_words) {
        message("Applying stemming...")
        corpus <- tm::tm_map(corpus, tm::stemDocument, language = "english")
    }

    # Optional: Lemmatization (requires textstem package)
    if (lemmatize_words) {
        message("Applying lemmatization (requires textstem package)...")
        # Need a wrapper function because tm_map expects function(x, ...)
        lemmatize_doc <- function(doc) {
            # Convert TextDocument back to character for lemmatize_strings
            text <- as.character(doc)
            lemmatized_text <- textstem::lemmatize_strings(text)
            return(lemmatized_text)
        }
        corpus <- tm::tm_map(corpus, content_transformer(lemmatize_doc))
    }

    # Extract cleaned text back into a character vector
    cleaned_text <- sapply(corpus, as.character)

    # Post-tm cleaning steps (mimicking original Python script logic)
    if (remove_short) {
        # Tokenize again, remove short tokens, and rejoin
        # This is less efficient than integrating into tm if possible,
        # but replicates the original approach where short token removal
        # happened after other steps.
        message(paste("Removing tokens with length <=", min_token_length))
        cleaned_list <- lapply(cleaned_text, function(doc) {
            tokens <- stringr::str_split(doc, "\\s+")[[1]]
            tokens <- tokens[tokens != ""] # Remove empty strings from split
            tokens <- remove_short_tokens(tokens, min_length = min_token_length)
            paste(tokens, collapse = " ") # Rejoin tokens
        })
        cleaned_text <- unlist(cleaned_list)
    }

    return(cleaned_text)
}
