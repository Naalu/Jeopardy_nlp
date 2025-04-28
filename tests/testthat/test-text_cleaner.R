library(testthat)
library(dplyr)
library(jeopardyNLP)

# --- Helper Data ---
test_df <- tibble::tibble(
    id = 1:3,
    `Question And Answer` = c(
        "This is document 1. It has numbers 123 and punctuation! Also hyphens state-of-the-art.",
        "Document 2 has \"quotes\" and short words like a an the it.",
        "Final doc 3 with email@address.com and www.website.net"
    ),
    other_col = c("a", "b", "c")
)

test_vector <- test_df$`Question And Answer`

# --- Tests ---

test_that("extract_text_vector works correctly", {
    expect_equal(extract_text_vector(test_df), test_vector)
    expect_equal(extract_text_vector(test_df, col = "other_col"), c("a", "b", "c"))
    expect_error(extract_text_vector(test_df, col = "non_existent"), "not found")
})

test_that("replace_hyphens works", {
    input <- c("state-of-the-art", "no-hyphen", "multi word phrase", "word-")
    expected <- c("state of the art", "no hyphen", "multi word phrase", "word-")
    expect_equal(replace_hyphens(input), expected)
})

test_that("remove_short_tokens works", {
    input <- c("a", "an", "the", "word", "longer", "it", "is")
    # Keep words with nchar > 1: "an", "the", "word", "longer", "it", "is"
    expect_equal(remove_short_tokens(input, min_length = 1), c("an", "the", "word", "longer", "it", "is"))
    expect_equal(remove_short_tokens(input, min_length = 2), c("the", "word", "longer"))
    expect_equal(remove_short_tokens(input, min_length = 3), c("word", "longer")) # Default
    expect_equal(remove_short_tokens(character(0)), character(0)) # Empty input
})

test_that("load_stopwords default works", {
    # Assumes the package structure is findable via devtools::load_all or similar
    # Requires the stopwords.txt to be in inst/extdata
    default_sw <- suppressMessages(load_stopwords()) # Suppress loading message
    expect_true(length(default_sw) > 50) # Should have tm + custom
    expect_true("i" %in% default_sw) # Standard tm stopword
    expect_true("word" %in% default_sw) # Check for a known custom stopword
    expect_true(all(tolower(default_sw) == default_sw)) # Check lowercase
})

test_that("load_stopwords handles custom newline file", {
    # Create a temporary newline-separated file
    temp_file <- tempfile(fileext = ".txt")
    writeLines(c("custom1", " custom2 ", "", "custom3"), temp_file)
    on.exit(unlink(temp_file)) # Ensure cleanup

    custom_sw <- suppressMessages(load_stopwords(custom_stopwords_path = temp_file))
    expect_true("custom1" %in% custom_sw)
    expect_true("custom2" %in% custom_sw)
    expect_true("custom3" %in% custom_sw)
    expect_false("" %in% custom_sw)
    expect_true(length(custom_sw) > 3) # Includes tm stopwords
})

test_that("load_stopwords handles custom comma file", {
    # Create a temporary comma-separated file
    temp_file <- tempfile(fileext = ".txt")
    writeLines(c("comma1, comma2 ,, custom3 , comma4"), temp_file)
    on.exit(unlink(temp_file))

    custom_sw <- suppressMessages(load_stopwords(custom_stopwords_path = temp_file))
    expect_true("comma1" %in% custom_sw)
    expect_true("comma2" %in% custom_sw)
    expect_true("custom3" %in% custom_sw)
    expect_true("comma4" %in% custom_sw)
    expect_false(" " %in% custom_sw)
    expect_true(length(custom_sw) > 4) # Includes tm stopwords
})

test_that("load_stopwords handles missing custom file", {
    expect_warning(load_stopwords("definitely_missing_file.txt"), "not found")
    # Check it still returns tm stopwords
    tm_sw <- tm::stopwords("en")
    expect_equal(suppressWarnings(load_stopwords("definitely_missing_file.txt")), tm_sw)
})

test_that("clean_text_corpus performs default cleaning", {
    cleaned <- clean_text_corpus(test_vector)
    expect_equal(length(cleaned), 3)
    # Check results of default cleaning (lowercase, hyphen, punct, numbers, stopwords, short tokens)
    # Example check on first doc
    expect_false(grepl("[0-9]", cleaned[1]))
    expect_false(grepl("[[:punct:]]", cleaned[1]))
    # expect_false(grepl("document", cleaned[1])) # 'document' is not a default stopword
    expect_true(grepl("state", cleaned[1])) # hyphen replaced (depends on Fix #4)
    expect_false(grepl("art", cleaned[1])) # short token removed (art)
    expect_false(grepl(" the | it ", cleaned[2])) # Standard stopwords
    # expect_false(grepl("emailaddresscom", cleaned[3])) # Punct removed correctly leads to this string
    # Short token removal (<=3) - check 'doc' is removed
    expect_false(grepl(" doc ", cleaned[3]))
})

# Note: Testing stemming/lemmatization requires optional packages
# and specific checks might be complex. Basic run checks could be added:
test_that("clean_text_corpus stem/lemma options run if packages installed", {
    skip_if_not_installed("SnowballC")
    cleaned_stem <- suppressMessages(clean_text_corpus(test_vector, stem_words = TRUE))
    expect_equal(length(cleaned_stem), 3)
    # Stemming might change word forms, e.g., 'document' might become 'document'
    # A better test would check for known stem changes.

    skip_if_not_installed("textstem")
    cleaned_lemma <- suppressMessages(clean_text_corpus(test_vector, lemmatize_words = TRUE))
    expect_equal(length(cleaned_lemma), 3)
    # Lemmatization should also change forms, e.g., 'has' might become 'have'
})

test_that("clean_text_corpus handles empty input", {
    expect_equal(clean_text_corpus(character(0)), character(0))
    cleaned_empty <- clean_text_corpus(c("", ""))
    expect_equal(unname(cleaned_empty), c("", "")) # Compare unamed vector
})
