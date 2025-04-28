library(testthat)
library(dplyr)
library(jeopardyNLP)
library(tm) # Needed for creating test matrix
library(Matrix) # Needed for matrix operations
library(NMF) # Needed for NMF object checks

# --- Helper Data / Setup ---

# Create a very small sample text corpus
sample_text_nmf <- c(
    "government president election state",
    "president state law court",
    "geography island river lake",
    "island lake mountain sea",
    "art film book author",
    "film book novel play star"
)

# Create a TF-IDF matrix for testing NMF functions
# Note: Requires tm to be available
create_test_tfidf <- function(text = sample_text_nmf) {
    corpus <- VCorpus(VectorSource(text))
    tdm <- TermDocumentMatrix(corpus, control = list(weighting = weightTfIdf))
    # Ensure non-negative (TF-IDF can sometimes produce tiny negatives due to float precision)
    tdm$v[tdm$v < 0] <- 0
    # Remove empty rows/cols (though unlikely with this sample)
    term_sums <- slam::row_sums(tdm)
    tdm <- tdm[term_sums > 0, ]
    doc_sums <- slam::col_sums(tdm)
    tdm <- tdm[, doc_sums > 0]
    return(as.matrix(tdm)) # Convert to dense for simpler NMF test run
}

# Create a sample NMF result for testing extractors
create_sample_nmf_result <- function(k = 2) {
    skip_if_not_installed("NMF")
    mat <- create_test_tfidf()
    # Use a fast method for testing, maybe fewer iterations
    suppressMessages(NMF::nmf(mat, rank = k, method = "lee", nrun = 1, seed = 123, .options = "v"))
}

# --- Tests ---

test_that("create_tfidf_matrix works", {
    skip_if_not_installed("tm")
    skip_if_not_installed("slam")

    tfidf_sparse <- create_tfidf_matrix(sample_text_nmf,
        control = list(
            weighting = weightTfIdf,
            bounds = list(global = c(1, Inf))
        )
    ) # Min freq 1

    expect_true(inherits(tfidf_sparse, "TermDocumentMatrix"))
    expect_true(nrow(tfidf_sparse) > 0)
    expect_equal(ncol(tfidf_sparse), length(sample_text_nmf))
    expect_true(all(tfidf_sparse$v >= 0))
})

test_that("run_nmf_model works", {
    skip_if_not_installed("NMF")
    test_mat <- create_test_tfidf()

    nmf_res <- suppressMessages(run_nmf_model(test_mat, k = 2, seed = 42, method = "lee", .options = "v")) # Faster method

    expect_s4_class(nmf_res, "NMF")
    expect_equal(nbasis(nmf_res), 2)
    expect_equal(dim(basis(nmf_res)), c(nrow(test_mat), 2))
    expect_equal(dim(coef(nmf_res)), c(2, ncol(test_mat)))
})

test_that("run_nmf_model handles negative values with warning", {
    skip_if_not_installed("NMF")
    test_mat_neg <- create_test_tfidf()
    test_mat_neg[1, 1] <- -0.1 # Introduce a negative value

    expect_warning(run_nmf_model(test_mat_neg, k = 2, seed = 42, method = "lee", .options = "v"), "contains negative values")
    # Check it still runs
    nmf_res <- suppressWarnings(suppressMessages(run_nmf_model(test_mat_neg, k = 2, seed = 42, method = "lee", .options = "v")))
    expect_s4_class(nmf_res, "NMF")
})

test_that("get_nmf_residuals works", {
    skip_if_not_installed("NMF")
    nmf_res <- create_sample_nmf_result()
    res <- get_nmf_residuals(nmf_res)
    expect_type(res, "double")
    expect_length(res, 1)
    expect_true(res >= 0)
})

test_that("get_top_words_per_topic works", {
    skip_if_not_installed("NMF")
    k <- 2
    nmf_res <- create_sample_nmf_result(k = k)

    top_words <- get_top_words_per_topic(nmf_res, n_top_words = 3)

    expect_type(top_words, "list")
    expect_length(top_words, k)
    expect_named(top_words, paste0("Topic_", 1:k))
    expect_type(top_words[[1]], "character")
    expect_length(top_words[[1]], 3)
})

test_that("get_word_weights_for_cloud works", {
    skip_if_not_installed("NMF")
    nmf_res <- create_sample_nmf_result()

    weights <- get_word_weights_for_cloud(nmf_res, topic_index = 1, n_top_words = 5)

    expect_type(weights, "double")
    expect_true(length(weights) <= 5)
    expect_true(!is.null(names(weights)))
    expect_true(all(weights >= 0))

    # Test invalid index
    expect_warning(get_word_weights_for_cloud(nmf_res, topic_index = 10), "Invalid topic_index")
    expect_null(suppressWarnings(get_word_weights_for_cloud(nmf_res, topic_index = 10)))
})

# --- Tests for Evaluation/Plotting (Basic Checks) ---

test_that("evaluate_nmf_ranks runs", {
    skip_if_not_installed("NMF")
    skip_if_not_installed("purrr")
    test_mat <- create_test_tfidf()

    ranks_to_test <- 2:3
    eval_res <- suppressMessages(
        evaluate_nmf_ranks(test_mat, ranks = ranks_to_test, seed = 42, method = "lee", .options = "v")
    )

    expect_s3_class(eval_res, "tbl_df")
    expect_equal(nrow(eval_res), length(ranks_to_test))
    expect_named(eval_res, c("k", "metric"))
    expect_true(all(!is.na(eval_res$metric))) # Should succeed for this small example
})

test_that("plot_topic_word_cloud runs without error", {
    skip_if_not_installed("NMF")
    skip_if_not_installed("wordcloud")
    nmf_res <- create_sample_nmf_result()

    # Just check that it runs without error
    expect_error(plot_topic_word_cloud(nmf_res, topic_index = 1, save = FALSE), NA)

    # Check saving needs args
    expect_error(
        plot_topic_word_cloud(nmf_res, topic_index = 1, save = TRUE),
        "output_dir and filename must be provided"
    )
})

test_that("plot_all_topic_clouds runs without error", {
    skip_if_not_installed("NMF")
    skip_if_not_installed("wordcloud")
    nmf_res <- create_sample_nmf_result(k = 2)

    # Just check that it runs without error
    expect_message(plot_all_topic_clouds(nmf_res, save = FALSE), "Generating word cloud for Topic 1")
    expect_message(plot_all_topic_clouds(nmf_res, save = FALSE), "Generating word cloud for Topic 2")
    expect_error(plot_all_topic_clouds(nmf_res, save = FALSE), NA)

    # Check saving needs args
    expect_error(
        plot_all_topic_clouds(nmf_res, save = TRUE),
        "output_dir must be provided"
    )
})

test_that("plot_nmf_evaluation runs without error", {
    skip_if_not_installed("ggplot2")
    eval_res <- tibble::tibble(k = 2:4, metric = c(10, 5, 3))

    p <- plot_nmf_evaluation(eval_res, save = FALSE)
    expect_s3_class(p, "ggplot")

    # Check saving needs args
    expect_error(
        plot_nmf_evaluation(eval_res, save = TRUE),
        "output_dir and filename must be provided"
    )
})
