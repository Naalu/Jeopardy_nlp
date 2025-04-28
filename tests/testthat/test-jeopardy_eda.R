library(testthat)
library(dplyr)
library(ggplot2)
library(jeopardyNLP)

# --- Helper Data ---
sample_episodes_eda <- tibble::tribble(
    ~`J-Category`, ~`Clue Difficulty`, ~Answer, ~Question, ~`Question And Answer`,
    "HISTORY", "easy", "Answer 1", "Question text one", "Q A 1",
    "SCIENCE", "average", "Ans 2", "Question two words", "Q A 2",
    "HISTORY", "easy", "Answer three", "Question three is here", "Q A 3",
    "LITERATURE", "hard", "Ans 4", "Question text four four", "Q A 4",
    "SCIENCE", "average", "Answer five", "Question five five five", "Q A 5",
    "HISTORY", "hard", "A 6", "Q six six six six six", "Q A 6"
)

# Rename column for consistency with function expectations
sample_episodes_eda <- sample_episodes_eda %>%
    dplyr::rename(category = `J-Category`)

# --- Tests ---

test_that("calculate_word_counts works", {
    # Note: calculate_word_counts uses 'clue_difficulty', 'answer', 'question'
    # Need to ensure sample_episodes_eda has these lowercase names if the function expects them
    # Let's adjust the sample data creation AND rename step if needed.
    # Current sample uses 'Clue Difficulty', 'Answer', 'Question'
    # Let's rename them too for robustness, assuming functions might expect lowercase.
    test_df_wc <- sample_episodes_eda %>%
        dplyr::rename(
            clue_difficulty = `Clue Difficulty`,
            answer = Answer,
            question = Question
        )

    counts <- calculate_word_counts(test_df_wc)
    expect_equal(nrow(counts), 6)
    expect_named(counts, c("Clue Difficulty", "Answer Word Count", "Question Word Count"))
    expect_equal(counts$`Answer Word Count`, c(2, 2, 2, 2, 2, 2))
    expect_equal(counts$`Question Word Count`, c(3, 3, 4, 4, 4, 6))
})

test_that("calculate_top_categories works", {
    top2 <- calculate_top_categories(sample_episodes_eda, n = 2)
    expect_equal(nrow(top2), 2)
    expect_named(top2, c("category", "Estimated Episodes"))
    expect_equal(top2$category, c("HISTORY", "SCIENCE"))
    expect_equal(top2$`Estimated Episodes`, c(3 / 5, 2 / 5))

    top5 <- calculate_top_categories(sample_episodes_eda, n = 5)
    expect_equal(nrow(top5), 3) # Only 3 unique categories in sample
})

# --- Plotting Function Tests (Basic Checks) ---
# Testing plots thoroughly is complex. Focus on running without error,
# checking return types, and maybe basic structure/class.

test_that("plot_word_cloud runs without error", {
    # Test calculation part by checking returned value (invisibly)
    freq_df <- suppressMessages(
        plot_word_cloud(sample_episodes_eda, "category", save = FALSE)
    )
    expect_s3_class(freq_df, "data.frame")
    expect_true(nrow(freq_df) > 0)
    expect_named(freq_df, c("word", "freq"))

    # Test saving requires args
    expect_error(
        plot_word_cloud(sample_episodes_eda, "category", save = TRUE),
        "output_dir and filename must be provided"
    )

    # Test run with saving (to temp file)
    temp_file <- tempfile(fileext = ".png")
    expect_message(
        plot_word_cloud(sample_episodes_eda, "category",
            save = TRUE,
            output_dir = dirname(temp_file),
            filename = basename(temp_file)
        ),
        "saved to"
    )
    expect_true(file.exists(temp_file))
    unlink(temp_file)
})

test_that("plot_word_counts_difficulty runs without error", {
    # Rename cols needed by calculate_word_counts for this test call
    test_df_wc_plot <- sample_episodes_eda %>%
        dplyr::rename(
            clue_difficulty = `Clue Difficulty`,
            answer = Answer,
            question = Question
        )
    counts <- calculate_word_counts(test_df_wc_plot)

    # Test plotting runs and returns ggplot object
    p <- plot_word_counts_difficulty(counts, "Question Word Count", save = FALSE)
    expect_s3_class(p, "ggplot")

    # Test saving requires args
    expect_error(
        plot_word_counts_difficulty(counts, "Question Word Count", save = TRUE),
        "output_dir and filename must be provided"
    )

    # Test run with saving (to temp file)
    temp_file <- tempfile(fileext = ".png")
    expect_message(
        plot_word_counts_difficulty(counts, "Question Word Count",
            save = TRUE,
            output_dir = dirname(temp_file),
            filename = basename(temp_file)
        ),
        "saved to"
    )
    expect_true(file.exists(temp_file))
    unlink(temp_file)
})

test_that("plot_top_categories runs without error", {
    top_cats <- calculate_top_categories(sample_episodes_eda, n = 3)

    # Test plotting runs and returns ggplot object
    p <- plot_top_categories(top_cats, save = FALSE)
    expect_s3_class(p, "ggplot")

    # Test saving requires args
    expect_error(
        plot_top_categories(top_cats, save = TRUE),
        "output_dir and filename must be provided"
    )

    # Test run with saving (to temp file)
    temp_file <- tempfile(fileext = ".png")
    expect_message(
        plot_top_categories(top_cats,
            save = TRUE,
            output_dir = dirname(temp_file),
            filename = basename(temp_file)
        ),
        "saved to"
    )
    expect_true(file.exists(temp_file))
    unlink(temp_file)
})
