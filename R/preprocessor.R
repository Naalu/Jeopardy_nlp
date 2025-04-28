#' Data Preprocessing Utilities
#'
#' Functions for reading, cleaning, and preparing the raw Jeopardy TSV data
#' for analysis. Includes column renaming, filtering, and adding derived columns
#' like clue difficulty.
#'
#' @name jeopardyNLP-preprocessor-internal
#' @keywords internal

# Load required libraries
library(readr)
library(dplyr)
library(stringr)

#' Read Jeopardy TSV Data [Deprecated]
#'
#' Reads a TSV file containing Jeopardy data into a tibble.
#' This function is deprecated. Please use the internal `jeopardy_data` object
#' directly (available after loading the package) or load it via `data(jeopardy_data)`.
#' Use this function only if you need to load a custom, external TSV file with the
#' same structure.
#'
#' @param filepath Path to the TSV file.
#'
#' @return A tibble containing the Jeopardy data.
#' @importFrom readr read_tsv
#' @export
#' @examples
#' # Access internal data instead:
#' # library(jeopardyNLP)
#' # head(jeopardy_data)
#'
#' # Or load explicitly:
#' # data(jeopardy_data)
#' # head(jeopardy_data)
#'
#' # Only use for external files:
#' # external_file <- system.file("extdata", "master_season1-35.tsv", package = "jeopardyNLP")
#' # if (file.exists(external_file)) {
#' #   external_data <- read_jeopardy_tsv(external_file)
#' # }
read_jeopardy_tsv <- function(filepath) {
    # Add lifecycle badge/message
    lifecycle::deprecate_warn(
        when = "0.2.0", # Replace with the version this deprecation happens
        what = "read_jeopardy_tsv()",
        details = "Please use the built-in `jeopardy_data` object instead."
    )

    # Check if file exists before attempting to read
    if (!file.exists(filepath)) {
        stop("File not found at: ", filepath)
    }

    # Read the TSV file
    # Need to handle potential column type specification issues if they arise
    # Use show_col_types = FALSE to suppress messages during automated checks
    df_raw <- readr::read_tsv(filepath, show_col_types = FALSE)

    # Clean column names using the helper function
    df_cleaned <- format_column_names(df_raw)
    return(df_cleaned)
}

#' Format Column Names
#'
#' Renames columns to a consistent format (e.g., snake_case to PascalCase or adding hyphens).
#'
#' @param df Input data frame (tibble).
#'
#' @return A tibble with renamed columns.
#' @importFrom dplyr filter select rename all_of
#' @keywords internal
format_column_names <- function(df) {
    original_names <- names(df)
    # Define the expected final, lowercase names
    required_final <- c("show_number", "air_date", "round", "category", "value", "question", "answer", "notes", "daily_double")

    # Check if all required final names are already present
    already_clean <- all(required_final %in% original_names)

    if (!already_clean) {
        message("Cleaning column names...") # Indicate cleaning is happening
        # Always convert all names to lowercase first
        new_names <- tolower(original_names)
        # Then replace spaces with underscores (handles "Show Number", "Air Date", etc.)
        new_names <- gsub(" ", "_", new_names)
        # Assign the cleaned names
        names(df) <- new_names

        # Re-check if all required final names are now present
        current_names <- names(df)
        missing_cols <- setdiff(required_final, current_names)
        if (length(missing_cols) > 0) {
            # Include original and attempted new names in error for debugging
            stop(paste(
                "Expected columns missing after cleaning.",
                "Missing:", paste(missing_cols, collapse = ", "),
                "Original Names:", paste(original_names, collapse = ", "),
                "Attempted New Names:", paste(new_names, collapse = ", ")
            ))
        }
    }
    # If already clean, just return df as is
    return(df)
}

#' Create Combined Question and Answer Column
#'
#' Concatenates the 'Question' and 'Answer' columns into a single
#' 'Question And Answer' column.
#'
#' @param df Input data frame (tibble) with 'Question' and 'Answer' columns.
#'
#' @return A tibble with the added 'Question And Answer' column.
#' @export
add_question_answer_col <- function(df) {
    # Equivalent to df['Question And Answer'] = df["Question"] + ' ' + df['Answer']
    df %>%
        dplyr::mutate(
            `Question And Answer` = stringr::str_c(.data$Question, .data$Answer, sep = " ")
        )
}

#' Add Clue Difficulty Column
#'
#' Adds a factor column 'clue_difficulty' (easy, average, hard) based on
#' the round and dollar value of the clue. Provides two slightly different
#' logic options.
#'
#' @param df Input data frame containing `round`, `value`, and `daily_double` columns.
#' @param viewer_assumptions Logical. If FALSE (default), uses standard logic.
#'   If TRUE, uses alternative logic based on viewer perception analysis cited
#'   in original project.
#'
#' @return The data frame with the added `clue_difficulty` factor column.
#' @importFrom dplyr mutate case_when select
#' @importFrom readr parse_number
#' @export
add_clue_difficulty <- function(df, viewer_assumptions = FALSE) {
    # Ensure required columns exist
    required_cols <- c("round", "value", "daily_double")
    if (!all(required_cols %in% names(df))) {
        stop(paste("Input dataframe must contain columns:", paste(required_cols, collapse = ", ")))
    }

    df_processed <- df %>%
        # Clean and convert value to numeric, handling NAs
        dplyr::mutate(
            value_num = readr::parse_number(.data$value, na = c("", "NA", "None")),
            # Ensure daily_double is logical or character "yes"/"no" for comparisons
            # Assuming it's already reasonably formatted from format_column_names
            # Convert potential NA in daily_double to "no" or handle explicitly?
            # Let's assume daily_double is character "yes"/"no" for now
            # daily_double = ifelse(is.na(daily_double), "no", daily_double) # Option
        )

    if (viewer_assumptions) {
        # Logic based on external analysis (viewer assumptions)
        # IMPORTANT: Internal data uses round == "1", "2", not "Jeopardy!", etc.
        df_processed <- df_processed %>%
            dplyr::mutate(clue_difficulty = dplyr::case_when(
                # Hard: DD R2, $1000+, $1600+ R2, Final Jeopardy
                (.data$daily_double == "yes" & .data$round == "2") ~ "hard", # Changed round check
                (!is.na(.data$value_num) & .data$value_num >= 1000 & .data$round %in% c("1", "2")) ~ "hard", # Changed round check
                .data$round == "3" ~ "hard", # Changed round check for Final
                # Average: DD R1, $800 R1/R2
                (.data$daily_double == "yes" & .data$round == "1") ~ "average", # Changed round check
                (.data$daily_double == "no" & !is.na(.data$value_num) & .data$value_num == 800) ~ "average", # R1 or R2
                (.data$daily_double == "no" & !is.na(.data$value_num) & .data$value_num == 1200 & .data$round == "2") ~ "average", # Changed round check
                # Easy: <=$600 R1/R2 (and not DD)
                (.data$daily_double == "no" & !is.na(.data$value_num) & .data$value_num <= 600) ~ "easy",
                TRUE ~ NA_character_ # Default if none match
            ))
    } else {
        # Standard logic (default)
        # IMPORTANT: Internal data uses round == "1", "2", not "Jeopardy!", etc.
        df_processed <- df_processed %>%
            dplyr::mutate(clue_difficulty = dplyr::case_when(
                # Easy: < $800 R1, < $1200 R2 (equiv < $600 normalized), not DD
                (.data$daily_double == "no" & !is.na(.data$value_num) &
                    ((.data$round == "1" & .data$value_num < 800) | # Changed round check
                        (.data$round == "2" & .data$value_num < 1200)) # Changed round check
                ) ~ "easy",
                # Average: $800 R1, $1000 R1, $1200 R2, DD R1
                (.data$daily_double == "no" & !is.na(.data$value_num) &
                    ((.data$round == "1" & .data$value_num %in% c(800, 1000)) | # Changed round check
                        (.data$round == "2" & .data$value_num == 1200)) # Changed round check
                ) ~ "average",
                (.data$daily_double == "yes" & .data$round == "1") ~ "average", # Changed round check
                # Hard: >= $1600 R2, DD R2, Final Jeopardy
                (.data$daily_double == "no" & !is.na(.data$value_num) & .data$round == "2" & .data$value_num >= 1600) ~ "hard", # Changed round check
                (.data$daily_double == "yes" & .data$round == "2") ~ "hard", # Changed round check
                .data$round == "3" ~ "hard", # Changed round check for Final
                TRUE ~ NA_character_
            ))
    }

    # Convert to factor and remove temp column
    df_final <- df_processed %>%
        dplyr::mutate(clue_difficulty = factor(.data$clue_difficulty, levels = c("easy", "average", "hard"))) %>%
        dplyr::select(-.data$value_num)

    return(df_final)
}

#' Create Processed Data Frame for Regular Episodes
#'
#' Filters the raw Jeopardy data for standard Jeopardy! and Double Jeopardy!
#' rounds, cleans column names, optionally adds a combined Question/Answer
#' column, and optionally adds a clue difficulty classification.
#'
#' @param raw_df The raw data frame (tibble) read from the source TSV.
#' @param add_q_and_a Logical, if TRUE adds the 'question_and_answer' column.
#' @param add_difficulty Logical, if TRUE adds the 'clue_difficulty' column.
#' @param viewer_assumptions Passed to `add_clue_difficulty` if `add_difficulty` is TRUE.
#' @param filter_notes Logical, if TRUE removes rows where the 'notes' column is not NA (default TRUE).
#'
#' @return A tibble containing the processed data for regular episodes.
#' @export
#' @examples
#' # Assuming jeopardy_raw is your loaded raw data:
#' # regular_data <- create_regular_episodes_df(jeopardy_raw)
#' # head(regular_data)
create_regular_episodes_df <- function(raw_df,
                                       add_q_and_a = TRUE,
                                       add_difficulty = TRUE,
                                       viewer_assumptions = FALSE,
                                       filter_notes = TRUE) {
    # Assume input df (like jeopardy_data) already has clean names
    # df <- format_column_names(raw_df) # REMOVED
    df <- raw_df # Use input directly

    # Filter for regular rounds (1 and 2) and non-special episodes (notes == "-")
    df_filtered <- df %>%
        # dplyr::filter(.data$round %in% c("Jeopardy!", "Double Jeopardy!")) # Original incorrect filter
        dplyr::filter(.data$round %in% c("1", "2")) # Correct filter for internal data

    # Filter for regular clues (assuming notes == "-" signifies regular)
    if (filter_notes) {
        df_filtered <- df_filtered %>% dplyr::filter(.data$notes == "-") # Corrected logic based on data structure
    }

    # Optional: Add combined Question and Answer column
    if (add_q_and_a) {
        # Ensure columns exist
        if (!all(c("question", "answer") %in% names(df_filtered))) {
            stop("Cannot add 'question_and_answer': 'question' or 'answer' column missing.")
        }
        df_filtered <- df_filtered %>%
            # Replace NA with empty string before uniting
            dplyr::mutate(
                question = tidyr::replace_na(.data$question, ""),
                answer = tidyr::replace_na(.data$answer, "")
            ) %>%
            tidyr::unite("question_and_answer", .data$question, .data$answer, sep = " ", remove = FALSE) # na.rm=TRUE not needed now
    }

    # Optional: Add Clue Difficulty column
    if (add_difficulty) {
        df_filtered <- add_clue_difficulty(df_filtered, viewer_assumptions)
    }

    # Remove rows where key columns might be NA AFTER processing
    # E.g., if Q&A is NA or difficulty couldn't be calculated
    # If question_and_answer is NA/empty string, it indicates issue upstream
    rows_before_final_filter <- nrow(df_filtered)
    if (add_q_and_a) df_filtered <- df_filtered %>% filter(!is.na(.data$question_and_answer), .data$question_and_answer != " ") # Check for space only too
    rows_after_final_filter <- nrow(df_filtered)
    message(paste("Rows before final Q&A filter:", rows_before_final_filter))
    message(paste("Rows after final Q&A filter:", rows_after_final_filter))
    # REMOVED: Don't filter based on NA difficulty, let downstream handle it
    # if (add_difficulty) df_filtered <- df_filtered %>% filter(!is.na(clue_difficulty))

    # Minimal set for modeling:
    # REMOVED show_number as it's not in internal jeopardy_data
    final_cols <- c("air_date", "round", "category", "value")
    if (add_q_and_a) final_cols <- c(final_cols, "question", "answer", "question_and_answer") else final_cols <- c(final_cols, "question", "answer")
    if (add_difficulty) final_cols <- c(final_cols, "clue_difficulty")

    # Ensure all selected columns exist before selecting
    final_cols <- intersect(final_cols, names(df_filtered))

    df_final <- df_filtered %>%
        dplyr::select(dplyr::all_of(final_cols))

    return(df_final)
}

#' Create Special Tournaments Dataframe
#'
#' Filters the raw Jeopardy data for special tournament episodes (notes != '-'),
#' drops unnecessary columns, formats names, and optionally adds combined Q&A
#' and difficulty columns.
#'
#' @param df Raw input data frame from `read_jeopardy_tsv`.
#' @param add_q_and_a Logical. If TRUE, adds the combined 'Question And Answer' column.
#' @param add_difficulty Logical. If TRUE, adds the 'Clue Difficulty' column
#'  (using default logic unless `viewer_assumptions` is specified).
#' @param ... Additional arguments passed to `add_clue_difficulty` (e.g., `viewer_assumptions`).
#'
#' @return A tibble containing processed special tournament data.
#' @export
create_special_tournaments_df <- function(df, add_q_and_a = TRUE, add_difficulty = FALSE, ...) {
    processed_df <- df %>%
        dplyr::filter(.data$notes != "-") %>%
        # Select columns explicitly to match python behavior if drop isn't exact
        dplyr::select(-.data$notes, -.data$comments) %>% # Adjust if specific columns needed
        format_column_names()

    if (add_q_and_a) {
        processed_df <- processed_df %>% add_question_answer_col()
    }
    if (add_difficulty) {
        processed_df <- processed_df %>% add_clue_difficulty(...)
    }
    return(processed_df)
}

# Main execution logic (Example usage, not run automatically like if __name__ == "__main__")
# To run this part, you would source the script and then call these lines:
#
# jeopardy_df_r <- read_tsv_r('../data/master_season1-35.tsv')
# regular_episodes_r <- make_regular_episodes_df_r(jeopardy_df_r)
# special_tournaments_r <- make_special_tournaments_df_r(jeopardy_df_r)
#
# # Save the resulting dataframes to CSV files in the R data directory
# write_csv(regular_episodes_r, "../data/regular_episodes.csv")
# write_csv(special_tournaments_r, "../data/special_tournaments.csv")
#
# print("Preprocessor functions defined. Call them manually or source the script.")
