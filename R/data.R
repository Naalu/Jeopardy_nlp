#' Jeopardy! Clues Dataset
#'
#' A dataset containing information about Jeopardy! clues from show number 4680
#' (aired 2004-12-31) onwards, scraped from J! Archive (j-archive.com).
#' This version is pre-processed slightly from the raw source available on Kaggle
#' (https://www.kaggle.com/datasets/tunguz/200000-jeopardy-questions).
#'
#' @format A tibble (data frame) with approximately 216,930 rows and 9 columns:
#' \describe{
#'   \item{show_number}{Unique identifier for the show (character).}
#'   \item{air_date}{Date the show aired (Date object).}
#'   \item{round}{The round the clue appeared in (e.g., "Jeopardy!", "Double Jeopardy!", "Final Jeopardy!") (character).}
#'   \item{category}{The category of the clue (character).}
#'   \item{value}{The dollar value of the clue (e.g., "$200") (character).}
#'   \item{question}{The text of the clue/question (character).}
#'   \item{answer}{The correct answer to the clue (character).}
#'   \item{notes}{Additional notes, often indicating special rounds like Final Jeopardy (character).}
#'   \item{daily_double}{Indicates if the clue was a Daily Double ("yes" or "no") (character).}
#' }
#' @source \url{https://www.kaggle.com/datasets/tunguz/200000-jeopardy-questions}, originally scraped from \url{http://j-archive.com/}
"jeopardy_data"

#' Jeopardy Questions and Answers (Regular Episodes)
#'
#' A dataset containing processed questions, answers, and metadata from
#' regular Jeopardy! episodes. Derived from the original `jeopardy_data`.
#'
#' @format A data frame with 273769 rows and 8 variables:
#' \describe{
#'   \item{air_date}{The date the episode originally aired (Date).}
#'   \item{round}{The round within the game (e.g., "Jeopardy!", "Double Jeopardy!") (character).}
#'   \item{category}{The category of the clue (character).}
#'   \item{value}{The dollar value of the clue (numeric).}
#'   \item{question}{The text of the clue (question) (character).}
#'   \item{answer}{The correct answer to the clue (character).}
#'   \item{question_and_answer}{Combined text of question and answer (character).}
#'   \item{clue_difficulty}{Calculated difficulty score based on value and round (numeric).}
#' }
#' @source Processed from the raw data included in the package.
"regular_episodes"
