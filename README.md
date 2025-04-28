# jeopardyNLP: An R Package for Jeopardy! Clue Analysis

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

## Overview

`jeopardyNLP` provides functions and workflows implemented in R for preprocessing, exploring, and modeling text data from the popular quiz show Jeopardy!. It allows users to:

* Load pre-processed Jeopardy! clue data included with the package.
* Perform text cleaning tailored for clue text using custom stopwords.
* Conduct exploratory data analysis (EDA) through plots and word clouds.
* Apply unsupervised topic modeling (NMF) to discover latent themes.
* Build baseline supervised models to predict clue difficulty (demonstrated in vignettes).

This package is primarily based on the analysis workflow presented in the associated project reports and vignettes, translating previous Python work into an R context.

## Installation

You can install the development version of `jeopardyNLP` from GitHub using the `devtools` package:

```R
# install.packages("devtools") # If you don't have devtools installed
devtools::install_github("Naalu/Jeopardy_nlp")
```

## Data

This package includes a processed dataset named `jeopardy_data` containing clues from seasons 1-35. This dataset is loaded automatically when you load the package with `library(jeopardyNLP)`. This dataset was originally obtained from [Reddit user u/jwolle1](https://www.reddit.com/r/datasets/comments/cj3ipd/jeopardy_dataset_with_349000_clues/) and obtained via [Kaggle.com](https://www.kaggle.com/datasets/prondeau/350000-jeopardy-questions?select=master_season1-35.tsv).

```R
library(jeopardyNLP)
# The data is now available
head(jeopardy_data)
```

### Exported Data Objects

In addition to `jeopardy_data`, the package provides the `regular_episodes` data object. This is a pre-processed version of `jeopardy_data`, filtered for standard rounds and with additional columns like combined question/answer text and a difficulty score. You can load it explicitly using:

```R
data(regular_episodes, package = "jeopardyNLP")
head(regular_episodes)
```

See `?regular_episodes` for details on the columns.

### Other Included Files

The raw `.tsv` files (`master_season1-35.tsv`, `kids_teen.tsv`) and the custom `stopwords.txt` file used for processing are also available within the package's `inst/extdata` directory. You can access their paths using `system.file("extdata", "filename.tsv", package = "jeopardyNLP")` if needed for specific purposes, but standard usage should rely on the included `jeopardy_data` object.

Additionally, a pre-computed NMF model object (`jeopardy_nmf_model.rds`), generated from the topic modeling vignette (`vignettes/02-topic-modeling.Rmd`), is stored in `inst/extdata/`. This allows vignettes and potentially functions to load the model quickly without re-running the time-consuming NMF analysis. It can be loaded using:

```R
model_path <- system.file("extdata", "jeopardy_nmf_model.rds", package = "jeopardyNLP")
if (nzchar(model_path)) {
  nmf_model <- readRDS(model_path)
} else {
  # Handle case where model file is not found (e.g., during initial build)
  nmf_model <- NULL 
}
```

## Usage

Here's a basic workflow example:

```R
library(jeopardyNLP)
library(dplyr)
library(ggplot2)

# --- 1. Access Included Data ---
# The 'jeopardy_data' object is loaded with the package

# --- 2. Preprocess Data --- 
# Create the standard analysis dataframe: filters regular episodes, 
# adds combined Question+Answer, and adds clue difficulty
regular_episodes <- create_regular_episodes_df(jeopardy_data, 
                                               add_q_and_a = TRUE, 
                                               add_difficulty = TRUE)

# --- 3. Clean Text --- 
# Uses stopwords file included with the package by default
# Select the column to clean, e.g., the combined Q&A
cleaned_q_and_a <- clean_text_corpus(regular_episodes$question_and_answer)

# Add cleaned text back for potential use (optional)
regular_episodes$cleaned_text <- cleaned_q_and_a
# Filter out any rows that became empty after cleaning
regular_episodes <- regular_episodes %>% filter(cleaned_text != "")

# --- 4. Exploratory Data Analysis --- 

# Calculate top categories using the internal 'jeopardy_data'
# (No need to pass it explicitly as it's the default)
top_cats <- calculate_top_categories(n = 15)
print(top_cats)

# Plot top categories (displays plot)
# plot_top_categories(top_cats)

# Calculate word counts using the PREPROCESSED data
word_counts <- calculate_word_counts(regular_episodes)

# Plot question word count vs difficulty (displays plot)
# plot_word_counts_difficulty(word_counts, "Question Word Count")

# Plot word cloud for the 'category' column using internal 'jeopardy_data'
# (No need to pass df explicitly as it's the default)
# plot_word_cloud(col = "category", max_words = 100)

# --- 5. Topic Modeling (NMF) --- 

# Use the cleaned text from the processed 'regular_episodes' data
# Use a subset for demonstration if needed
set.seed(123)
sample_indices <- sample(1:nrow(regular_episodes), min(10000, nrow(regular_episodes))) # Sample up to 10k docs
sample_text <- regular_episodes$cleaned_text[sample_indices]

# Create TF-IDF Matrix (Terms x Docs)
tfidf_matrix <- create_tfidf_matrix(
  sample_text, 
  control = list(weighting = tm::weightTfIdf, bounds = list(global = c(5, Inf)))
)

# Evaluate ranks (e.g., k=7 to 10 for quick demo)
# k_eval <- evaluate_nmf_ranks(tfidf_matrix, ranks = 7:10, seed = 43)
# print(k_eval)
# plot_nmf_evaluation(k_eval)

# Run NMF with a chosen k (e.g., 10)
# nmf_model <- run_nmf_model(tfidf_matrix, k = 10, seed = 43)

# Get top words per topic
# top_words <- get_top_words_per_topic(nmf_model, n_top_words = 10)
# print(top_words)

# Plot word cloud for a topic (e.g., topic 1)
# plot_topic_word_cloud(nmf_model, topic_index = 1)

# --- 6. Vignettes --- 

For more detailed workflows, including supervised modeling, please see the package vignettes:

# Browse vignettes if package is installed
# browseVignettes("jeopardyNLP") 

# Or find them in the GitHub repository under the vignettes/ directory.
```

## Contributing

Please note that this project is primarily for educational/demonstration purposes. Contributions are welcome, but please open an issue first to discuss potential changes.

## License

MIT License (see LICENSE file).
