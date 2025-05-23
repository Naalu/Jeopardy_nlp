% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nmf_model.R
\name{plot_topic_word_cloud}
\alias{plot_topic_word_cloud}
\title{Plot Topic Word Cloud}
\usage{
plot_topic_word_cloud(
  nmf_result,
  topic_index,
  n_top_words = 15,
  color_palette = "Dark2",
  save = FALSE,
  output_dir = NULL,
  filename = NULL
)
}
\arguments{
\item{nmf_result}{An object of class `NMF`.}

\item{topic_index}{The index (1-based) of the topic to visualize.}

\item{n_top_words}{The number of top words to include in the cloud.}

\item{color_palette}{Name of the RColorBrewer palette to use.}

\item{save}{Logical. If TRUE, saves the plot to a file.}

\item{output_dir}{Directory path to save the plot if `save = TRUE`.}

\item{filename}{Optional filename. Defaults to `_topic_X_wordcloud.png` (using 1-based X).}
}
\value{
Nothing. Displays or saves the word cloud plot.
}
\description{
Generates and displays/saves a word cloud for a specific NMF topic.
}
