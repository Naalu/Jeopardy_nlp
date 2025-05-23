% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nmf_model.R
\name{evaluate_nmf_ranks}
\alias{evaluate_nmf_ranks}
\title{Evaluate NMF Ranks (Find Best k)}
\usage{
evaluate_nmf_ranks(
  term_doc_matrix,
  ranks = 7:15,
  seed = 43,
  method = "brunet",
  metric = get_nmf_residuals,
  n_runs = 1,
  ...
)
}
\arguments{
\item{term_doc_matrix}{Input term-document matrix.}

\item{ranks}{A numeric vector of ranks (k values) to evaluate.}

\item{seed}{Random seed for reproducibility.}

\item{method}{NMF algorithm method.}

\item{metric}{Function to extract the evaluation metric from the NMF result object.
Defaults to `get_nmf_residuals`.}

\item{n_runs}{Number of NMF runs per rank (if using methods that benefit from multiple runs).}

\item{...}{Additional arguments passed to `NMF::nmf`.}
}
\value{
A tibble with columns 'k' (rank) and 'metric' (the evaluated metric value).
}
\description{
Runs NMF for a range of ranks (k) and collects a specified metric
(defaulting to Frobenius residuals) to help determine an optimal number of topics.
}
