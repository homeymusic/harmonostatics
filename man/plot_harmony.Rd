% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{plot_harmony}
\alias{plot_harmony}
\title{Plot Harmony}
\usage{
plot_harmony(
  x,
  home = NULL,
  columns,
  unlist = FALSE,
  include_names = TRUE,
  title = NULL
)
}
\arguments{
\item{x}{A list of notes or chords expressed as an interval integers or vector of interval integers}

\item{home}{The home pitch expressed an as interval integer}

\item{columns}{A vector of 2 column names to plot: semitone, affinity, brightness and magnitude}

\item{unlist}{A logical TRUE or FALSE to plot points individually or as one point}

\item{include_names}{Include the names from the list x on the plot}

\item{title}{An optional title for the plot}
}
\value{
Generates the requested scatter plot and returns TRUE
}
\description{
Provides scatter plots of musical harmony metrics of a note or chord.
}
