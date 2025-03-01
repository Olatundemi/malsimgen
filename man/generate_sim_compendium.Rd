% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_sim_compendium.R
\name{generate_sim_compendium}
\alias{generate_sim_compendium}
\title{Function that generates multiple instances of simulated data}
\usage{
generate_sim_compendium(
  n_sims,
  volatility,
  init_EIR,
  duration,
  out_step,
  plot_instance = FALSE
)
}
\arguments{
\item{n_sims}{Number of simulated datasets to be created}

\item{volatility}{Volatility of the random walk}

\item{init_EIR}{Initial EIR value to kick of the random walk}

\item{duration}{The length of time in days the model will run.}

\item{out_step}{The number of days between each simulated data point.}

\item{plot_instance}{If TRUE, plots the simulated prevalence.}

\item{min_EIR}{Minimum value over which the random walk will not exceed.}

\item{max_EIR}{Maximum value over which the random walk will not exceed.}
}
\description{
\code{generate_sim_compendium} Generate multiple simulation iterations
}
