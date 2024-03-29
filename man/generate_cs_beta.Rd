% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_data.R
\name{generate_cs_beta}
\alias{generate_cs_beta}
\title{Generate Cross Sectional Data, in which negatives also have symptoms}
\usage{
generate_cs_beta(n = 10000, prevalence = 0.1)
}
\arguments{
\item{n}{sample size}

\item{prevalence}{disease prevalence. Can be a single number from 0 to 1 or a
vector of two numbers.  If a vector, the first number will be the
prevalence in the group with x = 0 (usually the classical group), the second will be the prevalence in
the group with x = 1 (ususally the non-classical group).}
}
\value{
data frame of n individuals with baseline binary covariate x, disease
  status, and continuous disease representativeness
}
\description{
Produces a model data frame for a cross-sectional study of n individuals.
Allow non-diseased people to still have some symptoms, following a beta
distribution.
}
