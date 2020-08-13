# ------------------------------------------------------------------------------
# DIAGNOSE DATA
# ------------------------------------------------------------------------------

#' Diagnose Cross-sectional dataset
#'
#' Add diagnosis labels (0 or 1) to each individual in a cross-sectional dataset
#'
#' @param df cross-sectional dataset
#' @param theta, parameters for diagnosis function
#'
#' @return diagnosed cross_sectional data set.
#' @export
diagnose_cross_sectional <- function(df, theta = c(-10, 20, -5)){
  n_row <- dim(df)[1]

  result <- df %>%
    mutate(p_diagnose = diagnosis_fn(severity, x, theta)) %>%
    mutate(diagnosed = rbinom(n_row, size = 1, p = p_diagnose))

  return(result)
}



#' Diagnose Longitudinal Data Set
#'
#' At each time point for each individual, calculate probability of diagnosis
#' using diagnosis_fn.  If that individual is not yet diagnosed, check if a new
#' diagnosis would happen at this time point based on the diagnosis probability.
#'
#' @param df longitudinal dataset
#' @param theta parameters for diagnosis function
#'
#' @return diagnosed longitudinal data set.
#' @export
diagnose_longitudinal <- function(df, theta = c(-10, 20, -5)){
  n_row <- dim(df)[1]

  result <- df %>%
    mutate(diagnosed = 0,
           p_diagnose = diagnosis_fn(severity, x, theta)) %>%
    mutate(newly_diagnosed = rbinom(n_row, size = 1, p = p_diagnose)) %>%
    group_by(id) %>%
    mutate(diagnosed = (cumsum(newly_diagnosed) > 0)) %>%
    ungroup()

  return(result)
}

#' Calculate Diagnosis Probability
#'
#' @param x binary covariate
#' @param severity disease severity (in [0,1])
#' @param theta numeric vector giving parameters of diagnostic function
#'
#' @return diagnosis probability between 0 and 1.
#' @export
diagnosis_fn <- function(severity, x, theta = c(-10, 20, -5)){
  c <- 1/(1 + exp(-(theta[1] + theta[3]*x)))

  return(1/(1 + exp(-(theta[1] + theta[2] * severity + theta[3]*x))) - c)
}
