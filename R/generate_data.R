# ------------------------------------------------------------------------------
# GENERATE UNDIAGNOSED DATA
# ------------------------------------------------------------------------------


#' Generate Data
#'
#' @param n sample size
#' @param prevalence disease prevalence
#'
#' @return data frame of n individuals baseline binary covariate x and desease severity
#' @export
generate_cross_sectional <- function(n = 10000, prevalence = 0.15){
  result <- data.frame(
    x = (rbinom(n, 1, 0.5) == 1),
    disease = rbinom(n, 1, prevalence)
    ) %>%
    dplyr::mutate(severity = disease * runif(n))
}


#' Generate Longitudinal
#'
#' @param n sample size
#'
#' @return
#' @export
generate_longitudinal <- function(n = 10000){
  start <- data.frame(
    x = (rbinom(n, 1, 0.5) == 1),
    disease = 1,
    beta = rbeta(n, shape1 = 2, shape2 = 6),
    T_shift = rnorm(n, 50, 5),
    t = 0,
    id = 1:n
  )

  result <- start %>%
    rowwise() %>%
    do(extend_longitudinal_i(.)) %>%
    ungroup()

  return(result)
}

#' Extend Longitudinal Data from starting parameters
#'
#' Helper function for generate_longitudinal
#'
#' @param row_i row of starting parameters for a single simulated subject
#'
#' @return result_i, a data frame of years of time for row i
#' @export
extend_longitudinal_i <- function(row_i){
  result_i <- data.frame(id = row_i$id,
                         x = row_i$x,
                         disease = row_i$disease,
                         beta = row_i$beta,
                         T_shift = row_i$T_shift,
                         t = 0:100) %>%
    mutate(severity = severity_fn(t, beta, T_shift))

  return(result_i)
}


#' Severity Function
#'
#' Calculate severity over time for longitudinal data based on an individual's
#' starting parameters.  Helper function for extend_longitudinal.
#'
#' @param t time (years)
#' @param beta slope of disease progression
#' @param T_shift shift for time of progression onset
#'
#' @return severity between 0 and 1
#' @export
severity_fn <- function(t, beta = 0.5, T_shift = 50){
  severity <- 1 / (1 + exp(-beta * (t - T_shift)))
  return(severity)
}

