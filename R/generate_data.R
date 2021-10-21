# ------------------------------------------------------------------------------
# GENERATE UNDIAGNOSED DATA
# ------------------------------------------------------------------------------


#' Generate Cross Sectional Data
#'
#' Produces a model data frame for a cross-sectional study of n individuals.
#'
#' @param n sample size
#' @param prevalence disease prevalence. Can be a single number from 0 to 1 or a
#'   vector of two numbers.  If a vector, the first number will be the
#'   prevalence in the group with x = 0 (usually the classical group), the second will be the prevalence in
#'   the group with x = 1 (ususally the non-classical group).
#'
#' @return data frame of n individuals with baseline binary covariate x, disease
#'   status, and continuous disease severity
#' @export
generate_cross_sectional <- function(n = 10000, prevalence = 0.15){
  if(length(prevalence) == 1){
    prevalence <- c(prevalence, prevalence)
  }

  result <- data.frame(
    x = (rbinom(n, 1, 0.5) == 1)) %>%
    mutate(disease = ifelse(x == 0,
                            rbinom(n, 1, prevalence[1]),
                            rbinom(n, 1, prevalence[2]))) %>%
    dplyr::mutate(severity = disease * runif(n))
}


#' Generate Cross Sectional Data, in which negatives also have symptoms
#'
#' Produces a model data frame for a cross-sectional study of n individuals.
#' Allow non-diseased people to still have some symptoms, following a beta
#' distribution.
#'
#' @inheritParams generate_cross_sectional
#'
#' @return data frame of n individuals with baseline binary covariate x, disease
#'   status, and continuous disease severity
#' @export
generate_cs_beta <- function(n = 10000, prevalence = 0.1){

  if(length(prevalence) == 1){
    prevalence <- c(prevalence, prevalence)
  }

  result <- data.frame(
    x = (rbinom(n, 1, 0.5) == 1)) %>%
    mutate(disease0 = rbinom(n, 1, prevalence[1]),
           disease1 = rbinom(n, 1, prevalence[2])) %>%
    mutate(disease = ifelse(x == 0, disease0, disease1)) %>%
    select(-c(disease0, disease1)) %>%
    dplyr::mutate(severity = disease * rbeta(n, 5, 2.5) + (1 - disease) * rbeta(n, 1, 5) ) %>%
    dplyr::mutate(disease = as.logical(disease))
}


#' Generate Cross Sectional Data with variation in presentation between groups
#'
#' Produces a model data frame for a cross-sectional study of n individuals.
#' Allow non-diseased people to still have some symptoms, following a beta
#' distribution.  Individuals with covariate X = 1 have a different distribution
#' of symptoms than those with X = 0
#'
#' @inheritParams generate_cross_sectional
#'
#' @return data frame of n individuals with baseline binary covariate x, disease
#'   status, and continuous disease severity
#' @export
generate_cs_case4 <- function(n = 10000, prevalence = 0.1){

  if(length(prevalence) == 1){
    prevalence <- c(prevalence, prevalence)
  }

  result <- data.frame(
    x = (rbinom(n, 1, 0.5) == 1)) %>%
    dplyr::mutate(disease0 = rbinom(n, 1, prevalence[1]),
           disease1 = rbinom(n, 1, prevalence[2])) %>%
    dplyr::mutate(disease = ifelse(x == 0, disease0, disease1)) %>%
    dplyr::select(-c(disease0, disease1)) %>%
    dplyr::mutate(severity_control = rbeta(n, 1, 5),
                  severity_0 = rbeta(n, 5, 2.5),
                  severity_1 = rbeta(n, 5, 4)) %>%
    dplyr::mutate(severity = dplyr::case_when(disease == 0 ~ severity_control,
                                       disease == 1 & x == 0 ~ severity_0,
                                       disease == 1 & x == 1 ~ severity_1,
                                       TRUE ~ NA_real_) ) %>%
    dplyr::select(-c(severity_control, severity_0, severity_1)) %>%
    dplyr::mutate(disease = as.logical(disease))
}

#' Generate Cross Sectional Data with Doctors
#'
#' Produces a model data frame for a cross-sectional study of individuals, each
#' assigned to a specific doctor with diagnostic practices described by beta0,
#' betaS, and betaX
#'
#' @param patients_per_doc number of patients seen by each doctor
#' @param n_docs number of unique doctors in the data set
#' @param prevalence disease prevalence (for now, only supports one number for
#'   prevalence - i.e. groups must have the same disease prevalence)
#' @param theta_mean mean prescribing practices
#'
#' @return
#' @export
generate_cs_with_doctors <- function(patients_per_doc,
                                     n_docs,
                                     prevalence = 0.15,
                                     theta_mean = c(-10, 20, -5),
                                     sigma = 0.2){
  n <- patients_per_doc * n_docs

  doc_params <- tibble(doctor_id = 1:n_docs,
                       beta0 = theta_mean[1],
                       betaS = theta_mean[2],
                       betaX = rnorm(n_docs, theta_mean[3], sigma)) # rnorm(n_docs, theta_mean[3], sigma)

  generate_cross_sectional(n,
                           prevalence = prevalence) %>%
    mutate(doctor_id = rep(1:n_docs, each = patients_per_doc)) %>%
    left_join(doc_params, by = "doctor_id")
}


#' Generate Longitudinal Data Set
#'
#' Generate a long-form longitudinal data set of n individuals, each observed at
#' 100 time points.  Each individual has the underlying disease, with a disease
#' severity that increases over time based on a logistic function.  The logistic
#' function is parametrized by beta (rate of onset) and T_shift (time of onset),
#' which varies between individuals.
#'
#' @param n sample size
#'
#' @return a data.frame in long-form
#' @export
generate_longitudinal <- function(n = 10000){
  # TODO: this could probabily be made much faster with expand.grid(id, t =
  # 1:100) and then joining with beta, T, and disease
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
#' Helper function for generate_longitudinal.
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
