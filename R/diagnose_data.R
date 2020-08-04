# ------------------------------------------------------------------------------
# DIAGNOSE DATA
# ------------------------------------------------------------------------------

#' Diagnose Cross-sectional
#'
#' @param df cross-sectional dataset
#' @param useX boolean, specifies whether baseline covariate should be used in
#'   function determining diagnosis probability
#'
#' @return
#' @export
diagnose_cross_sectional <- function(df, useX = TRUE){
  n_row <- dim(df)[1]

  if(useX){
    df <- mutate(df, p_diagnose = diagnosis_fn(severity, x, theta = c(-10, 20, -5)))
  } else {
    df <- mutate(df, p_diagnose = diagnosis_fn(severity, x, theta = c(-10, 20, 0)))
  }

  result <- df %>%
    mutate(diagnosed = rbinom(n_row, size = 1, p = p_diagnose))

  return(result)
}



#' Diagnose Longitudinal
#'
#' @param df longitudinal dataset
#' @param useX boolean, specifies whether baseline covariate should be used in
#'   function determining diagnosis probability
#'
#' @return
#' @export
diagnose_longitudinal <- function(df, useX = TRUE){
  n_row <- dim(df)[1]

  if(useX){
    df <- mutate(df, p_diagnose = diagnosis_fn(severity, x, theta = c(-10, 20, -5)))
  } else {
    df <- mutate(df, p_diagnose = diagnosis_fn(severity, x, theta = c(-10, 20, 0)))
  }

  result <- df %>%
    mutate(diagnosed = 0) %>%
    mutate(newly_diagnosed = rbinom(n_row, size = 1, p = p_diagnose)) %>%
    group_by(id) %>%
    mutate(diagnosed = (cumsum(newly_diagnosed) > 0)) %>%
    ungroup()

  return(result)
}


#' Calculate Diagnosis Probability, depending on x
#'
#' @param x binary covariate
#' @param severity disease severity (in [0,1])
#' @param theta numeric vector giving parameters of diagnostic function (default \code{c(-10, 20, -5)}s)
#'
#' @return
#' @export
diagnosis_fn <- function(severity, x, theta = c(-10, 20, -5)){
  c <- 1/(1 + exp(-(theta[1] + theta[3]*x)))

  return(1/(1 + exp(-(theta[1] + theta[2] * severity + theta[3]*x))) - c)
}
