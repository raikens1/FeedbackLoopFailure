ContextualDiagnosisBandit <- R6::R6Class(
  inherit = Bandit,

  class = FALSE,
  public = list(
    weights = NULL,
    class_name = "ContextualDiagnosisBandit",
    initialize = function(prevalences, notest_reward) {
      self$d <- length(prevalences) # d features
      self$k <- 2 # k arms
      self$weights <- matrix(c(prevalences, rep(notest_reward, self$d)), nrow = self$d) # d x k weight matrix
    },
    get_context = function(t) {
      # generate d dimensional feature vector, one random feature active at a time
      Xa <- sample(c(1,rep(0,self$d-1)))
      context <- list(
        X = Xa,
        k = self$k,
        d = self$d
      )
    },
    get_reward = function(t, context, action) {
      # which arm was selected?
      arm <- action$choice
      # d dimensional feature vector for chosen arm
      Xa <- context$X
      # weights of active context
      weight <- Xa %*% self$weights
      # assign rewards for active context with weighted probs
      rewards <- as.double(c(weight[1] > runif(1), weight[2]))
      optimal_arm <- which_max_tied(weight)
      reward <- list(
        reward = rewards[arm],
        optimal_arm = optimal_arm,
        optimal_reward = weight[optimal_arm]
      )
    }
  )
)
