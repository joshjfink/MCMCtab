#' Posterior Intervals
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' add(1, 1)
#' add(10, 1)
intervals <- function(sims, alpha=.9, ci_method="hpd", digits=3,...) {
  ints <- if(ci_method=="hpd"){
    HPDinterval(sims, alpha)
  }
  else if(ci_method=="quantile"){
    bounds <- c((1-alpha)/2, (.5-alpha/2))
    t(apply(sims,2,quantile,bounds[1], bounds[2]))
  }
  return(ints)
}

#' Posterior Mean, Median, Mode
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' add(1, 1)
#' add(10, 1)
central <- function(sims, measure="mean", ...) {
  if(measure == "mean") t(apply(sims,2,mean))
}


####
# Effective Sample Size (effectiveSize is coda package)

###
# Prediction
# mape(coleman$Y, predict(fit), includeSE = TRUE)

# batchSE,
