#' Convert STAN output to coda
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' add(1, 1)
#' add(10, 1)
stan2coda <- function(fit) {
     mcmc.list(lapply(1:ncol(fit), function(x) mcmc(as.array(fit)[,x,])))
}

#' Run transform to coda function based on programming language
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' add(1, 1)
#' add(10, 1)
codatransform <- function(sims=sims, program=NULL,...){
  if(program == "stan") return(stan2coda(sims, ...))
  # if(program == "jags") return(
  # if(program == "bugs") return(
  # if(program == "coda") return(
}

