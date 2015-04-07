#' Subset Data and return posterior summaries
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' add(1, 1)
#' add(10, 1)
sub_sims <- function(sims, params, program,...){
	# Transform to MCMC object
	c_sims <- codatransform(sims, program,...)
	# Subset based on parameters of interest
	return(c_sims[,params])
}


#' Generate Table from MCMC Output
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' add(1, 1)
#' add(10, 1)
latex_t <- function(sims, prob, method, bounds, params, program,...){
	print(xtable(intervals(sims, prob, method, bounds), comment=FALSE))
}

