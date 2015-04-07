#' Posterior Intervals
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' add(1, 1)
#' add(10, 1)
intervals <- function(sims, ci, method, ...) {
	if(method=="hpd"){
		HPDinterval(sims, ci)
	}
	else if(method=="quantile"){
		bounds <- c((1-ci)/2, (.5-ci/2))
		t(apply(stan_coda,2,quantile,bounds[1], bounds[2]))
	}
}

###
# Mean, median, mode
# central <- function(sims, ci, method="hpd", bounds=NULL) {
# 	}
# }

###
# Prediction
# mape(coleman$Y, predict(fit), includeSE = TRUE)
