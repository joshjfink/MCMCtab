# #' Merge Multiple MCMC Chains
# #'
# #' @param x A number.
# #' @param y A number.
# #' @return The sum of \code{x} and \code{y}.
# #' @examples
# #' add(1, 1)
# #' add(10, 1)
# combine_mcmc <- function(sims=sims, program=NULL,...){
#   if(program == "stan") return(stan2coda(sims, ...))