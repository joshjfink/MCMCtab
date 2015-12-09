# Format coefs (Could replace with gsub)
gen_pretty <- function(input, digits=3, ...){
  RoundOut <- formatC(input, format='f', digits=digits)
  return(ifelse(RoundOut <1, str_replace_all(RoundOut, "0[[:punct:]]", "."), RoundOut))
}

# Function to format posterior coefficient means for table
pretty_betas <- function(mc_sims, ...){
  beta <- central(mc_sims)
  gen_pretty(beta)
}

# Beautify credible intervals
pretty_creds <- function(mc_sims, new_order, ...){
  cis <- gen_pretty(intervals(mc_sims[,new_order]))
  pretty_cis <- paste("(",  cis[,1],", ", cis[,2], ")", sep = "")
  return(matrix(pretty_cis))
}



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
