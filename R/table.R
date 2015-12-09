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
	return(as.mcmc(c_sims[,params]))
}


#' Generate Table from MCMC Output
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' add(1, 1)
#' add(10, 1)
latex_t <- function(sims, ci, ci_method="hpd", bounds, params, program,...){
  coda_sims <- sub_sims(sims, params, program,...)
	print(xtable(intervals(coda_sims, ci, ci_method, bounds), comment=FALSE))
}


#' Format coefs
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' add(1, 1)
#' add(10, 1)
gen_pretty <- function(input, digits=3){
  RoundOut <- formatC(input, format='f', digits=3)
  return(ifelse(RoundOut <1,str_replace_all(RoundOut, "0[[:punct:]]", "."), RoundOut))
}


#' Function to format posterior coefficient means for table
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' add(1, 1)
#' add(10, 1)
pretty_betas <- function(mc_sims){
  beta <- central(mc_sims)
  gen_pretty(beta)
}


#' Function for STAN post mean coefs and sig stars
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' add(1, 1)
#' add(10, 1)
MCMCstars <- function(mc_sims, new_order){
  pos001 <- HPDinterval(mc_sims[,new_order], .999)>0
  pos01 <- HPDinterval(mc_sims[,new_order], .99)>0
  pos05 <- HPDinterval(mc_sims[,new_order], .95)>0
  pos10 <- HPDinterval(mc_sims[,new_order], .9)>0
  sig_stars <- ifelse(pos10[,1]==pos10[,2],"^", "")
  sig_stars <- ifelse(pos05[,1]==pos05[,2],"*", sig_stars)
  sig_stars <- ifelse(pos01[,1]==pos01[,2],"**", sig_stars)
  sig_stars <- ifelse(pos001[,1]==pos001[,2],"***", sig_stars)
  stred_betas <-  as.matrix(paste(pretty_betas(mc_sims[,new_order]),sig_stars, sep=""))
  row.names(stred_betas) <- c(new_order)
  return(stred_betas)
}


#' Fcn to create grid to intersect posterior means and CIs
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' add(1, 1)
#' add(10, 1)
BlendCoefCI <- function(mc_sims, labels, new_order){
  PostMeans <- MCMCstars(mc_sims=mc_sims, new_order=new_order)
  PostCIs <- pretty_creds(mc_sims, new_order=new_order)
  # Empty grid to intersect posterior means and CIs
  TabGrid <- matrix(NA, nrow=nrow(PostMeans)*2)
  TabGrid[seq(1, nrow(TabGrid), 2),] <- PostMeans
  TabGrid[seq(0, nrow(TabGrid), 2),] <- PostCIs
  return(TabGrid)
}


#' Function to cbind models together
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' add(1, 1)
#' add(10, 1)
ModBind <- function(listMCsims, labels, listNewOrders){
  mods <- matrix(nrow=length(labels)*2, ncol=length(listMCsims))
  for(i in 1:length(listMCsims)){
    mods[,i] <- BlendCoefCI(listMCsims[[i]], labels, listNewOrders[[i]])
  }
  # Use labels for every other line in table
  FullLabs <- matrix(NA, nrow=length(labels)*2)
  FullLabs[seq(1, nrow(FullLabs), 2),] <- paste(labels)
  FullLabs[seq(0, nrow(FullLabs), 2),] <- paste("")
  rownames(mods) <- FullLabs
  return(mods)
}
