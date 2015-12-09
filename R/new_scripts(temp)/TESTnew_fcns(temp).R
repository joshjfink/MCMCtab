# --- # --- # 
# Test/Impliment new MCMCtab fcns
# --- # --- # 
setwd("/Universe/GitHub/MCMCtab")

# Load in required packages
library(stargazer); library(coda); library(stringr)

# Load in psoterior summaries 
load("/Universe/Papers/JQC/ModResults/stan_posteriors")

# Labeling and order input for MCMCtab
	# Order
	b_order <- c("beta_b_disadvind", "beta_segreg", "beta_b_manuf","beta_b_divorced", "beta_log_pop", "beta_resmob","beta_hispindx",  "beta_b_incar","beta_b_drugs", "beta_police", "beta_year_dums1995", "beta_year_dums2005","Intercept")
	w_order <- c("beta_w_disadvind", "beta_segreg", "beta_w_manuf","beta_w_divorced", "beta_log_pop", "beta_resmob","beta_hispindx",  "beta_w_incar","beta_w_drugs", "beta_police", "beta_year_dums1995", "beta_year_dums2005","Intercept")
	# Labels for covariates 
	labels <- c("Economic Disadvantage Index", "Racial Segregation", "Industrial Restructuring", "Divorced Males", "Total Population","Residential Mobility", "Hispanic Immigration Index", "Incarceration Rate", "Drug Sales Arrest Rate", "Police Presence", "Year 1995", "Year 2005", "(Intercept)")

# Source working functions
source("R/new_scripts(temp)/new_script_fcns(temp).R")

MCMCtab(listMCsims=list(b_coda_perm, w_coda_perm), listNewOrders=list(b_order, w_order), vlabels=labels)

# /Universe/GitHub/MCMCtab/R/new_scripts\(temp\)/new_script_fcns\(temp\).R

mbout <- 
ModBind(listMCsims=list(b_coda_perm, w_coda_perm),  vlabels=labels, listNewOrders=list(b_order, w_order))
tmat <- cbind(Variable = rownames(mbout), mbout)
colnames(tmat) <- clabels; rownames(tmat)=NULL
	# Set column allignment and seperation (still need to automate the '36pt' resizing/give option for customizing)
	# col_sep <- "36pt}}l

library(devtools)
install_github("chrfrantz/R2LaTeX")

class(mbout)
printLatexTable(mbout)

m <- matrix(rexp(100), 10)
printLatexTable(m)