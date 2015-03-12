####
setwd("/Universe/GitHub/MCMCtab")
source("check_packages.R")
check_packages(c("rstan","rjags", "R2jags", "xtable", "foreign", "data.table", "parallel", "dplyr", "epicalc","gmodels", "foreign", "stringr", "knitr"))

detachAllData(); rm(list=ls())
### Load in data
load("SF_Environment_Moving.RData")
c.data <- cdata[,c("dspendlaw", "cntry", "foreignpct", "migpct", "police", "homicide", "inczscore", "age", "agesq", "female", "nevermar", "divorced", "widow", "kidshh", "rural", "suburb", "lesshs", "univ", "ptemp", "unemp", "nolabor", "selfemp", "pubemp", "protestant", "catholic", "otherrel", "relreligion")]
attach(c.data)
df1 <- c.data
x   <- data.frame(cbind(c.data[,3:ncol(c.data)]))
# x   <- data.frame(cbind(c.data[,3:24]))
M  <- length(unique(cntry))
K     <- dim(x)[2]
levels(cntry) <- seq(1,25,1)
cntry <- as.numeric(as.character(cntry))
polimm_dat  <- list(N= length(dspendlaw), M=M, K=K,
              y=dspendlaw, x=x,g=cntry)

# polimm_dat <- list(N=nrow(hosp),M=501,K=4,y=hosp[,1],x=hosp[,2:5],g=hosp[,6])

mod_code <- '
data {
    int N; // number of obs (pregnancies)
    int M; // number of groups (women)
    int K; // number of predictors
    
    int y[N]; // outcome
    row_vector[K] x[N]; // predictors
    int g[N];    // map obs to groups (pregnancies to women)
}
parameters {
    real alpha;
    real a[M]; 
    vector[K] beta;
    real sigma;  
}
model {
  alpha ~ normal(0,100);
  a ~ normal(0,sigma);
  beta ~ normal(0,100);
  for(n in 1:N) {
    y[n] ~ bernoulli(inv_logit( alpha + a[g[n]] + x[n]*beta));
  }
}'

# hfit <- stan(model_code=mod_code, model_name="hospitals", data=polimm_dat, iter=100, chains=2)

stk_mig06 <- stan(model_code=mod_code, model_name="Stock and MigPct w/Police and Homicide 2006", data=hosp_data, chains = 0)

CL = makeCluster(2, outfile = 'parallel.log')
clusterExport(cl = CL, c("polimm_dat", "stk_mig06")) 
sflist <- mclapply(1:2, mc.cores = 2, function(i)  stan(fit = stk_mig06, data=polimm_dat, chains=1, iter=1000,chain_id = i))


fit <- sflist2stanfit(sflist)

save(fit, file= "stk_mig_ph06.dat")

stopCluster(CL)
print(fit)
pdf("stk_mig_ph06_MCMC.pdf")
t <- rstan::traceplot(fit)
dev.off()

# hfit <- stan(model_code=mod_code, model_name="hospitals", data=polimm_dat, iter=200, chains=2)

# stk_mig06 <- stan(model_code=mod_code, data = mod_code, chains = 0)

# f1 <-stan(fit = stk_mig06, data=polimm_dat, chains=1, iter=100)



# sflist <- mclapply(1:2, mc.cores = 2, function(i)  stan(fit = stk_mig06, data=polimm_dat, chains=1, iter=1000,chain_id = i))

# (fit <- sflist2stanfit(sflist))


# hfit <- stan(model_code=mod_code, model_name="hospitals", data=polimm_dat, iter=1000, chains=1)


# # Test objects 
# l <- c1.labels; O <- o.dat
# F3 <- c10.fit; F4 <- c10.fit 
# F1 <- c10.fit; F2 <- c20.fit

# Q <- cbind(as.mcmc(F1)[,27:49], as.mcmc(F2)[,27:49], 
#            as.mcmc(F3)[,27:49], as.mcmc(F4)[,27:49])
# x.90<- function(M) {
#   t(matrix(unlist(HPDinterval(M, prob=.90)), ncol = 23, byrow = TRUE))
# }
# t90 <- lapply(Q, x.90)
# s.90 <-matrix(, nrow = 4, ncol = 23); ninety <-matrix(, nrow = 4, ncol = 23)
# for(i in 1:4){
#   t.90 <- t90[[i]]
#   stars=ifelse(t.90[,1] < 0 & t.90[,2] <0 | t.90[,1] > 0 & t.90[,2] >0, "*", "")
#   ninety[i, 1:23] <- paste("(", t.90[,1], ", ", t.90[,2], ")", sep = "")
#   s.90[i, 1:23] <- stars
# }
# x.95<- function(M) {
#   t(matrix(unlist(HPDinterval(M, prob=.95)), ncol = 23, byrow = TRUE))
# }
# t95 <- lapply(Q, x.95)
# s.95 <-matrix(, nrow = 4, ncol = 23); ninety <-matrix(, nrow = 4, ncol = 23)
# for(i in 1:4){
#   t.95 <- t95[[i]]
#   stars=ifelse(t.95[,1] < 0 & t.95[,2] <0 | t.95[,1] > 0 & t.95[,2] >0, "*", "")
#   ninety[i, 1:23] <- paste("(", t.95[,1], ", ", t.95[,2], ")", sep = "")
#   s.95[i, 1:23] <- stars
# }
# x.99<- function(M) {
#   t(matrix(unlist(HPDinterval(M, prob=.99)), ncol = 23, byrow = TRUE))
# }
# t99 <- lapply(Q, x.99)
# s.99 <-matrix(, nrow = 4, ncol = 23); ninety <-matrix(, nrow = 4, ncol = 23)
# for(i in 1:4){
#   t.99 <- t99[[i]]
#   stars=ifelse(t.99[,1] < 0 & t.99[,2] <0 | t.99[,1] > 0 & t.99[,2] >0, "*", "")
#   ninety[i, 1:23] <- paste("(", t.99[,1], ", ", t.99[,2], ")", sep = "")
#   s.99[i, 1:23] <- stars
# }
# P <- cbind(unlist(F1$BUGSoutput$mean$beta1), unlist(F2$BUGSoutput$mean$beta1), 
#            unlist(F3$BUGSoutput$mean$beta1), unlist(F4$BUGSoutput$mean$beta1))
# pretty<- function(F) {
#   sm <-  matrix(unlist(F), ncol = 23, byrow = TRUE)
#   sm <- formatC(sm, digits=3, format="f")
#   sub('^(-)?0[.]', '\\1.', sm)
# }
# means <- apply(P, 2, pretty)
# N <- cbind(unlist(t90[1]), unlist(t90[2]), 
#            unlist(t90[3]), unlist(t90[4]))
# pretty<- function(F) {
#   ct90 <- matrix(F, ncol=23, byrow = TRUE)
#   ct90 <- formatC(ct90, digits=3, format="f")
#   sub('^(-)?0[.]', '\\1.', ct90)
#   paste("(", ct90[1,], ", ", ct90[2,], ")", sep = "")
# }
# cis <- apply(N, 2, pretty)
# sc <- matrix(, nrow = 4, ncol = 4)
# CI <- matrix(, nrow = 4, ncol = 4)
# fsc <- matrix(, nrow = 8, ncol = 4)
# for(i in 1:4){
#   rsc <- paste(means[1,i], s.90[i,1], s.95[i,1], s.99[i,1], sep="")
#   mrsc <- as.matrix(rsc)
#   sc[i,i] <- mrsc
#   #add confidence interval bit here 
#   cic <- paste(cis[1,i])
#   mrci <- as.matrix(cic)
#   CI[i,i] <- mrci
# }

# #New method for staggering key IVs and cis for those IVs
# fsc[seq(1, nrow(fsc), 2),] <- sc
# fsc[seq(0, nrow(fsc), 2),] <- CI

# lat <- matrix(, nrow = 22, ncol = 4)
# for(i in 1:4){
#   rsc <- paste(means[1:22,i], s.90[i,1:22], s.95[i,1:22], s.99[i,1:22], sep="")
#   mrsc <- as.matrix(rsc)
#   lat[,i] <- mrsc
# }
# fb <- matrix(F, ncol=4, nrow=42, byrow = TRUE)
# fb[seq(1, nrow(fb), 2),] <- lat[2:22,]
# fb[seq(0, nrow(fb), 2),] <- cis[2:22,]
# dev <- cbind(F1$BUGSoutput$mean$deviance, F2$BUGSoutput$mean$deviance, 
#              F3$BUGSoutput$mean$deviance, F4$BUGSoutput$mean$deviance)
# rownames(dev) <- "Deviance"
# tfb <- rbind(fsc, fb)
# tfb
# rownames(tfb) <- l
# final <- rbind(tfb, dev)
# osc <- final[rownames(O),,drop=FALSE]
# SElab <- rep("", 25)
# osc
# rownames(osc[seq(0, nrow(osc), 2),] ) <- paste(SElab)

# library(xtable)

# print(xtable(data.frame(row = rownames(osc), (data.frame(osc))),
#       include.rownames = FALSE, align="lccccc", caption=t), caption.placement = 'top')
# print(xtable(osc, align="lcccc", caption=t), caption.placement = 'top', include.rownames = TRUE, )

# #GEtting VERY close--just need to find out how to stack two 
# #lines in one row or get rid of SE row names

