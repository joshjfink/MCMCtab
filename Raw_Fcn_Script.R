####
# http://stackoverflow.com/questions/15036754/r-package-xtable-how-to-create-a-latextable-with-multiple-rows-and-columns-from

# Test objects 
l <- c1.labels; O <- o.dat
F3 <- c10.fit; F4 <- c10.fit 
F1 <- c10.fit; F2 <- c20.fit

Q <- cbind(as.mcmc(F1)[,27:49], as.mcmc(F2)[,27:49], 
           as.mcmc(F3)[,27:49], as.mcmc(F4)[,27:49])
x.90<- function(M) {
  t(matrix(unlist(HPDinterval(M, prob=.90)), ncol = 23, byrow = TRUE))
}
t90 <- lapply(Q, x.90)
s.90 <-matrix(, nrow = 4, ncol = 23); ninety <-matrix(, nrow = 4, ncol = 23)
for(i in 1:4){
  t.90 <- t90[[i]]
  stars=ifelse(t.90[,1] < 0 & t.90[,2] <0 | t.90[,1] > 0 & t.90[,2] >0, "*", "")
  ninety[i, 1:23] <- paste("(", t.90[,1], ", ", t.90[,2], ")", sep = "")
  s.90[i, 1:23] <- stars
}
x.95<- function(M) {
  t(matrix(unlist(HPDinterval(M, prob=.95)), ncol = 23, byrow = TRUE))
}
t95 <- lapply(Q, x.95)
s.95 <-matrix(, nrow = 4, ncol = 23); ninety <-matrix(, nrow = 4, ncol = 23)
for(i in 1:4){
  t.95 <- t95[[i]]
  stars=ifelse(t.95[,1] < 0 & t.95[,2] <0 | t.95[,1] > 0 & t.95[,2] >0, "*", "")
  ninety[i, 1:23] <- paste("(", t.95[,1], ", ", t.95[,2], ")", sep = "")
  s.95[i, 1:23] <- stars
}
x.99<- function(M) {
  t(matrix(unlist(HPDinterval(M, prob=.99)), ncol = 23, byrow = TRUE))
}
t99 <- lapply(Q, x.99)
s.99 <-matrix(, nrow = 4, ncol = 23); ninety <-matrix(, nrow = 4, ncol = 23)
for(i in 1:4){
  t.99 <- t99[[i]]
  stars=ifelse(t.99[,1] < 0 & t.99[,2] <0 | t.99[,1] > 0 & t.99[,2] >0, "*", "")
  ninety[i, 1:23] <- paste("(", t.99[,1], ", ", t.99[,2], ")", sep = "")
  s.99[i, 1:23] <- stars
}
P <- cbind(unlist(F1$BUGSoutput$mean$beta1), unlist(F2$BUGSoutput$mean$beta1), 
           unlist(F3$BUGSoutput$mean$beta1), unlist(F4$BUGSoutput$mean$beta1))
pretty<- function(F) {
  sm <-  matrix(unlist(F), ncol = 23, byrow = TRUE)
  sm <- formatC(sm, digits=3, format="f")
  sub('^(-)?0[.]', '\\1.', sm)
}
means <- apply(P, 2, pretty)
N <- cbind(unlist(t90[1]), unlist(t90[2]), 
           unlist(t90[3]), unlist(t90[4]))
pretty<- function(F) {
  ct90 <- matrix(F, ncol=23, byrow = TRUE)
  ct90 <- formatC(ct90, digits=3, format="f")
  sub('^(-)?0[.]', '\\1.', ct90)
  paste("(", ct90[1,], ", ", ct90[2,], ")", sep = "")
}
cis <- apply(N, 2, pretty)
sc <- matrix(, nrow = 4, ncol = 4)
CI <- matrix(, nrow = 4, ncol = 4)
fsc <- matrix(, nrow = 8, ncol = 4)
for(i in 1:4){
  rsc <- paste(means[1,i], s.90[i,1], s.95[i,1], s.99[i,1], sep="")
  mrsc <- as.matrix(rsc)
  sc[i,i] <- mrsc
  #add confidence interval bit here 
  cic <- paste(cis[1,i])
  mrci <- as.matrix(cic)
  CI[i,i] <- mrci
}

#New method for staggering key IVs and cis for those IVs
fsc[seq(1, nrow(fsc), 2),] <- sc
fsc[seq(0, nrow(fsc), 2),] <- CI

lat <- matrix(, nrow = 22, ncol = 4)
for(i in 1:4){
  rsc <- paste(means[1:22,i], s.90[i,1:22], s.95[i,1:22], s.99[i,1:22], sep="")
  mrsc <- as.matrix(rsc)
  lat[,i] <- mrsc
}
fb <- matrix(F, ncol=4, nrow=42, byrow = TRUE)
fb[seq(1, nrow(fb), 2),] <- lat[2:22,]
fb[seq(0, nrow(fb), 2),] <- cis[2:22,]
dev <- cbind(F1$BUGSoutput$mean$deviance, F2$BUGSoutput$mean$deviance, 
             F3$BUGSoutput$mean$deviance, F4$BUGSoutput$mean$deviance)
rownames(dev) <- "Deviance"
tfb <- rbind(fsc, fb)
tfb
rownames(tfb) <- l
final <- rbind(tfb, dev)
osc <- final[rownames(O),,drop=FALSE]
SElab <- rep("", 25)
osc
rownames(osc[seq(0, nrow(osc), 2),] ) <- paste(SElab)

library(xtable)

print(xtable(data.frame(row = rownames(osc), (data.frame(osc))),
      include.rownames = FALSE, align="lccccc", caption=t), caption.placement = 'top')
print(xtable(osc, align="lcccc", caption=t), caption.placement = 'top', include.rownames = TRUE, )

#GEtting VERY close--just need to find out how to stack two 
#lines in one row or get rid of SE row names

