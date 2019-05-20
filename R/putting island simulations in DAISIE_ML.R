rm(list=ls())

library(DAISIE)

#create a simulation of islands with these parameters
pars = c(2.5,2.6,20,0.009,1.01)
island_replicates=DAISIE_sim(time=4,M=1000,pars=pars,replicates=10)


###maximum likelihood parameter values in a list

#create empty list DAISIEML
for (i in 1:length(island_replicates)) {
DAISIEML[[i]] <-list()
}

#put all maximum likelihood estimates for the replicates in a list
for (i in 1:length(island_replicates)) {
  DAISIEML[[i]] <- DAISIE_ML(datalist=island_replicates[[i]], initparsopt = pars, ddmodel=11, idparsopt = c(1,2,4,5), parsfix = NULL, idparsfix = NULL)
}



###maximum likelihood parameter values in a dataframe

#create empty dataframe DASIEMLSUM
lambda_c <- factor()
mu <- factor()
K <- factor()
gamma <- factor()
lambda_a <- factor()
loglik <- factor()
df <- factor()
conv <- factor()
DAISIEMLSUM <- data.frame(lambda_c,mu,K,gamma,lambda_a
                         ,loglik,df,conv)

#create a dataframe containing the maximum likelihood parameters of each replicate
for (i in 1:length(island_replicates)){
  DAISIEML <-DAISIE_ML(datalist=island_replicates[[i]], initparsopt = pars, ddmodel=11, idparsopt = 1:5, parsfix = NULL, idparsfix = NULL)
  DAISIEMLSUM <- rbind(DAISIEMLSUM, DAISIEML)
}

#exporting the dataframe
write.table(DAISIEMLSUM, "maximum likelihood parameter values.csv", quote = F, row.names = T, sep = ",")
