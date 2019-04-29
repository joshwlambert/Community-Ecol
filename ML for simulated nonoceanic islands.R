rm(list=ls())

library(DAISIE)

#load the functions that are needed for the simulation
setwd("~/studie biologie/jaar 2/Community Research/simulations")
load("nonoceanic_functions.Rdata")
#This includes the functions DAISIE_ONEcolonist, DAISIE_sim_core_nonoceanic, DAISIE_sim_nonoceanic and DAISIE_sim_update_state from the DAISIE repository

#determine the parameter values for the nonoceanic island simulation
pars = c(2.5,2.6,20,0.009,1.01) #anagenis rate, extinction rate, carrying capacity, immigration, cladogenesis
time=4 #island age in million of years
M=1000 #mainland species pool
nonoceanic= c(0.1,0.1)
#run the simulation
island_replicates=DAISIE_sim_nonoceanic(time=time,M=M,pars=pars,replicates=10, nonoceanic = nonoceanic, divdepmodel = "CS")

#save the simulation parameter values
save(pars,M, time, nonoceanic, file="pars_nonoceanic_sim_1.Rdata")


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
  DAISIEML <- DAISIE_ML_CS(datalist=island_replicates[[i]], initparsopt = pars, ddmodel=11, idparsopt = 1:5, parsfix = NULL, idparsfix = NULL)
  DAISIEMLSUM <- rbind(DAISIEMLSUM, DAISIEML)
}

#exporting the dataframe
write.table(DAISIEMLSUM, "maximum likelihood parameter values.csv", quote = F, row.names = T, sep = ",")

#save the dataframe
save(DAISIEMLSUM,file="ML_values_sim_1.Rdata")