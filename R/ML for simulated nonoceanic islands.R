rm(list=ls())

devtools::load_all(".") DAISI
library(ggplot2)

setwd("~/studie biologie/jaar 2/Community Research")

#determine the parameter values for the nonoceanic island simulation
pars = c(1,2.5,20,0.01,2.5) #anagenis rate, extinction rate, carrying capacity, immigration, cladogenesis
time=4 #island age in million of years (possible later 10)
M=1000 #mainland species pool
nonoceanic= c(0.1,0.9)
#run the simulation
island_replicates=DAISIE_sim_nonoceanic(time=time,M=M,pars=pars,replicates=20, nonoceanic = nonoceanic, divdepmodel = "CS")

#save the simulation parameter values
save(pars,M, time, nonoceanic, file="simulation_parameters/pars_nonoceanic_sim_1.Rdata")


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
DAISIEMLSUM <- data.frame(lambda_c,mu,K,gamma,lambda_a,loglik,df,conv)

#create a dataframe containing the maximum likelihood parameters of each replicate
for (i in 1:length(island_replicates)){
  DAISIEML < DAISIE_ML_CS(datalist=island_replicates[[i]], initparsopt = pars, ddmodel=11, idparsopt = 1:5, parsfix = NULL, idparsfix = NULL)
  DAISIEMLSUM <- rbind(DAISIEMLSUM, DAISIEML)
}

#save the dataframe
save(DAISIEMLSUM,file="Maximum_Likelihood/ML_values_sim_nonoceanic_2.Rdata")

##creates violin plots with dots for a single predicted variable or the log likelihood

ggplot(DAISIEMLSUM,aes(x=0,y=loglik)) +geom_violin() + geom_dotplot(binaxis = "y", binwidth = 2, stackdir="center") 

ggplot(DAISIEMLSUM,aes(x=0,y=gamma)) +geom_violin() + geom_dotplot(binaxis = "y", binwidth = .0002, stackdir="center") +  geom_hline(yintercept=0.009)

ggplot(DAISIEMLSUM,aes(x=0,y=lambda_a)) +geom_violin() + geom_dotplot(binaxis = "y", binwidth = .5, stackdir="center") +scale_y_log10() +  geom_hline(yintercept=1.01)

ggplot(DAISIEMLSUM,aes(x=0,y=lambda_c)) +geom_violin() + geom_dotplot(binaxis = "y", binwidth = .1, stackdir="center") + scale_y_log10() +  geom_hline(yintercept=2.5)

ggplot(DAISIEMLSUM,aes(x=0,y=lambda_c)) +geom_violin() + geom_dotplot(binaxis = "y", binwidth = 1, stackdir="center") + ylim(0,35) +  geom_hline(yintercept=2.5)


ggplot(DAISIEMLSUM,aes(x=0,y=mu)) +geom_violin() + geom_dotplot(binaxis = "y", binwidth = .1, stackdir="center") + geom_hline(yintercept=2.6)

ggplot(DAISIEMLSUM,aes(x=0,y=K)) +geom_violin() + geom_dotplot(binaxis = "y", binwidth = .1, stackdir="center") + geom_hline(yintercept=20) + scale_y_log10()

ggplot(DAISIEMLSUM,aes(x=0,y=K)) +geom_violin() + geom_dotplot(binaxis = "y", binwidth = 1, stackdir="center") + geom_hline(yintercept=20) + ylim(0,60)
