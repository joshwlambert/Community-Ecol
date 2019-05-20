###########################
#Script for oceanic DAISIE
##########################

devtools::load_all(".")
library(tidyverse)
library(testit)

pars = c(2.5, 2.5, 20, 0.01, 1) #cladogenesis rate, extinction rate, carrying capacity, immigration, anagenesis

time=4 #island age in million of years (possibly time=10 if time is available).

M=1000 #mainland species pool

#run the simulation
island_replicates <- DAISIE_sim_oceanic(time=time,M=M,pars=pars,replicates=10, divdepmodel = "CS")

#save the simulation parameter values
setwd("C:/Users/Gebruiker/Documents/DAISIEproject/Oceanic_simulations")
save(pars, M, time, file ="pars_oceanic_sim_1.Rdata")

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
  DAISIEML <- DAISIE_ML_CS(datalist=island_replicates[[i]], initparsopt = pars, ddmodel=11, idparsopt = 1:5, parsfix = NULL, idparsfix = NULL)
  DAISIEMLSUM <- rbind(DAISIEMLSUM, DAISIEML)
}

#save the dataframe
save(DAISIEMLSUM,file="ML_values_sim_1_oceanic.Rdata")

##creates a violin plot with dots for a single predicted variable or the log likelihood(change the variable by chancing the y value)
ggplot(testdata,aes(x=0,y=loglik)) +geom_violin() + geom_dotplot(binaxis = "y", binwidth = 1, stackdir="center")
ggplot(testdata,aes(x=0,y=gamma)) +geom_violin() + geom_dotplot(binaxis = "y", binwidth = .001, stackdir="center")