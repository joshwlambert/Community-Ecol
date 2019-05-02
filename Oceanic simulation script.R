rm(list=ls())

devtools::load_all(".")
library(tidyverse)
library(testit)
library(ggplot2)

#load all the functions from the repository
setwd("~/DAISIEproject")

## finds all .R files within a folder and sources them by Ahmadou Dicko
sourceEntireFolder <- function(folderName, verbose=FALSE, showWarnings=TRUE) { 
  files <- list.files(folderName, full.names=TRUE)
  
  # Grab only R files
  files <- files[ grepl("\\.[rR]$", files) ]
  
  if (!length(files) && showWarnings)
    warning("No R files in ", folderName)
  
  for (f in files) {
    if (verbose)
      cat("sourcing: ", f, "\n")
    ## TODO:  add caught whether error or not and return that
    try(source(f, local=FALSE, echo=FALSE), silent=!verbose)
  }
  return(invisible(NULL))
}
sourceEntireFolder(folderName = "R")

#determine the parameter values for the oceanic island simulation.
pars = c(1,2.5,20,0.01,2.5) #anagenis rate, extinction rate, carrying capacity, immigration, cladogenesis
# standard initial values, at least change each value once for a simulation and MLE.
time=4 #island age in million of years (possibly time=10 if time is available).
M=1000 #mainland species pool
#run the simulation
island_replicates=DAISIE_sim_oceanic(time=time,M=M,pars=pars,replicates=10, divdepmodel = "CS")

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
DAISIEMLSUM <- data.frame(lambda_c,mu,K,gamma,lambda_a
                          ,loglik,df,conv)

DAISIE_ML(datalist=island_replicates[[1]], initparsopt = pars, ddmodel = 11, idparsopt = 1:5, parsfix = NULL, idparsfix = NULL)

#create a dataframe containing the maximum likelihood parameters of each replicate
for (i in 1:length(island_replicates)){
  DAISIEML <- DAISIE::DAISIE_ML(datalist=island_replicates[[i]], initparsopt = pars, ddmodel=0, idparsopt = 1:5, parsfix = NULL, idparsfix = NULL)
  DAISIEMLSUM <- rbind(DAISIEMLSUM, DAISIEML)
}

#exporting the dataframe
write.table(DAISIEMLSUM, "maximum likelihood parameter values.csv", quote = F, row.names = T, sep = ",")

#save the dataframe
save(DAISIEMLSUM,file="ML_values_sim_1_oceanic.Rdata")

##creates a violin plot with dots for a single predicted variable or the log likelihood(change the variable by chancing the y value)

ggplot(testdata,aes(x=0,y=loglik)) +geom_violin() + geom_dotplot(binaxis = "y", binwidth = 1, stackdir="center")

ggplot(testdata,aes(x=0,y=gamma)) +geom_violin() + geom_dotplot(binaxis = "y", binwidth = .001, stackdir="center")