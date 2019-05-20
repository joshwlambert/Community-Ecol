#load the required packages
library(ggplot2)
library(dplyr)
library(tidyr)

#load the data
load("~/Groningen/Community-Ecol/Full_dataframe.RDATA")

lac_mu <- filter(tidyDAISIE, lambda_c, mu)
lac_mu <- cbind(lac_mu, DAISIETable['mu'])

#tidy the data
tidyDAISIE <- gather(DAISIETable)

#subset the data
lac_mu <- filter(tidyDAISIE, lambda_c, mu)



#plot the data

p <- ggplot(data = DAISIETable, aes(x = lambda_c, y = mu)) +
  geom_point()
plot(p)
