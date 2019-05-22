#load the required packages
library(ggplot2)
library(dplyr)
library(tidyr)

#Parameter differences for island_age = 4, mainland = 0.1, nonendemic = 0.9

Clado_hh = 0.205411
clado_lh =  0.6262914
clado_ll =  1.851296
clado_hl = 0.9706154

ext_hh = 0.002954286
ext_lh = 0.008827768
ext_ll = 0.04387181
ext_hl = 0.1591237

immig_hh = 9.986142
immig_lh = 3.455252
immig_ll = 13.12838
immig_hl = 30.25532

ana_hh = 0.8227131
ana_lh = 1.443557
ana_ll = 1.691687
ana_hl = 1.443557

clado = c(Clado_hh, clado_lh, clado_ll, clado_hl)
ext = c(ext_hh, ext_lh, ext_ll, ext_hl)
immig = c(immig_hh, immig_lh, immig_ll, immig_hl)
ana = c(ana_hh, ana_lh, ana_ll, ana_hl)
df <- data.frame(clado, ext, immig, ana)
tidydata <- gather(df)
mutate(tidydata, c())
names <- c('HH', 'LH', 'LL', 'HL')
tidydata <- cbind(tidydata, names)

p <- ggplot(data = tidydf, aes (x = tidydata[,1], y = tidydata[,2], group = )) +
  geom_col(aes(fill = names), position = 'fill', colour = 'black', fill = 'white') +
  geom_text(aes(label = tidydata[,3]), position = position_fill(vjust = 0.5))
plot(p)
