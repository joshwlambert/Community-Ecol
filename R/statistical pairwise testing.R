rm(list = ls())
library(multcomp)

#filter for time=4
data_t4 <-dplyr::filter(final_data, time==4)

#Anova
m1 <- aov(lambda_a~island, data=final_data)

anova(m1)

layout(rbind(c(1,2))) 

plot(m1)

mc1 <- glht(m1,linfct=mcp(island="Tukey"))

summary(mc1)

pairwise.t.test(x=data_t4$lambda_a, g=data_t4$island,data=final_data,  p.adj = "none")
