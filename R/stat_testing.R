library(multcomp)
library(dplyr)
library(ggplot2)

#Isolate the requried data
oceanic_four <- final_data %>%
  dplyr::filter(island_type == 'oceanic') %>%
  dplyr::filter(time == 4)

#plot the distribution of the data
ggplot(data = oceanic_four) +
  geom_histogram(aes(x = log(oceanic_four$lambda_c)))

ggplot(data = oceanic_four) +
  geom_histogram(aes(x = log(oceanic_four$mu)))

ggplot(data = oceanic_four) +
  geom_histogram(aes(x = log(oceanic_four$gamma)))

ggplot(data = oceanic_four) +
  geom_histogram(aes(x = log(oceanic_four$lambda_a)))

#plot the qq plot for the data
ggplot(data = oceanic_four, aes(sample = log(oceanic_four$lambda_c))) +
  stat_qq() +
  geom_qq_line()

ggplot(data = oceanic_four, aes(sample = log(oceanic_four$mu))) +
  stat_qq() +
  geom_qq_line()

ggplot(data = oceanic_four, aes(sample = log(oceanic_four$gamma))) +
  stat_qq() +
  geom_qq_line()

ggplot(data = oceanic_four, aes(sample = log(oceanic_four$lambda_a))) +
  stat_qq() +
  geom_qq_line()


#Shapiro-Wilks test for normality
shapiro.test(log(oceanic_four$lambda_c))
shapiro.test(log(oceanic_four$mu))
shapiro.test(log(oceanic_four$gamma))
shapiro.test(log(oceanic_four$lambda_a))


nonoceanic_four_one_nine <- final_data %>%
  dplyr::filter(island_type == 'nonoceanic') %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(prop_mainland == 0.1) %>%
  dplyr::filter(prop_non_endemic == 0.9)

#plot the distribution of the data
ggplot(data = nonoceanic_four_one_nine) +
  geom_histogram(aes(x = log(nonoceanic_four_one_nine$lambda_c)))

ggplot(data = nonoceanic_four_one_nine) +
  geom_histogram(aes(x = log(nonoceanic_four_one_nine$mu)))

ggplot(data = nonoceanic_four_one_nine) +
  geom_histogram(aes(x = log(nonoceanic_four_one_nine$gamma)))

ggplot(data = nonoceanic_four_one_nine) +
  geom_histogram(aes(x = log(nonoceanic_four_one_nine$lambda_a)))

#plot the qq plot for the data
ggplot(data = nonoceanic_four_one_nine, aes(sample = log(nonoceanic_four_one_nine$lambda_c))) +
  stat_qq() +
  geom_qq_line()

ggplot(data = nonoceanic_four_one_nine, aes(sample = log(nonoceanic_four_one_nine$mu))) +
  stat_qq() +
  geom_qq_line()

ggplot(data = nonoceanic_four_one_nine, aes(sample = log(nonoceanic_four_one_nine$gamma))) +
  stat_qq() +
  geom_qq_line()

ggplot(data = nonoceanic_four_one_nine, aes(sample = log(nonoceanic_four_one_nine$lambda_a))) +
  stat_qq() +
  geom_qq_line()

#Shapiro-Wilks test for normality
shapiro.test(log(nonoceanic_four_one_nine$lambda_c))
shapiro.test(log(nonoceanic_four_one_nine$mu))
shapiro.test(log(nonoceanic_four_one_nine$gamma))
shapiro.test(log(nonoceanic_four_one_nine$lambda_a))

##########################
FINISH
##########################
#Barlett test for homogeneity of variances
bartlett.test(log(oceanic_four$lambda_c), log(nonoceanic_four_one_nine$lambda_c), data = )

#Paired t-test
test1 <- t.test(oceanic_four$lambda_c, nonoceanic_four_one_nine$lambda_c, paired=TRUE, alt="less")
test2 <- t.test(oceanic_four$lambda_c, nonoceanic_four_one_nine$lambda_c, paired=TRUE, alt="less")
test3 <- t.test(oceanic_four$lambda_c, nonoceanic_four_one_nine$lambda_c, paired=TRUE, alt="less")
test4 <- t.test(oceanic_four$lambda_c, nonoceanic_four_one_nine$lambda_c, paired=TRUE, alt="less")
tests <- #data.frame of p values
p.adjust(method = 'bonferroni')


#Transform the data
oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) 
attach(oceanic_nonoceanic)
#dependent variable island type
#independent variable parameter estimate

#One-way ANOVA
anova1 <- aov(lambda_c ~ island)
#post-hoc p-value adjustment
TukeyHSD(anova1)

#Pairwise t-test
pairwise.t.test(x= lambda_c, g = island, data = oceanic_nonoceanic,  p.adj = "bonferroni")

