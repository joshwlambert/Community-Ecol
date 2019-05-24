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

#Transform the data
oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)

#One-way ANOVA
anova_clado <- aov(lambda_c ~ island)
anova(anova_clado)
#post-hoc p-value adjustment
TukeyHSD(anova_clado)

#One-way ANOVA
anova_mu <- aov(mu ~ island)
anova(anova_mu)
#post-hoc p-value adjustment
TukeyHSD(anova_mu)

#One-way ANOvA
anova_gamma <- aov(gamma ~ island)
anova(anova_gamma)
#post-hoc p-value adjustment
TukeyHSD(anova_gamma)

#One-way ANOvA
anova_ana <- aov(ana ~ island)
anova(anova_ana)
#post-hoc p-value adjustment
TukeyHSD(anova_ana)

#Transform the data for time = 10
oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 10) %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)

#One-way ANOvA for time = 10
anova_clado <- aov(lambda_c ~ island)
anova(anova_clado)
#post-hoc p-value adjustment
TukeyHSD(anova_clado)

#One-way ANOvA for time = 10
anova_mu <- aov(mu ~ island)
anova(anova_mu)
#post-hoc p-value adjustment
TukeyHSD(anova_mu)

#One-way ANOvA for time = 10
anova_gamma <- aov(gamma ~ island)
anova(anova_gamma)
#post-hoc p-value adjustment
TukeyHSD(anova_gamma)

#One-way ANOvA for time = 10
anova_ana <- aov(lambda_a ~ island)
anova(anova_ana)
#post-hoc p-value adjustment
TukeyHSD(anova_ana)


#Pairwise t-test
pairwise.t.test(x= lambda_c, g = island, data = oceanic_nonoceanic,  p.adj = "bonferroni")


oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(island == 'LL' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
anova1<-aov(lambda_c ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(island == 'LM' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
anova2<-aov(lambda_c ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(island == 'LH' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
anova3<-aov(lambda_c ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(island == 'ML' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
anova4<-aov(lambda_c ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(island == 'MM' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
anova5<-aov(lambda_c ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(island == 'MH' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
anova6<-aov(lambda_c ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(island == 'HL' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
anova7<-aov(lambda_c ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(island == 'HM' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
anova8<-aov(lambda_c ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(island == 'HH' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
anova9<-aov(lambda_c ~ island)
anova9<-anova(anova9)

p = c(anova1$`Pr(>F)`[1], anova2$`Pr(>F)`[1], anova3$`Pr(>F)`[1], anova4$`Pr(>F)`[1], anova5$`Pr(>F)`[1], anova6$`Pr(>F)`[1], anova7$`Pr(>F)`[1], anova8$`Pr(>F)`[1], anova9$`Pr(>F)`[1])
p.adjust(p, method = 'bonferroni', n = length(p))
