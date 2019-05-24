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


#Paired t-test for lambda_c at time = 4

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(island == 'LL' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_1<-t.test(lambda_c ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(island == 'LM' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_2<-t.test(lambda_c ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(island == 'LH' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_3<-t.test(lambda_c ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(island == 'ML' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_4<-t.test(lambda_c ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(island == 'MM' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_5<-t.test(lambda_c ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(island == 'MH' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_6<-t.test(lambda_c ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(island == 'HL' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_7<-t.test(lambda_c ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(island == 'HM' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_8<-t.test(lambda_c ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(island == 'HH' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_9<-t.test(lambda_c ~ island)

p = c(t_test_1$p.value, t_test_2$p.value, t_test_3$p.value, t_test_4$p.value, t_test_5$p.value, t_test_6$p.value, t_test_7$p.value, t_test_8$p.value, t_test_9$p.value)
p.adjust(p, method = 'bonferroni', n = length(p))

#Paired t-test for lambda_c at time = 10

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 10) %>%
  dplyr::filter(island == 'LL' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_1<-t.test(lambda_c ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 10) %>%
  dplyr::filter(island == 'LM' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_2<-t.test(lambda_c ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 10) %>%
  dplyr::filter(island == 'LH' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_3<-t.test(lambda_c ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 10) %>%
  dplyr::filter(island == 'ML' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_4<-t.test(lambda_c ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 10) %>%
  dplyr::filter(island == 'MM' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_5<-t.test(lambda_c ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 10) %>%
  dplyr::filter(island == 'MH' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_6<-t.test(lambda_c ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 10) %>%
  dplyr::filter(island == 'HL' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_7<-t.test(lambda_c ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 10) %>%
  dplyr::filter(island == 'HM' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_8<-t.test(lambda_c ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 10) %>%
  dplyr::filter(island == 'HH' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_9<-t.test(lambda_c ~ island)

p = c(t_test_1$p.value, t_test_2$p.value, t_test_3$p.value, t_test_4$p.value, t_test_5$p.value, t_test_6$p.value, t_test_7$p.value, t_test_8$p.value, t_test_9$p.value)
p.adjust(p, method = 'bonferroni', n = length(p))

#Paired t-test for mu at time = 4

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(island == 'LL' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_1<-t.test(mu ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(island == 'LM' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_2<-t.test(mu ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(island == 'LH' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_3<-t.test(mu ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(island == 'ML' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_4<-t.test(mu ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(island == 'MM' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_5<-t.test(mu ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(island == 'MH' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_6<-t.test(mu ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(island == 'HL' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_7<-t.test(mu ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(island == 'HM' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_8<-t.test(mu ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(island == 'HH' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_9<-t.test(mu ~ island)

p = c(t_test_1$p.value, t_test_2$p.value, t_test_3$p.value, t_test_4$p.value, t_test_5$p.value, t_test_6$p.value, t_test_7$p.value, t_test_8$p.value, t_test_9$p.value)
p.adjust(p, method = 'bonferroni', n = length(p))

#Paired t-test for mu at time = 10

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 10) %>%
  dplyr::filter(island == 'LL' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_1<-t.test(mu ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 10) %>%
  dplyr::filter(island == 'LM' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_2<-t.test(mu ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 10) %>%
  dplyr::filter(island == 'LH' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_3<-t.test(mu ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 10) %>%
  dplyr::filter(island == 'ML' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_4<-t.test(mu ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 10) %>%
  dplyr::filter(island == 'MM' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_5<-t.test(mu ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 10) %>%
  dplyr::filter(island == 'MH' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_6<-t.test(mu ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 10) %>%
  dplyr::filter(island == 'HL' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_7<-t.test(mu ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 10) %>%
  dplyr::filter(island == 'HM' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_8<-t.test(mu ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 10) %>%
  dplyr::filter(island == 'HH' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_9<-t.test(mu ~ island)

p = c(t_test_1$p.value, t_test_2$p.value, t_test_3$p.value, t_test_4$p.value, t_test_5$p.value, t_test_6$p.value, t_test_7$p.value, t_test_8$p.value, t_test_9$p.value)
p.adjust(p, method = 'bonferroni', n = length(p))

#Paired t-test for gamma at time = 4

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(island == 'LL' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_1<-t.test(gamma ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(island == 'LM' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_2<-t.test(gamma ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(island == 'LH' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_3<-t.test(gamma ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(island == 'ML' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_4<-t.test(gamma ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(island == 'MM' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_5<-t.test(gamma ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(island == 'MH' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_6<-t.test(gamma ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(island == 'HL' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_7<-t.test(gamma ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(island == 'HM' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_8<-t.test(gamma ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(island == 'HH' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_9<-t.test(gamma ~ island)

p = c(t_test_1$p.value, t_test_2$p.value, t_test_3$p.value, t_test_4$p.value, t_test_5$p.value, t_test_6$p.value, t_test_7$p.value, t_test_8$p.value, t_test_9$p.value)
p.adjust(p, method = 'bonferroni', n = length(p))

#Paired t-test for gamma at time = 10

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 10) %>%
  dplyr::filter(island == 'LL' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_1<-t.test(gamma ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 10) %>%
  dplyr::filter(island == 'LM' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_2<-t.test(gamma ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 10) %>%
  dplyr::filter(island == 'LH' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_3<-t.test(gamma ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 10) %>%
  dplyr::filter(island == 'ML' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_4<-t.test(gamma ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 10) %>%
  dplyr::filter(island == 'MM' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_5<-t.test(gamma ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 10) %>%
  dplyr::filter(island == 'MH' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_6<-t.test(gamma ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 10) %>%
  dplyr::filter(island == 'HL' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_7<-t.test(gamma ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 10) %>%
  dplyr::filter(island == 'HM' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_8<-t.test(gamma ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 10) %>%
  dplyr::filter(island == 'HH' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_9<-t.test(gamma ~ island)

p = c(t_test_1$p.value, t_test_2$p.value, t_test_3$p.value, t_test_4$p.value, t_test_5$p.value, t_test_6$p.value, t_test_7$p.value, t_test_8$p.value, t_test_9$p.value)
p.adjust(p, method = 'bonferroni', n = length(p))

#Paired t-test for lambda_a at time = 4

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(island == 'LL' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_1<-t.test(lambda_a ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(island == 'LM' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_2<-t.test(lambda_a ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(island == 'LH' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_3<-t.test(lambda_a ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(island == 'ML' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_4<-t.test(lambda_a ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(island == 'MM' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_5<-t.test(lambda_a ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(island == 'MH' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_6<-t.test(lambda_a ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(island == 'HL' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_7<-t.test(lambda_a ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(island == 'HM' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_8<-t.test(lambda_a ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(island == 'HH' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_9<-t.test(lambda_a ~ island)

p = c(t_test_1$p.value, t_test_2$p.value, t_test_3$p.value, t_test_4$p.value, t_test_5$p.value, t_test_6$p.value, t_test_7$p.value, t_test_8$p.value, t_test_9$p.value)
p.adjust(p, method = 'bonferroni', n = length(p))


#Paired t-test for lambda_a at time = 10

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 10) %>%
  dplyr::filter(island == 'LL' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_1<-t.test(lambda_a ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 10) %>%
  dplyr::filter(island == 'LM' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_2<-t.test(lambda_a ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 10) %>%
  dplyr::filter(island == 'LH' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_3<-t.test(lambda_a ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 10) %>%
  dplyr::filter(island == 'ML' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_4<-t.test(lambda_a ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 10) %>%
  dplyr::filter(island == 'MM' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_5<-t.test(lambda_a ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 10) %>%
  dplyr::filter(island == 'MH' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_6<-t.test(lambda_a ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 10) %>%
  dplyr::filter(island == 'HL' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_7<-t.test(lambda_a ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 10) %>%
  dplyr::filter(island == 'HM' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_8<-t.test(lambda_a ~ island)

oceanic_nonoceanic <- final_data %>%
  dplyr::filter(time == 10) %>%
  dplyr::filter(island == 'HH' | island == 'O') %>%
  tidyr::drop_na()
attach(oceanic_nonoceanic)
t_test_9<-t.test(lambda_a ~ island)

p = c(t_test_1$p.value, t_test_2$p.value, t_test_3$p.value, t_test_4$p.value, t_test_5$p.value, t_test_6$p.value, t_test_7$p.value, t_test_8$p.value, t_test_9$p.value)
p.adjust(p, method = 'bonferroni', n = length(p))




