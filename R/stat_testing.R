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

#Barlett test for homogeneity of variances
bartlett.test(log(oceanic_four$lambda_c), log(nonoceanic_four_one_nine$lambda_c), data = temp_data)
bartlett.test(count ~ spray, data=InsectSprays)

#Paired t-test
test1 <- t.test(oceanic_four$lambda_c, nonoceanic_four_one_nine$lambda_c, paired=TRUE, alt="less")
test2 <- t.test(oceanic_four$lambda_c, nonoceanic_four_one_nine$lambda_c, paired=TRUE, alt="less")
test3 <- t.test(oceanic_four$lambda_c, nonoceanic_four_one_nine$lambda_c, paired=TRUE, alt="less")
test4 <- t.test(oceanic_four$lambda_c, nonoceanic_four_one_nine$lambda_c, paired=TRUE, alt="less")
tests <- #data.frame of p values
p.adjust(method = 'bonferroni')


#One-way ANOVA 
anova <- aov(oceanic_four$lambda_c, nonoceanic_four_one_nine$lambda_c)


fit1 = lm(formula = log(nonoceanic_four_one_nine[, 1]) ~ log(oceanic_four$lambda_c[, 1]))
fit2 = 
fit3 
anova(c(fits))
TukeyHSD(fit)

pairwise.t.test(x = oceanic_four$lambda_c, g = nonoceanic_four_one_nine$lambda_c, p.adjust.method = 'none')



