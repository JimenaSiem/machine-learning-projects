library(data.table)
library(ggplot2)
library(dplyr)
library(purrr)

setwd("/home/jimena/Desktop/machine-learning-projects/Logistic Regression/Hosmer Applied Logistic Regression/Chapter 1/data set/ICU")

dataBase <- fread("ICU.txt", header = TRUE)

ggplot(data = dataBase)+
geom_point(aes(x = AGE, y = STA))

intervals <- 15 +10 * 0:8

lower_bound <- head(intervals, -1)
upper_bound <- intervals[-1]-1
mean_clases <- rowMeans(cbind(lower_bound,upper_bound))

icu_data_summarice <- dataBase %>%
  mutate(age_intervals = cut(AGE, intervals, include.lowest = T, right = F)) %>%
  group_by(age_intervals) %>%
  summarise(stat_age = mean(STA)) %>%
  mutate(avg_intervals = mean_clases)

ggplot(data = icu_data_summarice, aes(x = avg_intervals, y = stat_age))+
  geom_line()+
  geom_point()

logistic_model <- glm(STA ~ AGE, data = dataBase, family = binomial)
