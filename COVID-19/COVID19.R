library(readr)

library(ggplot2)

library(dplyr)

confirmed_cases_worldwide <- read.csv("coronavirus.csv")

head(confirmed_cases_worldwide)

str(confirmed_cases_worldwide)

plot(confirmed_cases_worldwide$date,confirmed_cases_worldwide$cases)

