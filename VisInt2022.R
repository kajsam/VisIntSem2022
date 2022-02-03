# Visual Intelligense seminar
# by Kajsa Møllersen (kajsa.mollersen@uit.no) February 2022

library(dplyr)
library(magrittr)
library(rvest)
library(purrr)
library(readr)
library(ggplot2)

library(plotly)

# Andrew Mashchak (andrew.d.mashchak@uit.no) scraped the kaggle Melanoma 2020 
# challenge leaderboard web page for me: 
# https://www.kaggle.com/c/siim-isic-melanoma-classification/leaderboard

comb_data <- readRDS("SIIM-ISIC_Melanoma_kaggle_leadboard_data.RDS")

head(comb_data) # have a look

# Create violin plots

# all teams
C1 <- data.frame(AUC = comb_data$pub_score, dataset = "validation")
C2 <- data.frame(AUC = comb_data$prv_score, dataset = "test")

dat <- rbind(C1, C2)

p = ggplot(dat, aes(x = dataset, y = AUC)) +
  geom_violin(trim = T)

p + scale_x_discrete(limits=c("validation", "test"))

# best performing teams
C1 <- data.frame(AUC = comb_data$pub_score[comb_data$prv_score > 0.8], dataset = "validation")
C2 <- data.frame(AUC = comb_data$prv_score[comb_data$prv_score > 0.8], dataset = "test")

dat <- rbind(C1, C2)

p = ggplot(dat, aes(x = dataset, y = AUC)) +
  geom_violin(trim = T)

p + scale_x_discrete(limits=c("validation", "test"))


