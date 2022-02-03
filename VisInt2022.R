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

overfit = comb_data$pub_score[comb_data$pub_score > max(C1$AUC)]
length(overfit)/length(comb_data$pub_score)

# All the confidence intervals ############################################################

library(pROC) # to find the conf int of auc

# These numbers are from https://www.kaggle.com/c/siim-isic-melanoma-classification/data?select=train.csv 
n_val_test = 10982 # size of test and validation set
test_prop = 0.7 # 30/70 split
n = round(test_prop*n_val_test)
print(n)

malignant_rate = 584/33126 # in training set ( from https://arxiv.org/ftp/arxiv/papers/2008/2008.07360.pdf)
teams = 3308
AUC_1 = 0.9490 # best performing team

n_mal = round(n*malignant_rate) # estimated number of malignant cases
print(n_mal)
n_ben = n-n_mal # estimated number of benign cases

class = rep(0, n)
class[1:n_mal] = 1 # true class label

# Create toy example with AUC = 0.9490

predict1 = class
false1 = 0.322 # this parameter is adjusted until required AUC is produced
# malignant prediction
n_mal_05 = round(false1*n_mal)
n_mal_pred = seq(from = (1/n_mal_05), to = 1, by = (1/n_mal_05))
predict1[1:n_mal_05] = n_mal_pred

# benign prediction
n_ben_05 = round(false1*n_ben)
n_ben_pred = seq(from = (1/n_ben_05), to = 1, by = (1/n_ben_05))
predict1[(n_mal+1):(n_mal+n_ben_05)] = n_ben_pred

roc1 = roc(class,predict1)
plot(roc1)
auc(class,predict1)
# ci.auc(class, predict1) # deLong doesn't work that well
ci.auc(class, predict1, method = "bootstrap", boot.n=5000) 

# AUC = 0.9306 gives upper limit 95% CI: 0.9489
predict2 = class
false = false1+ 0.055

# malignant prediction
n_mal_05 = round(false*n_mal)
n_mal_pred = seq(from = (1/n_mal_05), to = 1, by = (1/n_mal_05))
predict2[1:n_mal_05] = n_mal_pred

# benign prediction
n_ben_05 = round(false*n_ben)
n_ben_pred = seq(from = (1/n_ben_05), to = 1, by = (1/n_ben_05))
predict2[(n_mal+1):(n_mal+n_ben_05)] = n_ben_pred

roc2 = roc(class,predict2)
plot(roc2)
auc2 = auc(class,predict2)
print(auc2)
ci.auc(class, predict2, method = "bootstrap", boot.n=5000) 

ci1low = comb_data$prv_score[comb_data$prv_score > auc2]
rank2 = length(ci1low)
print(rank2)

up_lim3 = comb_data$prv_score[round(rank2*0.025)]
print(rank2*0.025)
up_lim3

# AUC = 0.9282 gives upper limit 95% CI: 0.9455, rank=930, 99.5% CI: 9491
predict3 = class
false = false1+ 0.065

# malignant prediction
n_mal_05 = round(false*n_mal)
n_mal_pred = seq(from = (1/n_mal_05), to = 1, by = (1/n_mal_05))
predict3[1:n_mal_05] = n_mal_pred

# benign prediction
n_ben_05 = round(false*n_ben)
n_ben_pred = seq(from = (1/n_ben_05), to = 1, by = (1/n_ben_05))
predict3[(n_mal+1):(n_mal+n_ben_05)] = n_ben_pred

roc3 = roc(class,predict3)
plot(roc3)
auc3 = auc(class,predict3)
ci.auc(class, conf.level=0.95, predict3, method = "bootstrap", boot.n=5000) 

print(auc3)


