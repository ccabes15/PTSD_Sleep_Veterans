#Connor Cabrey
#Analysis for Research Paper

library(tidyverse)
library(lubridate)
library(ggplot2)
library(reshape2)
library(mosaic)
library(ggthemes)
library(DescTools)
library(broom)
library(readxl)

setwd("~/GOVT-650/Research Paper/Analysis Files")

user1 <- read_csv("../Data/DataPaper/user_1/questionnaire.csv")
user2 <- read_csv("../Data/DataPaper/user_2/questionnaire.csv")
user3 <- read_csv("../Data/DataPaper/user_3/questionnaire.csv")
user4 <- read_csv("../Data/DataPaper/user_4/questionnaire.csv")
user5 <- read_csv("../Data/DataPaper/user_5/questionnaire.csv")
user6 <- read_csv("../Data/DataPaper/user_6/questionnaire.csv")
user7 <- read_csv("../Data/DataPaper/user_7/questionnaire.csv")
user8 <- read_csv("../Data/DataPaper/user_8/questionnaire.csv")
user9 <- read_csv("../Data/DataPaper/user_9/questionnaire.csv")
user10 <- read_csv("../Data/DataPaper/user_10/questionnaire.csv")
user11 <- read_csv("../Data/DataPaper/user_11/questionnaire.csv")
user12 <- read_csv("../Data/DataPaper/user_12/questionnaire.csv")
user13 <- read_csv("../Data/DataPaper/user_13/questionnaire.csv")
user14 <- read_csv("../Data/DataPaper/user_14/questionnaire.csv")
user15 <- read_csv("../Data/DataPaper/user_15/questionnaire.csv")
user16 <- read_csv("../Data/DataPaper/user_16/questionnaire.csv")
user17 <- read_csv("../Data/DataPaper/user_17/questionnaire.csv")
user18 <- read_csv("../Data/DataPaper/user_18/questionnaire.csv")
user19 <- read_csv("../Data/DataPaper/user_19/questionnaire.csv")
user20 <- read_csv("../Data/DataPaper/user_20/questionnaire.csv")
user21 <- read_csv("../Data/DataPaper/user_21/questionnaire.csv")
user22 <- read_csv("../Data/DataPaper/user_22/questionnaire.csv")

info1 <- read_csv("../Data/DataPaper/user_1/user_info.csv")
info2 <- read_csv("../Data/DataPaper/user_2/user_info.csv")
info3 <- read_csv("../Data/DataPaper/user_3/user_info.csv")
info4 <- read_csv("../Data/DataPaper/user_4/user_info.csv")
info5 <- read_csv("../Data/DataPaper/user_5/user_info.csv")
info6 <- read_csv("../Data/DataPaper/user_6/user_info.csv")
info7 <- read_csv("../Data/DataPaper/user_7/user_info.csv")
info8 <- read_csv("../Data/DataPaper/user_8/user_info.csv")
info9 <- read_csv("../Data/DataPaper/user_9/user_info.csv")
info10 <- read_csv("../Data/DataPaper/user_10/user_info.csv")
info11 <- read_csv("../Data/DataPaper/user_11/user_info.csv")
info12 <- read_csv("../Data/DataPaper/user_12/user_info.csv")
info13 <- read_csv("../Data/DataPaper/user_13/user_info.csv")
info14 <- read_csv("../Data/DataPaper/user_14/user_info.csv")
info15 <- read_csv("../Data/DataPaper/user_15/user_info.csv")
info16 <- read_csv("../Data/DataPaper/user_16/user_info.csv")
info17 <- read_csv("../Data/DataPaper/user_17/user_info.csv")
info18 <- read_csv("../Data/DataPaper/user_18/user_info.csv")
info19 <- read_csv("../Data/DataPaper/user_19/user_info.csv")
info20 <- read_csv("../Data/DataPaper/user_20/user_info.csv")
info21 <- read_csv("../Data/DataPaper/user_21/user_info.csv")
info22 <- read_csv("../Data/DataPaper/user_22/user_info.csv")

user_info <- bind_rows(info1, info2, info3, info4, info5, info6, info7, info8,
                       info9, info10, info11, info12, info13, info14, info15,
                       info16, info17, info18, info19, info20, info21, info22)

user_sleep <- bind_rows(user1, user2, user3, user4, user5, user6, user7, user8,
                        user9, user10, user11, user12, user13, user14, user15,
                        user16, user17, user18, user19, user20, user21, user22)

sleep_info <- bind_cols(user_info, user_sleep)

ggplot(data = sleep_info, mapping = aes(sample = Pittsburgh)) + 
  stat_qq() +
  stat_qq_line() +
  geom_qq( color = "dark red", alpha = 0.3) +
  ggtitle("qq plot of PSQI") +
  theme_bw()

ggplot(data = sleep_info, mapping = aes(x = Pittsburgh))+
  geom_density()+
  xlab("PSQI Score")+
  ggtitle("Density Plot of PSQI Score")+
  theme_bw()

ggplot(data = sleep_info, mapping = aes(x = Pittsburgh))+
  geom_histogram(bins = 10)+
  xlab("PSQI Score")+
  ggtitle("Histogram Plot of PSQI Score")+
  theme_bw()

ggplot(data = sleep_info, mapping = aes(x = Age, y = Pittsburgh))+
  geom_point()+
  xlab("Age")+
  ylab("PSQI Score")+
  ggtitle("Age vs PSQI")+
  theme_bw()

ggplot(data = sleep_info, mapping = aes(x = Daily_stress, y = Pittsburgh))+
  geom_point()+
  xlab("Daily Stress Score")+
  ylab("PSQI Score")+
  ggtitle("Stress vs PSQI")+
  theme_bw()

cor(data = sleep_info, Pittsburgh ~ Daily_stress)
cor(data = sleep_info, Pittsburgh ~ Age)

favstats(sleep_info$Pittsburgh)
table(sleep_info$Pittsburgh)

# Initial exploratory data exploration shows a mean PSQI score of 5.3, and a
# median PSQI score of 5.  The distribution appears to uni-modal, around the mean
# and median scores, indicating a normal distribution of the random sample.
# I've included other simple graphs between Age and PSQI, as well as Daily Stress
# score and PSQI.  Initial thoughts are that they are not associated untransformed.
# Further exploration into transformations may show otherwise.  From the study on
# OIF/OEF veterans, the average PSQI score was 11.25.  This is drastically higher
# than the healthy sample explored here.





