---
title: "Data analysis in R- Bellabeat"
author: "Yun-Chin(Verna) Lee"
date: "2023-02-25"
output: html_document
---

```{r setup, include=FALSE}
options(repos = list(CRAN="http://cran.rstudio.com/"))
knitr::opts_chunk$set(echo = TRUE)
```

## Introduction of the Project

This is project is to help Bellabeat, a high-tech manufacturer of health-focused products for women, to gain insight into how consumers are using their smart devices. *The insights will then help guide marketing strategy for the company.*

## Project Goal

Find out **the trends in smart device usage** and **how these trends could apply to Bellabeat customers**. Furthermore, point out **how these trends could help influence Bellabeat marketing strategy**.

## Source of Data Set

[FitBit Fitness Tracker Data](https://www.kaggle.com/datasets/arashnic/fitbit) (CC0: Public Domain, dataset made available through [Mobius](https://www.kaggle.com/arashnic)): This Kaggle data set
contains personal fitness tracker from thirty fitbit users. Thirty eligible Fitbit users consented to the submission of personal tracker data, including minute-level output for physical activity, heart rate, and sleep monitoring. It includes information about daily activity, steps, and heart rate that can be used to explore users’ habits.

## Loading Packages

First thing first, we need to load the packages which will be used throughout the project.

```{r}
install.packages("tidyverse", repos = "http://cran.us.r-project.org")
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(purrr)
```

## Importing Data

After loading the packages, let's import the datasets. Since we are given 18 data sets, I decided to check all of them in Excel beforehand to better understand which data sets to use. Following are my chosen data sets and the reasons why I chose them.

*dailyActivity_merged
This data set contains data of dailyCalories_merged, dailyIntensities_merged and dailySteps_merged. It should be useful for us to find out the user trend.

*hourlyIntensities_merged and hourlySteps_merged
Both hourlyIntensities and hourlySteps can help us to know when users tend to engage in more physically required activities. Also, I'd like to find the correlation between these two data sets.

*hourlyCalories_merged
HourlyCalories can help us to know the users' calorie-burning routine better. In addition, we can find out how highly it is correlated to hourlyIntensities and hourlySteps. By knowing the correlation, we can better recommend the activities to participate in if our user wants to burn more calories.

*sleepDay_merged
SleepDay_merged is useful to help us to understand users' sleeping habit.

```{r}
setwd("/Users/leeverna/Downloads/Fitabase Data 4.12.16-5.12.16/To use")
daily_activity <- read_csv('dailyActivity_merged.csv')
hourly_intensities <- read_csv('hourlyIntensities_merged.csv')
hourly_steps <- read_csv('hourlySteps_merged.csv')
hourly_calories <- read_csv('hourlyCalories_merged.csv')
sleep <- read_csv('sleepDay_merged.csv')
```

The data set weightLogInfo_merged seems useful for defining the target audiences. However, after checking it with pivot table in Excel, I realized it only contains 8 users' data, which is very far from the metadata provided from the data source that claims the data includes information of 30 users. For the sake of data integrity and the accuracy of our analysis, we won't use the data set weightLogInfo_merged.

## Data Exploration

Firstly, let's check the head and data type of our data sets to see if any cleaning needed in the next step.

```{r}
# Daily activity
head(daily_activity)
# Hourly intensities
head(hourly_intensities)
# Hourly steps
head(hourly_steps)
# Hourly calories
head(hourly_calories)
# Sleep
head(sleep)
```      

I noticed ActivityDate and ActivityHour in all of our data sets are string values instead of time. This should be corrected later on.

Secondly, let's check if there's any duplicate. We will need to remove them if any. 

```{r}
sum(duplicated(daily_activity))
sum(duplicated(hourly_intensities))
sum(duplicated(hourly_steps))
sum(duplicated(hourly_calories))
sum(duplicated(sleep))
```
3 duplicates are found in sleep dataframe. Let's remove them before proceeding.

```{r}
sleep_d <- distinct(sleep)
sum(duplicated(sleep_d))
```

Now our data sets are looking good to proceed! Let's have a look at the statistic summery of our data sets to get a better idea of our users.

```{r}
# Daily activity
daily_activity %>% 
  select(TotalSteps,
         TotalDistance,
         VeryActiveDistance,
         VeryActiveMinutes,
         LightActiveDistance,
         LightlyActiveMinutes,
         SedentaryMinutes,
         Calories) %>%
  summary()
# Hourly intensities
hourly_intensities %>% 
  select(TotalIntensity) %>% 
  summary()
# Hourly steps
hourly_steps %>% 
  select(StepTotal) %>% 
  summary()
# Hourly calories
hourly_calories %>% 
  select(Calories) %>% 
  summary()
# Sleep
sleep_d %>% 
  select(TotalMinutesAsleep,
         TotalTimeInBed) %>% 
  summary()
```

As per the descriptive statistics, we are able to get general information of the users as below:

* Have a sedentary lifestyle with an average of 991.2 mins (16.52hrs) sitting or lying down per day
* The average daily calorie consumption is 2,304. Presuming the users in the data set are women, 
* The average step taken per day is 7,638 which is fairly beneficial to users' health according to [this study](https://jamanetwork.com/journals/jamanetworkopen/fullarticle/2783711?utm_source=For_The_Media&utm_medium=referral&utm_campaign=ftm_links&utm_term=090321).
* Sleep averagely 419.5 mins (around 7hrs) a day. This falls in [CDC recommended hours of sleep per day](https://www.cdc.gov/sleep/about_sleep/how_much_sleep.html).
* Based on total time in bed and total minutes asleep, we can come out with the average time for users falling asleep which is around 40 mins. This indicates the users' sleep health may be under pressure according to [the sleep charity](https://thesleepcharity.org.uk/how-long-should-it-take-to-fall-sleep/#:~:text=Longer%20than%2015%20minutes%20and,you're%20in%20sleep%20debt.). We will dig deeper to find out how we can help our users with it.

## Cleaning Data

During data exploration, we found that time data is wrongly typed as string. Thus, we need to correct them.

```{r}
daily_activity$ActivityDate <- as.POSIXct(daily_activity$ActivityDate, format="%m/%d/%Y", tz=Sys.timezone())
hourly_intensities$ActivityHour <- as.POSIXct(hourly_intensities$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
hourly_steps$ActivityHour <- as.POSIXct(hourly_steps$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
hourly_calories$ActivityHour <- as.POSIXct(hourly_calories$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
sleep_d$SleepDay <- as.POSIXct(sleep_d$SleepDay, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
```

## Finding out the user's daily routine

By matching user ID and activity hour, we can plot to see what the daily physical activity schedule looks like for users.

```{r}
# Merging hourly_intensities, hourly_steps and hourly_calories by user ID and activity hour
hourly_list <- list(hourly_intensities, hourly_steps, hourly_calories)
hourly_data  <- hourly_list %>% reduce(inner_join, by=c('Id','ActivityHour'))
# Separate the date-time column and removed the combined one
hourly_data$date <- as.Date(hourly_data$ActivityHour)
hourly_data$time <- format(as.POSIXct(hourly_data$ActivityHour), format = '%H')
hourly_data_a <- hourly_data %>% select(-c(ActivityHour)) 
hourly_data_f <- select(hourly_data, Id, date, time, TotalIntensity, StepTotal, Calories)
# Get hourly average of TotalIntensity, StepTotal and Calories
hourly_data_tr <- hourly_data_f %>% group_by(time) %>% summarize(
  agg_intensity = mean(TotalIntensity),
  agg_step = mean(StepTotal),
  agg_calorie = mean(Calories))
# Plotting dataframe
hourly_data_tr %>%
  pivot_longer(cols = 4:2,
               names_to = 'variable',
               values_to = 'value') %>% 
  ggplot(aes(x=time, y=value, group=variable)) +
  geom_line(aes(color=variable))
```

According to the line graph, we can see that our users are active from 6am to 11pm. Plus, steps peak at 6pm. Other than this, we can notice that intensive activity seems to have a strong positive correlation to calorie consumption. Let's plot their correlation to find out their correlation.

```{r}
# Correlation between calorie and intensity
r_intensity <- round(cor(hourly_data_tr$agg_intensity, hourly_data_tr$agg_calorie), 3)
p_intensity <- cor.test(hourly_data_tr$agg_intensity, hourly_data_tr$agg_calorie)$p.value
ggplot(hourly_data_tr, aes(y=agg_intensity, x=agg_calorie)) + 
  geom_point() + 
  geom_smooth(method="lm", col="skyblue") +
  annotate("text", x=90, y=18, label=paste0("r = ", r_intensity), hjust=0) +
  annotate("text", x=90, y=16, label=paste0("p = ", round(p_intensity, 3)), hjust=0)
# Correlation between calorie and step
r_step <- round(cor(hourly_data_tr$agg_step, hourly_data_tr$agg_calorie), 3)
p_step <- cor.test(hourly_data_tr$agg_step, hourly_data_tr$agg_calorie)$p.value
ggplot(hourly_data_tr, aes(y=agg_step, x=agg_calorie)) + 
  geom_point() + 
  geom_smooth(method="lm", col="skyblue") +
  annotate("text", x=80, y=450, label=paste0("r = ", r_step), hjust=0) +
  annotate("text", x=80, y=410, label=paste0("p = ", round(p_step, 3)), hjust=0)
```

Although the correlation between steps taken and calorie consumption is less than it is with intensive activity, with correlation coefficient of 0.991, we can conclude that the two are indeed positively highly correlated. **These two insights can help our users better choosing their activity based on their needs, e.g. if one is seeking for losing weight, step counting in the app might be helpful but setting a daily alarm for reminding the user to do workout is going to give a greater aid.**

## Sleep analysis

Earlier, we found that our users seem to have trouble falling asleep. Let's find out if their sleep has correlation to their physical activity.

```{r}
# Merging daily_activity and sleep_d
colnames(sleep_d)[2] <- "Date"
colnames(daily_activity)[2] <- "Date"
sleep_activity_list <- list(sleep_d, daily_activity)
sleep_activity <- sleep_activity_list %>% reduce(inner_join, by=c('Id','Date'))
# Get average for each user's daily data
sleep_activity_tr <- sleep_activity %>% group_by(Id) %>% summarize(
  agg_sleep = mean(TotalMinutesAsleep),
  agg_inbed = mean(TotalTimeInBed),
  agg_fallasleep = (agg_inbed - agg_sleep),
  agg_steps = mean(TotalSteps),
  agg_veryactivemins = mean(VeryActiveMinutes),
  agg_calories = mean(Calories))
```

After preparing our data, let's find the correlation of steps taken, active minutes and calorie consumption with time to fall asleep respectively.

```{r}
# Correlation between agg_fallasleep and agg_steps
r_intensity <- round(cor(sleep_activity_tr$agg_fallasleep, sleep_activity_tr$agg_steps), 3)
p_intensity <- cor.test(sleep_activity_tr$agg_fallasleep, sleep_activity_tr$agg_steps)$p.value
ggplot(sleep_activity_tr, aes(y=agg_fallasleep, x=agg_steps)) + 
  geom_point() + 
  geom_smooth(method="lm", col="skyblue") +
  annotate("text", x=5000, y=150, label=paste0("r = ", r_intensity), hjust=0) +
  annotate("text", x=5000, y=120, label=paste0("p = ", round(p_intensity, 3)), hjust=0)
# Correlation between agg_fallasleep and agg_veryactivemins
r_intensity <- round(cor(sleep_activity_tr$agg_fallasleep, sleep_activity_tr$agg_veryactivemins), 3)
p_intensity <- cor.test(sleep_activity_tr$agg_fallasleep, sleep_activity_tr$agg_veryactivemins)$p.value
ggplot(sleep_activity_tr, aes(y=agg_fallasleep, x=agg_veryactivemins)) + 
  geom_point() + 
  geom_smooth(method="lm", col="skyblue") +
  annotate("text", x=30, y=150, label=paste0("r = ", r_intensity), hjust=0) +
  annotate("text", x=30, y=120, label=paste0("p = ", round(p_intensity, 3)), hjust=0)
# Correlation between agg_fallasleep and agg_calories
r_intensity <- round(cor(sleep_activity_tr$agg_fallasleep, sleep_activity_tr$agg_calories), 3)
p_intensity <- cor.test(sleep_activity_tr$agg_fallasleep, sleep_activity_tr$agg_calories)$p.value
ggplot(sleep_activity_tr, aes(y=agg_fallasleep, x=agg_calories)) + 
  geom_point() + 
  geom_smooth(method="lm", col="skyblue") +
  annotate("text", x=2000, y=150, label=paste0("r = ", r_intensity), hjust=0) +
  annotate("text", x=2000, y=120, label=paste0("p = ", round(p_intensity, 3)), hjust=0)
```

From the plot above, we can see that calorie consumption has moderately negative correlation with time for falling asleep. **Thus, we could recommend our user burn more calories in order to obtain better sleep quality.**


## Share insights

Through the analysis, we find our target audience for the smart device is

* having a sedentary lifestyle
* active from 6am to 11pm
* walking the most at 6pm

Based on the data, we also discover that calorie burn needs the help of intensive physical activity other than walking more. If one wants to fall asleep faster, having a higher calorie burn is one of the solutions.

## Conclusion

For the marketing strategy, we can focus on promoting the device to women with a similar lifestyle as our target audiences, such as office workers, writers, and Uber drivers. The ads are better shown within the time range from 6am to 11pm. Other than that, we can also cooperate with businesses with the same group of customers to reach a larger audience.

As for attracting users, we can ask users to complete a questionnaire while setting the device to get an idea about what they expect to achieve with our product. For example, for users with an interest in losing weight, we can suggest they set an alarm for workouts as well as provide them weekly reports of their physical activity. All in all, showing that our device could be the best assistant for achieving ideal their physical/healthy target.