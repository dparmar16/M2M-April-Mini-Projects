#Clear workspace
rm(list=ls())


#Load in packages
library(dplyr)
library(reshape2)
library(ggplot2)
library(lubridate)
library(data.table)


#Load in dataset
#Found here:
#https://github.com/fivethirtyeight/data/tree/master/trump-approval-ratings
polls <- read.csv("trump_approval_polls_2019-04-21.csv", header=TRUE)


#Exporatory data analysis
#Look at dataset
head(polls)
#Identify data type of each column
sapply(polls, class)
#Look at number of polls by pollster rating
polls %>%
  group_by(grade) %>%
  summarise(pollcount = n())
#Explore the "weight" field
polls %>%
  group_by(startdate) %>%
  summarise(polling_average = mean(adjusted_approve*weight))


#Create "start week" column to reduce noise and have cleaner chart
polls$poll_week <- floor_date(as.Date(polls$startdate, "%m/%d/%Y"), unit="week")


#Get weekly averages - all adults, registered voters, A-rated pollsters
all_adults <- polls %>%
  group_by(poll_week) %>%
  summarise(approval_all = sum(adjusted_approve*weight)/sum(weight))

registered_voters <- polls %>%
  filter(population == "rv") %>%
  group_by(poll_week) %>%
  summarise(approval_rv = sum(adjusted_approve*weight)/sum(weight))

registered_voters_with_grade_a_polls <- polls %>%
  filter(population == "rv" & grade %in% c("A-","A","A+")) %>%
  group_by(poll_week) %>%
  summarise(approval_pollster_a = sum(adjusted_approve*weight)/sum(weight))

polls_avg <- left_join(left_join(all_adults, registered_voters, by = "poll_week"), registered_voters_with_grade_a_polls, by="poll_week")

#Melt data to plot each of the columns
plot_data <- melt(polls_avg, id.vars = "poll_week", variable.name = "poll_type")


#overlay trend of all adults vs registered voters vs grade a pollsters
ggplot(data = plot_data, aes(x=poll_week, y=value, group=poll_type, color=poll_type)) +
  geom_point() +
  geom_smooth(se=FALSE, method="loess") +
  theme(legend.position = "bottom") +
  labs(title = "Trump Approval Rating by Week",x="Week of Poll Date",y="Weighted Approval Rating", colour="Poll Type")


#Create column with one week lag for each column and get percentage change
polls_avg$last_approval_all <- shift(polls_avg$approval_all, n=1L, type="lag")
polls_avg$last_approval_rv <- shift(polls_avg$approval_rv, n=1L, type="lag")
polls_avg$last_approval_pollster_a <- shift(polls_avg$approval_pollster_a, n=1L, type="lag")

polls_avg$approval_all_change <- (polls_avg$approval_all - polls_avg$last_approval_all)/polls_avg$last_approval_all
polls_avg$approval_rv_change <- (polls_avg$approval_rv - polls_avg$last_approval_rv)/polls_avg$last_approval_rv
polls_avg$approval_pollster_a_change <- (polls_avg$approval_pollster_a - polls_avg$last_approval_pollster_a)/polls_avg$last_approval_pollster_a

#Identify greatest one week rises and dips in all adults approval rating
change_from_previous <- as.data.frame(polls_avg[,c("poll_week","approval_all","last_approval_all","approval_all_change")])
change_from_previous$approval_all_change = change_from_previous$approval_all_change*100

change_from_previous[which.max(change_from_previous$approval_all_change),]
change_from_previous[which.min(change_from_previous$approval_all_change),]
                     