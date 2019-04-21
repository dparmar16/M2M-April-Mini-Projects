#Clear workspace
rm(list=ls())

#Load in packages
library(dplyr)
library(reshape2)
library(ggplot2)

#Load in dataset
#Found here:
#https://github.com/fivethirtyeight/data/tree/master/endorsements
endorsements <- read.csv("endorsements-2020-as-of-2019-04-14.csv", sep=",", header=TRUE)

#Preview data and do exporatory data analysis
head(endorsements)
unique(endorsements$body)
unique(endorsements$endorsee)
endorsements[1:100,-c(8)]
endorsements[endorsements$endorsee == "",-c(8)]

#Aggregate data by candidate and date
endorse_agg <- endorsements %>%
  filter(endorsee %in% c("Joe Biden","Julian Castro","Kamala Harris","Bernie Sanders","Cory Booker",
                         "Amy Klobuchar","Elizabeth Warren","Beto O'Rourke","Kirsten Gillibrand",
                         "Pete Buttigieg")) %>%
  group_by(endorsee, date) %>%
  summarise(totalpoints = sum(points)) %>%
  arrange(endorsee, date)

#Use ave function to get cumulative endorsement points by candidate over time
endorse_agg$csum <- ave(endorse_agg$totalpoints, endorse_agg$endorsee, FUN=cumsum)

#Plot candidate's endorsement points over tim3
ggplot(data=endorse_agg, aes(x=date, y=csum, group=endorsee, color=endorsee)) + 
  geom_point() + 
  geom_line() +
  geom_text(label=endorse_agg$endorsee, check_overlap = TRUE, size=3, hjust=0.5, vjust=-1.1) +
  theme(legend.position = "bottom",axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x="Date",y="Endorsement Points (FiveThrityEight)",title="Candidate Endorsement Points Over Time")


#Get each candidates endorsement points by state as a stacked bar
ggplot(data=endorsements[endorsements$endorsee %in% c("Joe Biden","Julian Castro","Kamala Harris","Bernie Sanders","Cory Booker",
                                         "Amy Klobuchar","Elizabeth Warren","Beto O'Rourke","Kirsten Gillibrand",
                                         "Pete Buttigieg"),], 
  aes(x=endorsee, y=points, fill=state)) + 
  geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
  labs(x="Candidate",y="Cumulative Endorsement Points", title="Cumulative Endorsement Points by State of Endorser")
