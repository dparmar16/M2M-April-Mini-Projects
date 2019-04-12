#Clear workspace
rm(list=ls())

#Load in packages
library(dplyr)
library(reshape2)
library(ggplot2)
install.packages("directlabels")
library("directlabels")
install.packages("ggrepel")
library("ggrepel")
install.packages("RColorBrewer")
library("RColorBrewer")
install.packages("wesanderson")
library(wesanderson)

#Load in dataset
#Found here:
#https://github.com/fivethirtyeight/data/tree/master/polls
polls <- read.csv("president_primary_polls_2019_04_07.csv", sep=",", header=TRUE)

head(polls)

polls %>%
  group_by(start_date) %>%
  summarise(rowcount = n_distinct(poll_id)) %>%
  arrange(start_date) %>%
  as.data.frame()

unique(polls$candidate_name)

dems <- polls[polls$party == 'DEM' & 
                as.Date(polls$start_date, "%m/%d/%y") >= as.Date('2019/03/11') & 
                polls$state =="" &
                polls$candidate_name %in% c("Bernard Sanders","Elizabeth Warren","Joseph R. Biden Jr.","Pete Buttigieg",
                "Beto O'Rourke","Cory A. Booker","Kamala D. Harris","Andrew Yang","Amy Klobuchar","Kirsten E. Gillibrand","Howard Schultz"),]

ggplot(dems, aes(x=start_date, y=pct, group= candidate_name, color = candidate_name)) + 
  geom_point() + 
  #geom_text(aes(label = candidate_name, colour = candidate_name, x = Inf, y = pct)) +
  stat_smooth(method = 'lm', se = FALSE, show.legend = TRUE,inherit.aes = TRUE) +
  scale_colour_discrete(guide = 'legend') +
  theme(plot.margin = unit(c(1,3,1,1), "lines")) +
  labs(x="Poll Date",y="Percent of Voters", title = "Democratic Primary Polling by Candidate")
  

# dem_pivot <- dcast(dems, question_id + start_date ~ candidate_name, value.var = "pct")
# 
# names(dem_pivot) <- make.names(names(dem_pivot), unique = TRUE)
# 
# ggplot(dem_pivot, aes(start_date)) +
#   geom_point(aes(y=Bernard.Sanders), colour = "red", size = 2) +
#   geom_point(aes(y=Beto.O.Rourke), colour = "blue", size = 2) +
#   geom_point(aes(y=Elizabeth.Warren), colour = "green", size = 2) +
#   geom_point(aes(y=Joseph.R..Biden.Jr.), colour = "black", size = 2) +
#   geom_point(aes(y=Kamala.D..Harris), colour = "orange", size = 2)
