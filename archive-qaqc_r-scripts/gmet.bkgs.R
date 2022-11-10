

# Gasmet data QAQC --------------------------------------------------------


if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, googlesheets4, googledrive, lattice)

options(scipen=999)
options(digits=4)
# options(device = "windows")


driveurl<-"https://docs.google.com/spreadsheets/d/19b9jO55zhUFlHeox2HDDPjlKMKIAQzIUms5-Ye-2B7Q/edit#gid=0"

dat<-read_sheet(driveurl)


str(dat)

library(lattice)

xyplot(change~factor(date), dat)

dat %>%
  ggplot(aes(date,change)) +
  geom_point() +
  geom_smooth(method="lm",
              se=F,
              linetype=2,
              size=.7) +
  theme_bw() +
  labs(y="morning background max - afternoon background max",
       x="2021")


dat %>%
  ggplot(aes(date)) +
  geom_errorbar(aes(ymax=morningmax,ymin=afternoonmax)) +
  labs(y="background signal",
       x="2021",
       title = "background signal range before and after sampling on same day in 2021") +
  theme_bw()
