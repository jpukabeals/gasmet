

# Gasmet data QAQC --------------------------------------------------------


if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, googlesheets4, googledrive, lattice)

options(scipen=999)
options(digits=4)
# options(device = "windows")


driveurl<-"https://docs.google.com/spreadsheets/d/1b9q0DsRL9BWOcPrAQvrvP_6xW18I-0SMDeU6ja_atKA/edit#gid=0"

dat<-read_sheet(driveurl, sheet = 2)


str(dat)
#time data is messed up
#also let's clear out some of the background info

dat <- dat %>%
  dplyr::select(-SpectrumFile,-LibraryFile)

# regardless of source name, we can just look at sampling by date since no 
# experiment was sampled twice on same date



# water content -----------------------------------------------------------

dat %>%
  ggplot(aes(x=Date,y=water.percent)) +
  geom_point() +
  labs(x="fall 2020 to spring 2021",
       y="water vapor concentration\n(% by volume)")

dat %>%
  ggplot(aes(x=Time,y=water.percent)) +
  geom_point() +
  geom_smooth(method='lm') +
  labs(x="time of day (army time)",
       y="water vapor concentration\n(% by volume)")


dat %>%
  filter(Date>"2021-05-20") %>%
  ggplot(aes(x=Time,y=water.percent)) +
  geom_point(aes(color=experiment)) +
  geom_smooth(method='lm',
              aes(group=experiment,
                  color=experiment)) +
  labs(x="time of day (army time)",
       y="water vapor concentration\n(% by volume)")

dat %>%
  filter(Date>"2021-05-10") %>%
  ggplot(aes(x=Time,y=water.percent)) +
  geom_point(aes(color=experiment)) +
  geom_smooth(method='lm',
              se=F,
              color=1) +
  labs(x="time of day (army time)",
       y="water vapor concentration\n(% by volume)") +
  facet_wrap(~factor(Date),ncol = 3) +
  scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M")



# temperature -------------------------------------------------------------

dat %>%
  ggplot(aes(x=Date,y=celltemp.c)) +
  geom_point() +
  labs(x="fall 2020 to spring 2021",
       y="Cell temperature (C)")

dat %>%
  ggplot(aes(x=Time,y=celltemp.c)) +
  geom_point() +
  geom_smooth(method='lm') +
  labs(x="time of day (army time)",
       y="Cell temperature (C)")

dat %>%
  filter(Date>"2021-05-10") %>%
  ggplot(aes(x=Time,y=celltemp.c)) +
  geom_point(aes(color=experiment)) +
  geom_smooth(method='lm',
              se=F,
              color=1) +
  labs(x="time of day (army time)",
       y="Cell temperature (C)") +
  facet_wrap(~factor(Date),ncol = 3) +
  scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M")
