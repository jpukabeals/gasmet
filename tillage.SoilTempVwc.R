


# Gasmet data QAQC --------------------------------------------------------


if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, googlesheets4, googledrive)

options(scipen=999)
options(digits=4)
# options(device = "windows")


driveurl<-"https://docs.google.com/spreadsheets/d/15ABmTZSQgAmpn7lz3296Emhy5O5XSOqFCLQPpaENBc4/edit#gid=651356398"

dat<-read_sheet(driveurl, sheet = 2)

str(dat)

library(dplyr)

dat1 <- dat %>%
  dplyr::select(date, temp,vwc,treatment)

str(dat1)
dat1 <- dat1 %>%
  mutate(treatment = as.factor(treatment))

str(dat1)

xtabs(vwc~date+treatment, data = dat1)

lattice::bwplot(vwc~treatment, dat1)
lattice::bwplot(vwc~treatment|date, dat1)
#control and fall glyphosate have higher water content


lattice::bwplot(temp~treatment, dat1)
lattice::bwplot(temp~treatment|date, dat1)

