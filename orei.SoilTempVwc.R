

# Gasmet data QAQC --------------------------------------------------------


if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, googlesheets4, googledrive, lattice)

options(scipen=999)
options(digits=4)
# options(device = "windows")


driveurl<-"https://docs.google.com/spreadsheets/d/15ABmTZSQgAmpn7lz3296Emhy5O5XSOqFCLQPpaENBc4/edit#gid=1449315228"

dat<-read_sheet(driveurl, sheet = 3)

str(dat)

library(dplyr)

dat1 <- dat %>%
  dplyr::select(date, temp,vwc,timing,fertilizer)

str(dat1)
dat1 <- dat1 %>%
  mutate(timing = as.factor(timing),
         fertilizer = as.factor(fertilizer))

str(dat1)

library(lattice)


# soil water content ------------------------------------------------------

bwplot(vwc~timing, dat1)
bwplot(vwc~timing|date, dat1)
# ^^ seems to be an outlier for 5Apr in None, likely a sandy porous spot

bwplot(vwc~fertilizer, dat1)
bwplot(vwc~fertilizer|date, dat1)

bwplot(vwc~timing|fertilizer, dat1)
bwplot(vwc~fertilizer|timing, dat1)

bwplot(vwc~fertilizer|timing*date, dat1)
# ^^ seems like the control fertilizer treatments have higher water content


# soil temperature ------------------------------------------------------

bwplot(temp~timing, dat1)
bwplot(temp~timing|date, dat1)
# ^^ seems to be an outlier for 5Apr in None, likely a sandy porous spot

bwplot(temp~fertilizer, dat1)
bwplot(temp~fertilizer|date, dat1)

bwplot(temp~timing|fertilizer, dat1)
bwplot(temp~fertilizer|timing, dat1)

bwplot(temp~fertilizer|timing*date, dat1)
# ^^ no obvious differences in soil temp between treatments
# ^^ 5Apr was warmer than 22Apr

