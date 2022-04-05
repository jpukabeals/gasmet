

# importing a single samplling day ----------------------------------------

dat <- read.delim("C:/Users/pukab001/Desktop/calcmetsamplereanalyze/txt files/20201013/try1.TXT")

library(tidyverse)

str(dat)
dat1 <- dat %>%
  select(SpectrumFile)

library(stringr)

head(dat1)

head(str_detect(dat1$SpectrumFile,"33"))
head(str_locate(dat1$SpectrumFile,"EddyFlux"))
head(str_locate(dat1$SpectrumFile,".SPE"))

head(str_sub(dat1$SpectrumFile,60,89))

dat1 <- dat1 %>%
  mutate(pname=str_sub(dat1$SpectrumFile,60,89));dat1

head(str_locate(dat1$pname,"33"))

dat1 %>%
  mutate(plot=str_sub(pname,14,15)) %>%
  head()

head(str_locate(dat1$pname,"_"))
head(str_locate(dat1$pname,"0210"))


dat1 %>%
  mutate(obs=str_sub(pname,26,30)) %>%
  head()

head(str_locate(dat1$pname,"I10"))

dat1 %>%
  mutate(site=str_sub(pname,10,12)) %>%
  head()

dat2 <- dat %>%
  mutate(pname=str_sub(dat1$SpectrumFile,60,89),
         plot=str_sub(pname,14,15),
         obs=str_sub(pname,26,30),
         site=str_sub(pname,10,12),
         co2=Carbon.dioxide.CO2)

library(lattice)

str(dat2)

bwplot(co2~plot,dat2)

library(lubridate)

dat2 <- dat2 %>%
  mutate(time=hms::as_hms(Time))

xyplot(co2~as.numeric(time),dat2)


# importing multiple samplling days ---------------------------------------

dat <- read.delim("C:/Users/pukab001/Desktop/calcmetsamplereanalyze/txt files/try2.TXT")

dat %>%
  group_by(Date) %>%
  select(SpectrumFile)

unique(dat$Date)

if (dat$Date=="2020-10-13") {
  print("Oct13")
}

for (i in 1:length(dat$Date)) {
  if (dat$Date=="2020-10-13") {
    print("hi")
  }
}
#it's using the whole vector of dat$date instead of just the value at that row

for (i in 1:length(dat$Date)) {
  if (dat[2]=="2020-10-13") {
    print("hi")
  } else {
    print("bye")
  }
}

for (i in 1:length(dat$Date)) {
  if (dat[2,]=="2020-10-13") {
    print("hi")
  } else {
    print("bye")
  }
}

for (i in 1:length(dat$Date)) {
  if (i[2]=="2020-10-13") {
    print("hi")
  } else {
    print("bye")
  }
}

for (i in 1:length(dat$Date)) {
  if (i[2,]=="2020-10-13") {
    print("hi")
  } else {
    print("bye")
  }
}

for (i in 1:length(dat$Date)) {
  if (i[2,i]=="2020-10-13") {
    print("hi")
  } else {
    print("bye")
  }
}

for (i in 1:length(dat$Date)) {
  if (i[i,2]=="2020-10-13") {
    print("hi")
  } else {
    print("bye")
  }
}



dat[2]
dat[,2]
dat[556,2]
dat[6,2]


dat[4,2]=="2020-10-13"

for (i in 1:length(dat$Date)) {
  if (dat[i,2]=="2020-10-13") {
    print("13Oct")
  } else if (dat[i,2]=="2020-10-14"){
    print("14Oct")
  } else{
    print("2Nov")
  }
}

unique(dat[2])

df <- data.frame(Date=character())

for (i in 1:length(dat$Date)) {
  if (dat[i,2]=="2020-10-13") {
    df[i,1] <-"13Oct"
  } else if (dat[i,2]=="2020-10-14"){
    df[i,1] <-"12Oct"
  } else{
    df[i,1] <-"2Nov"
  }
}


# Parse and import factors from name --------------------------------------

head(str_sub(dat$SpectrumFile,60,89))

head(str_sub(dat$SpectrumFile,-35))

unique(str_length(dat$SpectrumFile))

dat %>%
  mutate(slength=str_length(SpectrumFile)) %>%
  arrange(slength) %>%
  group_by(slength) %>%
  tally()

dat %>%
  mutate(slength=str_length(SpectrumFile)) %>%
  arrange(slength) %>%
  select(slength,SpectrumFile)

# ^ sorting by length doesn't work well

dat %>%
  transmute(tname=str_trunc(SpectrumFile,38,side="left"))

o <- str_locate_all(dat$SpectrumFile,"\\\\")
head(o)

o[[1]]
o[1]

o[[1]]

o[[1]][6] #the last double slash

# now create a vector for id and for character location of last double slash

dat[1,4]
str_locate_all(dat[1,4],"\\\\")
p <- str_locate_all(dat[1,4],"\\\\")

p
p[6,6]
dim(p)

str(p)
p[6,2]

as.data.frame(p)[6,2]

count(str(as.data.frame(p)$start))

as.data.frame(str_locate_all(dat[1,4],"\\\\"))[6,2] #note that 6 is the number of rows in dataframe

dat$end <- "placeholder"

for (i in 1:length(dat$Date)) {
  dat[i,47] <- as.data.frame(str_locate_all(dat[i,4],"\\\\"))[6,2]
}
dat$end

str_trunc(dat$SpectrumFile,54,side="left")[1]
str_length(dat$SpectrumFile)
str_trunc(dat$SpectrumFile,38,side="left")[400]

str_sub(dat$SpectrumFile,start = 55, end=-5)

dat %>%
  mutate(fname=str_sub(SpectrumFile,start = 55, end=-5)) %>%
  mutate(sl = str_length(fname)) %>%
  select(sl) %>% 
  group_by(sl) %>%
  tally()


