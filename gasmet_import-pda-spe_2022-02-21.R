# Jesse
# last updated 28feb2022


# importing data from PDA's uploaded to google drive
# only focusing on .SPE files
# there is a chance that some observations were only saved as .TXT files
# future work will be to see if .TXT can fill gaps in the .SPE observations

## NautizX8
# SD CARD > results > samples (n=411)
# ^no data related to our experiments, mosly just PURGE outputs
# My documents > results > samples (n=4524)
# ^good data

# Nautizx7
# storage card > samples (n=2059)
# ^good data

# these are all the files that I am extracting for this round


# importing files into R
# imported in batches due to large file size
dat.12 <- read.delim("C:\\Users\\pukab001\\Desktop\\calcmetsamplereanalyze\\txt files/Nautizx_afterJessIT.2741.6584.txt")
dat.13 <- read.delim("C:\\Users\\pukab001\\Desktop\\calcmetsamplereanalyze\\txt files/Nautizx_afterJessIT.1.2741.txt")
# combining batches into single dataframe
dat <- rbind(dat.12,dat.13)


# selecting and beautifying names of data
dat1 <- dat %>%
  dplyr::select(Date,Time,SpectrumFile,Carbon.dioxide.CO2,
                Nitrous.oxide.N2O,Methane.CH4,
                Ammonia.NH3,Cell.temperature) %>%
  rename(date=Date,
         time=Time,
         co2=Carbon.dioxide.CO2,
         spectrumfile=SpectrumFile,
         n20=Nitrous.oxide.N2O,
         ch4=Methane.CH4,
         nh3=Ammonia.NH3,
         temp=Cell.temperature) %>% 
  mutate(date=as.POSIXct(date,
                         format="%Y-%m-%d"),
         datetime=paste(date,time),
         datetime=as.POSIXct(datetime,
                             format="%Y-%m-%d %H:%M:%S")) %>% 
  relocate(datetime,
           .before=date);str(dat1)

# extracting relevant information from filename 
dat1 %>% 
  mutate(filename=str_sub(spectrumfile,76,-12)) %>%
  mutate(filename=tolower(filename)) %>%
  distinct(filename)

dat1 <- dat1 %>%
  mutate(filename=str_sub(spectrumfile,76,-12)) %>%
  mutate(filename=tolower(filename))


# now I need to figure out the patterns in the filename
# let's start with common names for experiments


# orei-manure -------------------------------------------------------------

# for orei-manure, selecting all filenames with "man" should capture all datafiles
# no filenames matched "orei"

dat1 %>%
  # glimpse()
  mutate(nametype=str_detect(dat1$filename,"man")) %>%
  # mutate(nametype=str_detect(dat1$filename,"orei")) %>%
  # mutate(nametype=str_detect(dat1$filename,"rose")) %>%
  filter(nametype==T) %>% 
  dplyr::select(filename)

dat.oreimanure <- dat1 %>%
  mutate(nametype=str_detect(dat1$filename,"man")) %>%
  filter(nametype==T)

dat.oreimanure$filename %>%
  str_split(.,
            pattern=" ",
            simplify = T) %>%
  as.data.frame() %>% 
  colnames()

dat.oreimanure$filename %>%
  str_split(.,
            pattern=" ",
            simplify = T) %>%
  as.data.frame() %>%
  bind_cols(.,dat.oreimanure) %>%
  write.csv("oreimanure_untidy.csv")
# going to try to manually clean

# V1 is always manure
# V2 is always rose
# V3 had minor issues for plot number
# V4 sometimes had "real" value, this has been changed

dat <- read.csv("oreimanure_untidy.csv") %>%
  rename(rowid=X,
         experiment=V1,
         site=V2,
         plot=V3) %>% 
  dplyr::select(-V5,-nametype,-V4);str(dat)


# driveurl<- "https://docs.google.com/spreadsheets/d/1GHp29yl8oZXCyFgGhc117n41_HVwL7HkfZToE-0YPWQ/edit#gid=604700612"
# orei.codes <- read_sheet(driveurl)
orei.codes<-read.csv("treatment codes orei manure - Sheet2.csv")
orei.codes$plot <- as.integer(orei.codes$plot)

dat %>%
  mutate(site="rosemount") %>%
  inner_join(.,orei.codes,
             by="plot") %>%
  select(date,time,co2,plot,site,
         n20,ch4,nh3,temp,filename,real) %>%
  write.csv("oreimanure_pda-data_28feb2022.csv",
            row.names = F)



# eddyflux ----------------------------------------------------------------

# all we need is plot number
# any plot with v08 already has fgi in the name
# some plots have i10 in the name without fgi

# step 1
# select all plots that have fgi and save that as jakes data

dat.eddyflux.1 <- dat1 %>% 
  mutate(nametype=str_detect(dat1$filename,"fgi")) %>%
  filter(nametype==T);dat.eddyflux.1

# select all plots that have i10 and no fgi, create that as second batch of jakes data
dat.eddyflux.2 <- dat1 %>%
  mutate(nametype=str_detect(dat1$filename,"i1")) %>%
  mutate(fgi_test=str_detect(dat1$filename, "fgi")) %>%
  filter(nametype==T &
           fgi_test==F) %>% 
  dplyr::select(-fgi_test);glimpse(dat.eddyflux.2)

# bind the two datasets together
dat.eddyflux <- bind_rows(dat.eddyflux.1,
          dat.eddyflux.2) 

# universally, it seems filenames are seperated by spaces
# it seems the second to last location is always the plot number
# the plot number is all that matters

eddyflux.filenames <- str_split(dat.eddyflux$filename,
          pattern = " ",
          simplify = T) %>%
  as.data.frame();eddyflux.filenames

bind_cols(eddyflux.filenames,dat.eddyflux) %>% 
  write.csv("eddyflux.untidy.csv")

# I fixed things manually in excel, now I'm reading in my fix

temp <- read.csv("eddyflux.tidy.csv") %>%
  # glimpse()
  # dplyr::select(-c(X,V1,V2,V3,V5)) %>% 
  # glimpse()
  dplyr::select(datetime,date,time,plot,
                co2,
                spectrumfile,
                filename,
                n20,ch4,nh3,temp)

# now I want to add site based on plot number
# simply, all plots<=32 are v08

temp.i10 <- temp %>%
  arrange(plot) %>% 
  filter(plot>32) %>% 
  mutate(site="i10")

temp.v08 <- temp %>% 
  arrange(plot) %>% 
  filter(plot<33) %>%
  mutate(site="v08")

bind_rows(temp.v08,
          temp.i10) %>%
  relocate(site,
           .before=plot) %>%
  write.csv("eddyflux_gasmet-data-update_pda-spe_20220301.csv",
            row.names = F)
