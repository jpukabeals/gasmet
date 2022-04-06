#Code to analyze TLI GHG flux data from East Bank Plots

#Script to extract Gasmet text files, regorganize them,
#and create simple figures for data interpretation.

# Jesse doesn't have google drive, so I'm downloading from the drive and extracting zipped file
# setwd("C:\\Users\\pukab001\\Documents\\R data\\2018")

mygoogledoc <- "C:\\Users\\pukab001\\Documents\\R data\\East_Bank_Kernza_TLI"


#Direct R to the folder where all the text files have been copied.
setwd(paste0(mygoogledoc,"\\Results\\2018")) #added 2018 cuz things got more complex
# datenames<-grep("txt|TXT", list.files(), value=TRUE) #make a object with all the files in it ending with txt
#Here's a loop that reformates the encoding from Mac to UTF8
#It dumps the new files into a folder "NewFiles"
# for(j in 1:(length(datenames))){
#   tmpplot<-datenames[j]
#   writeLines(iconv(readLines(tmpplot),
#                    from = "macintosh", to = "UTF8"),
#              file(paste0(mygoogledoc,"Results/NewFiles/",tmpplot), encoding="UTF-8"))
# }
# ^ commented out, im on a PC
# 
# for(j in 1:(length(datenames))){
#     tmpplot<-datenames[j]
# }



# dir.create("C:\\Users\\pukab001\\Documents\\R data\\East_Bank_Kernza_TLI/Results/2018/NewFiles")
# file(paste0(mygoogledoc,"/Results/2018/NewFiles"))
# dir.create(file(paste0(mygoogledoc,"/Results/2018/NewFiles")))
# ^ needed to add "NewFiles" folder


#Set directory to that new folder
# setwd(paste0(mygoogledoc,"/Results/2018\\NewFiles"))

#make a character string with all the file names in it. The grep drops the one weird object in the folder that's not a file
fnames<-grep("txt|TXT", list.files(), value=TRUE)
library(stringr)
#Make the dataframe to fill, which will include all the shortened and formatted text files  
shortdat<-data.frame(matrix(ncol=8)) 
#Fill in the column names
colnames(shortdat)<-c("date", "CO2", "CH4","N2O","plot","day", "code","time")
#This loop extracts only the relevant data from each text file
  for(i in 1:length(fnames)){ #runs through all the files in the folder
    sdat<-read.table(paste(fnames[i]), header=T, stringsAsFactors=F, sep="\t", fill = TRUE) #opens the text files, Jesse added "fill=TRUE"
    if(colnames(sdat)[1]=="Line"){ #The old files that were repredicted by the Gasmet folks have a strange format. This identifies those files by seeing if the first column is labeled "Line"
      sdat$date<-paste(sdat$Date, sdat$Time) #These files need to have two columns merged into one "time" column
      keepcols<-c(47,10,14,18) #The time column and the gasses of interest are in these columns
      sdat<-sdat[,keepcols] #Here we drop all the other columns that are not of interest
    }else{
      keepcols<-c(1,4,6,8) #This does the same thing, but for the files that were created with the updated gasmet equation
      sdat<-sdat[,keepcols]
    } 
    colnames(sdat)<-c("date", "CO2", "CH4","N2O") #rename those columns into something more simple
    sdat$plot<-str_split_fixed(fnames[i], " ",2)[,1] #create a "plot" column, and extract the plot id from the file name
    sdat$day<-str_split_fixed(fnames[i], " |[.]",3)[,2] #create a "day" column, and extract the day from the file name
    sdat$code<-str_split_fixed(fnames[i], "[.]",2)[,1] #create a "code" column, which is the plot and date combined, which can be extracted from the file name
    sdat$time<-c((1:nrow(sdat)*20)/3600) #this is the "time" in fractions of an hour, since our final units will be in CO2e/m2/hour
    shortdat<-rbind(shortdat, sdat)
  }
shortdat$block<-substr(shortdat$plot,1,1) #Add a column for the blocks
shortdat$plot<-as.numeric(shortdat$plot)
shortdat$CO2<-as.numeric(shortdat$CO2)
shortdat$CH4<-as.numeric(shortdat$CH4)
shortdat$N2O<-as.numeric(shortdat$N2O)

shortdat<-shortdat[-1,] #Have to remove that first row of NA's that was filled when we created the table before the loop

#read in treatment list for plots
dattrt<-read.csv(paste0(mygoogledoc,"/TreatmentList.csv"), stringsAsFactors=F)
#dattrt$plot<-as.character(dattrt$plot)
library(dplyr)

shortdat$trt<-left_join(shortdat,dattrt,"plot")[,10]

#Now all the raw data are in one file with the identifiers we need. Next, we'll convert some values so that our
#response variables are in a nice unit for reporting.
shortdat$CO2.C<-shortdat$CO2*0.00049886#in micro grams per cubic cm
shortdat$CH4.C<-shortdat$CH4*0.00049886
shortdat$N2O.N<-shortdat$N2O*0.001164

#converting concentration of each GHG to CO2 equivelants in g/ha.
#Equation from Gelfand Supplemental Material
shortdat$CO2.e<-shortdat$CO2
shortdat$CH4.e<-shortdat$CH4*25
shortdat$N2O.e<-shortdat$N2O*298

shortdat$plot<-as.character(shortdat$plot)

library(tidyr)

shortdat <- shortdat %>%
  drop_na()

#Calculate flux using linear regression to plot change in gas concentration through time. 
#A linear regression is ok, but it doesn't account for any effect of chamber on flux. 
#The chamber is known to slow the flux rate as gasses get trapped in the soil pore space,
#thus any bias introduced by the chamber is influenced by soil pore space, temp, and moisture content. 
#Next year, we'll measure all those throughout the growing season.
library(lubridate)

shortdat$date <- as_datetime(shortdat$date)
date(shortdat$date)


library(ggplot2)
ggplot(shortdat, aes(y=CO2, x=time, color=factor(plot)))+
  facet_grid(month(shortdat$date))+
  geom_point()+
  geom_line()


#Truncate the data by removing all values prior to 100 seconds
#note that time is in fraction of an hour
tshortdat<-shortdat[shortdat$time>0.028,]
rownames(tshortdat)<-seq(length=nrow(tshortdat))#reset row numbers

#Look for outliers. Here we make a boxplot of values by date and treatment,
#and then create a table of those outliers and where they are located in the 
#data frame (based on row number)
outlierlook<-function(gas1){
  colnum<-which(colnames(tshortdat)==gas1)
  boxplot(tshortdat[,colnum]~day*trt, data=tshortdat, range=2)
  tout<-boxplot.stats(tshortdat[,colnum], coef=2)$out
  toutable<-data.frame("GasValue"=boxplot.stats(tshortdat[,colnum], coef=2)$out,
                       "GasLocation"=which(tshortdat[,colnum] %in% tout))
  print(toutable)
}
outlierlook("CO2")
#Another way to spot outliers. The last point of the plot with IWG-con in block 1 on 06212017
#Plot all the data
ggplot(tshortdat, aes(y=CO2, x=time, color=plot))+
  facet_grid(day~trt)+
  geom_point()+
  geom_line()

library(plyr)
#CO2 without block
co2regressions <- dlply(tshortdat, .(day, trt), lm, formula = CO2.C ~ time)
co2coefs <- cbind(ldply(co2regressions, coef)[,c(1,2,4)],
               ldply(co2regressions, function(x){coef(summary(x))[4]})[,3])
colnames(co2coefs)<-c("day","trt", "slope", "slope_se")
co2coefs$day2<-as.Date(as.character(co2coefs$day),"%m%d%Y")
df2 <- data.frame(x1 = as.Date(as.character("11092017"),"%m%d%Y"),
                 x2 = as.Date(as.character("11092017"),"%m%d%Y"),
                 y1 = 1, y2 = 0.5)
#A color palette for color blindness
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","gray30")

ggplot(co2coefs, aes(y=slope, x=day2, color=trt))+
  geom_point()+
  geom_line()+
  geom_errorbar(width=.1, aes(ymin=slope-slope_se, ymax=slope+slope_se)) +
  xlab("Date")+
  ylab("Flux (micro grams CO2-C/cm3/hour)")+
  scale_x_date(date_breaks= ("25 days"),  date_labels = "%d %b %Y")+
  scale_color_manual(values=cbPalette, name="Treatment")+
  geom_segment(data=df2, aes(x=x1,xend=x2,y=y1, yend=y2),
               colour = "red", alpha=0.6, size=1, arrow=arrow())+
  theme(#panel.grid.major=element_blank(),
    #panel.grid.minor=element_blank(),
    #panel.background=element_rect(color="black", fill="white"),
    legend.title=element_blank(),
    legend.position=c(.25,.8),
    legend.box = "horizontal",
    legend.key=element_blank(),
    legend.text=element_text(size=10),
    legend.key.size = unit(5,"mm"),
    legend.background = element_blank(),
    axis.line = element_line(color='black'),        
    axis.text.x=element_text(size=10, color='black'),
    axis.title.x = element_blank(),
    axis.title.y=element_text(size=10, color='black'),
    axis.text.y=element_text(size=10, color='black'))
#Remove hashtag below to save figure
#ggsave("CO2flux_all.pdf", width=8, height=5, units="in", path="C:/Users/Jacob Jungers/Google Drive/Intermediate_wheatgrass_research/GMI_Experiments/East_Bank_Kernza_TLI")
#ggsave("CO2flux_all.jpg", width=8, height=5, units="in", path="C:/Users/Jacob Jungers/Google Drive/Intermediate_wheatgrass_research/GMI_Experiments/East_Bank_Kernza_TLI")
ggsave("CO2flux_all.pdf", width=8, height=5, units="in", path="C:\\Users\\pukab001\\Documents\\R figures")


ch4regressions <- dlply(tshortdat, .(day, trt), lm, formula = CH4.C ~ time)
ch4coefs <- cbind(ldply(ch4regressions, coef)[,c(1,2,4)],
                  ldply(ch4regressions, function(x){coef(summary(x))[4]})[,3])
colnames(ch4coefs)<-c("day","trt", "slope", "slope_se")
ch4coefs$day2<-as.Date(as.character(ch4coefs$day),"%m%d%Y")

ggplot(ch4coefs, aes(y=slope, x=day2, color=trt))+
  geom_point()+
  geom_line()+
  geom_errorbar(width=.1, aes(ymin=slope-slope_se, ymax=slope+slope_se)) +
  xlab("Date")+
  ylab("Flux (micro grams CH4-C/cm3)")

#This is where the N2O flux estimates occur
n20regressions <- dlply(tshortdat, .(day, trt), lm, formula = N2O.N ~ time)
n20coefs <- cbind(ldply(n20regressions, coef)[,c(1,2,4)],
                  ldply(n20regressions, function(x){coef(summary(x))[4]})[,3])
colnames(n20coefs)<-c("day","trt", "slope", "slope_se")
n20coefs$day2<-as.Date(as.character(n20coefs$day),"%m%d%Y")

#This is an important figure. Note that gap between mid May and late July...
ggplot(n20coefs, aes(y=slope, x=day2, color=trt))+
  geom_point()+
  geom_line()+
  geom_errorbar(width=.1, aes(ymin=slope-slope_se, ymax=slope+slope_se)) +
  xlab("Date")+
  ylab("Flux (micro grams N2O-N/cm3)")

##########################################################################
#Now include chamber volume, area, and height: Note that this changed when the extension was used, starting June 21.
#Fill in the appropriate values for your chamber
chamberdims<-data.frame(matrix(ncol=7))
colnames(chamberdims)<-c("clength", "cwidth","carea","cvol","cheight","cvolext", "cheightext")
chamberdims$clength<-50.165 #cm
chamberdims$cwidth<-32 #This is from a picture
chamberdims$carea<-chamberdims$clength*chamberdims$cwidth
chamberdims$cvol<-14287 #cm3 Using 50.165*32*8.9 as 8.9 should be the depth of the pan
chamberdims$cheight<-chamberdims$cvol/chamberdims$carea
chamberdims$cvolext<-9550
chamberdims$cheightext<-chamberdims$cvolext/chamberdims$carea
#Use the above table to calculate flux by area

#Running a loop to measure ghg fluxes on CO2e basis.
outall<-data.frame(matrix(ncol=7))
colnames(outall)<-c("day", "trt", "slope", "slope_se", "day2","flux", "GHG")
for(i in 11:13){ #If running CO2-C, CH4-C, and N2O-N, then units are in micro grams per cm3
  mydat<-data.frame(tshortdat[,1:10], respo=tshortdat[,i])
  regressionsx <- dlply(mydat, .(day, trt), lm, formula = respo ~ time)
  coefsx <- cbind(ldply(regressionsx, coef)[,c(1,2,4)],
                  ldply(regressionsx, function(x){coef(summary(x))[4]})[,3])
  colnames(coefsx)<-c("day","trt", "slope", "slope_se")
  coefsx$day2<-as.Date(as.character(coefsx$day),"%m%d%Y")
  for(j in 1:length(coefsx$day2)){ #Convert micro grams/cm3 (volume) to micro grams per cm2 (area)
    if(as.numeric(coefsx$day[j]) < 06202017){ 
      coefsx$flux[j]<-coefsx$slope[j]*chamberdims$cheight
    }else{
      coefsx$flux[j]<-coefsx$slope[j]*chamberdims$cheightext
    }
  }
  
  coefsx$GHG<-colnames(tshortdat)[i]
  outall <- rbind(outall, coefsx)
  #print(coefsx)
}
outall$day2<-as.Date(as.character(outall$day),"%m%d%Y")
outall<-outall[-1,] #drop that first row
#currently, flux is in micro grams per cm2 area per hour. 
#Next we convert to CO2e in g per ha per day
#The equations for conversion are based on Gelfand et al supplemental material
outall$CO2.e<-NA
contab<-data.frame(outall$GHG, 
                   "mass.area.con"=2400, #converts micrograms/cm2/hour to g/ha/day
                   "gas.con"=NA, #gas converter from Gelfand
                   "IPCC.con"=NA) #IPCC GHG factor
#fill in gas.con and IPCC.con for each gas
table(contab$outall.GHG) #see how many of each gas there are in the table
nst<-table(contab$outall.GHG)[1]
contab$gas.con<-rep(c((44/12),(16/12),(44/28)), each=nst)
contab$IPCC.con<-rep(c(1, 25, 298), each=nst)

#Now multiply these vectors
outall$CO2.e<-outall$flux*contab$gas.con*contab$IPCC.con*contab$mass.area.con #units in g co2e per ha per day
#Just a check of things
ggplot(outall, aes(x=day2, y=CO2.e, color=trt))+
  facet_grid(~GHG)+
  geom_point()+
  geom_line()

library(pracma)
#GHG values are in g CO2 per ha per day, this is what we need if our x unit is in days.
#Use the trapezoid integration function, but convert days to numbers... doesn't work with posix dates
#use "strftime" to convert to day of year
outall$day3<-as.numeric(strftime(outall$day2, format="%j"))
cumGHG<-ddply(outall, .(trt, GHG), summarize, trapz(day3,CO2.e))
#convert units to kg
cumGHG$CO2.e<-cumGHG[,3]*0.001
###################################################

ggplot(cumGHG, aes(x=trt, y=CO2.e, fill=GHG))+
  geom_bar(position=position_dodge(), stat="identity")+
  ylab(expression("Cumulative GHG as CO2 equivalents" ~ (kg ~ ha^{-2})))+  
  scale_color_discrete(guide = guide_legend(title = "Crop"))+
  theme(
    panel.grid.major=element_blank(),
    #panel.grid.minor=element_blank(),
    #panel.background=element_rect(color="black", fill="white"),
    panel.background=element_rect(color="black"),
    axis.title.x=element_blank()
  )
#ggsave("CumulativeGHGplot2.pdf", width=12, height=5, units="in", path="C:/Users/Jacob Jungers/Documents/Projects/MDA-Wheatgrass/GMI-Carbon/Research/GMI_2017_Meetings")

#Just potting N2O in terms of CO2e

ggplot(subset(cumGHG, GHG=="N2O.N"), 
       aes(x=trt, y=CO2.e))+
  geom_bar(position=position_dodge(), stat="identity")+
  ylab(expression("Cumulative GHG as CO2 equivalents" ~ (kg ~ ha^{-2})))+  
  scale_color_discrete(guide = guide_legend(title = "Crop"))+
  theme(
    panel.grid.major=element_blank(),
    #panel.grid.minor=element_blank(),
    #panel.background=element_rect(color="black", fill="white"),
    panel.background=element_rect(color="black"),
    axis.title.x=element_blank()
  )
#ggsave("CumulativeGHGplot4.pdf", width=12, height=5, units="in", path="C:/Users/Jacob Jungers/Documents/Projects/MDA-Wheatgrass/GMI-Carbon/Research/GMI_2017_Meetings")


