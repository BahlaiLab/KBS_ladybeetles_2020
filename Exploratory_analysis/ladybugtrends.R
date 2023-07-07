setwd("C:/Users/dauriael/OneDrive - Michigan State University/Ladybug 2020")


#####
#All CSVs are named for the SPID so I just use this one R Script and then I Find and Replace the SPID instead of having 14 separate scripts
###Any csv called SPID2020comb means that yes it has the 2020 data and the "comb" means I combined the forest and the main plots to be on one csv.

#bring data in from 
PQUA<-read.csv("PQUA2020comb.csv",
               header=T)


#https://github.com/cbahlai/PQUA/blob/master/lampyrid_analysis.R
#details of cleaning in the code in comments found at that link- in summary, get all the typoes
#out and make the date column usable


library(lubridate)
colnames(PQUA)[1]="DATE"
colnames(PQUA)[4]="REPLICATE"
PQUA$newdate<-mdy(PQUA$DATE)
PQUA$year<-year(PQUA$newdate)
PQUA$DOY<-yday(PQUA$newdate)
PQUA<-na.omit(PQUA)
PQUA$TREAT_DESC<-gsub("Early succesional community", "Early successional", PQUA$TREAT_DESC)
PQUA$TREAT_DESC<-gsub("Early successional community", "Early successional", PQUA$TREAT_DESC)
PQUA$TREAT_DESC<-gsub("Early Successional Community", "Early successional", PQUA$TREAT_DESC)
PQUA$TREAT_DESC<-gsub("Early sucessional community", "Early successional", PQUA$TREAT_DESC)
PQUA$TREAT_DESC<-gsub("poplar trees", "Poplar trees", PQUA$TREAT_DESC)
PQUA$TREAT_DESC<-gsub("Succesional", "Successional", PQUA$TREAT_DESC)
PQUA$TREAT_DESC<-gsub("Sucessional", "Successional", PQUA$TREAT_DESC)
PQUA$TREAT_DESC<-gsub("Alfalfa", "Alfalfa*", PQUA$TREAT_DESC)
PQUA$TREAT_DESC<-gsub("Switchgrass", "Alfalfa*", PQUA$TREAT_DESC)
PQUA$TREAT_DESC<-gsub("Biologically based \\(organic\\)", "Organic", PQUA$TREAT_DESC)
PQUA$TREAT_DESC<-gsub("Conventional till", "Conventional", PQUA$TREAT_DESC)
PQUA$TREAT_DESC<-as.factor(PQUA$TREAT_DESC)
PQUA$HABITAT<-as.factor(PQUA$HABITAT)
PQUA$REPLICATE<-as.factor(PQUA$REPLICATE)
PQUA$STATION<-as.factor(PQUA$STATION)
str(PQUA)

###########
#subset the data to include only data before August 10th or the 222 DOY
PQUA= subset(PQUA, DOY > 0 & DOY < 222)

# data is currently structured as subsamples, replicates, across treatments. We know from 
# http://biorxiv.org/content/early/2016/09/11/074633 that the organisms were most abundant in
# Alfalfa and no-till treatments, so we should use data from these treatments if we're 
# interested in picking up trends over time. We also don't really care about within
# year dynamics for this experiment- we essentially want a summary measure of what was going on
#within a rep, within a year. So let's reshape the data, drop out irrelevant treatments.

library(reshape2)
#tell R where the data is by melting it, assigning IDs to the columns
PQUA1<-melt(PQUA, id=c("DATE","TREAT_DESC","HABITAT","REPLICATE","STATION","newdate", "year", "DOY"))
#cast the data to count up the fireflies
PQUA2<-dcast(PQUA1, year+TREAT_DESC+REPLICATE~., sum)
#cast the data to count the traps
PQUA3<-dcast(PQUA1, year+TREAT_DESC+REPLICATE~., length)
#let's rename these new vectors within the data frame
names(PQUA2)[4]<-"ADULTS"
names(PQUA3)[4]<-"TRAPS"

#rename the data frame and combine the number of traps we counted into it from PQUA3
PQUA_summary<-PQUA2
PQUA_summary$TRAPS<-PQUA3$TRAPS

#create a new variable to account for trapping effort in a given year
PQUA_summary$pertrap<-PQUA_summary$ADULTS/PQUA_summary$TRAPS


#get rid of columns we don't need for analysis
PQUA_summary$REPLICATE<-NULL
PQUA_summary$ADULTS<-NULL
PQUA_summary$TRAPS<-NULL

####Creating a new treatment classification 

PQUA_summary$TREAT_CAT=ifelse(PQUA_summary$TREAT_DESC=="Coniferous"|
PQUA_summary$TREAT_DESC=="Deciduous"|
PQUA_summary$TREAT_DESC=="Successional", "Forest", 
ifelse(PQUA_summary$TREAT_DESC=="Alfalfa*"|
PQUA_summary$TREAT_DESC=="Early successional"|
PQUA_summary$TREAT_DESC=="Poplar trees", "Perennial",
ifelse(PQUA_summary$TREAT_DESC=="Conventional"|
PQUA_summary$TREAT_DESC=="No till"|
PQUA_summary$TREAT_DESC=="Organic"|
PQUA_summary$TREAT_DESC=="Reduced input", "Annual", "Check")))




#plyr library for ddply function below
library(plyr)

# First make a dataframe for error bars
PQUA_agg <- ddply(PQUA_summary, .(TREAT_CAT, year), summarise,
                  mean.bugs = mean(pertrap),
                  sd.bugs.pt = sd(pertrap),
                  N = length(pertrap),
                  SE = sd.bugs.pt / sqrt(N))

##Make line graph)
library(ggplot2)
ggplot(PQUA_agg, aes(x = year, y = mean.bugs, color = TREAT_CAT, shape = TREAT_CAT)) +
  geom_line(aes(group = TREAT_CAT)) +
  geom_ribbon(aes(ymin = mean.bugs - SE, ymax = mean.bugs + SE, 
                  group = TREAT_CAT, fill = TREAT_CAT), alpha = 0.3) +
  geom_point() +
  theme_bw()  +
  theme(
    # Hide panel borders and remove grid lines
    axis.line = element_line(colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
     text = element_text(size=16)) +
    scale_y_continuous(expand = c(0,.0))
