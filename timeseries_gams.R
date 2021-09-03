# start by cleaning data

#######################################
#bring data in from 
PQUA<-read.csv("data/PQUA2020comb.csv",
               header=T)


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
#subset the data to include  1993 or later, because sampling changed to add forests then
PQUA= subset(PQUA, year >= 1993)

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


#####################################################################################

#HVAR

#bring data in from 
HVAR<-read.csv("data/HVAR2020comb.csv",
               header=T)


library(lubridate)
colnames(HVAR)[1]="DATE"
colnames(HVAR)[4]="REPLICATE"
HVAR$newdate<-mdy(HVAR$DATE)
HVAR$year<-year(HVAR$newdate)
HVAR$DOY<-yday(HVAR$newdate)
HVAR<-na.omit(HVAR)
HVAR$TREAT_DESC<-gsub("Early succesional community", "Early successional", HVAR$TREAT_DESC)
HVAR$TREAT_DESC<-gsub("Early successional community", "Early successional", HVAR$TREAT_DESC)
HVAR$TREAT_DESC<-gsub("Early Successional Community", "Early successional", HVAR$TREAT_DESC)
HVAR$TREAT_DESC<-gsub("Early sucessional community", "Early successional", HVAR$TREAT_DESC)
HVAR$TREAT_DESC<-gsub("poplar trees", "Poplar trees", HVAR$TREAT_DESC)
HVAR$TREAT_DESC<-gsub("Succesional", "Successional", HVAR$TREAT_DESC)
HVAR$TREAT_DESC<-gsub("Sucessional", "Successional", HVAR$TREAT_DESC)
HVAR$TREAT_DESC<-gsub("Alfalfa", "Alfalfa*", HVAR$TREAT_DESC)
HVAR$TREAT_DESC<-gsub("Switchgrass", "Alfalfa*", HVAR$TREAT_DESC)
HVAR$TREAT_DESC<-gsub("Biologically based \\(organic\\)", "Organic", HVAR$TREAT_DESC)
HVAR$TREAT_DESC<-gsub("Conventional till", "Conventional", HVAR$TREAT_DESC)
HVAR$TREAT_DESC<-as.factor(HVAR$TREAT_DESC)
HVAR$HABITAT<-as.factor(HVAR$HABITAT)
HVAR$REPLICATE<-as.factor(HVAR$REPLICATE)
HVAR$STATION<-as.factor(HVAR$STATION)
str(HVAR)

###########
#subset the data to include only data before August 10th or the 222 DOY
HVAR= subset(HVAR, DOY > 0 & DOY < 222)
#subset the data to include  1993 or later, because sampling changed to add forests then
HVAR= subset(HVAR, year >= 1993)

library(reshape2)
#tell R where the data is by melting it, assigning IDs to the columns
HVAR1<-melt(HVAR, id=c("DATE","TREAT_DESC","HABITAT","REPLICATE","STATION","newdate", "year", "DOY"))
#cast the data to count up the fireflies
HVAR2<-dcast(HVAR1, year+TREAT_DESC+REPLICATE~., sum)
#cast the data to count the traps
HVAR3<-dcast(HVAR1, year+TREAT_DESC+REPLICATE~., length)
#let's rename these new vectors within the data frame
names(HVAR2)[4]<-"ADULTS"
names(HVAR3)[4]<-"TRAPS"

#rename the data frame and combine the number of traps we counted into it from HVAR3
HVAR_summary<-HVAR2
HVAR_summary$TRAPS<-HVAR3$TRAPS

#create a new variable to account for trapping effort in a given year
HVAR_summary$pertrap<-HVAR_summary$ADULTS/HVAR_summary$TRAPS




####Creating a new treatment classification 

HVAR_summary$TREAT_CAT=ifelse(HVAR_summary$TREAT_DESC=="Coniferous"|
                                HVAR_summary$TREAT_DESC=="Deciduous"|
                                HVAR_summary$TREAT_DESC=="Successional", "Forest", 
                              ifelse(HVAR_summary$TREAT_DESC=="Alfalfa*"|
                                       HVAR_summary$TREAT_DESC=="Early successional"|
                                       HVAR_summary$TREAT_DESC=="Poplar trees", "Perennial",
                                     ifelse(HVAR_summary$TREAT_DESC=="Conventional"|
                                              HVAR_summary$TREAT_DESC=="No till"|
                                              HVAR_summary$TREAT_DESC=="Organic"|
                                              HVAR_summary$TREAT_DESC=="Reduced input", "Annual", "Check")))




# First make a dataframe for error bars
HVAR_agg <- ddply(HVAR_summary, .(TREAT_CAT, year), summarise,
                  mean.bugs = mean(pertrap),
                  sd.bugs.pt = sd(pertrap),
                  N = length(pertrap),
                  SE = sd.bugs.pt / sqrt(N))



######################################################################################

#HPARN

#bring data in from 
HPARN<-read.csv("data/HPARN2020comb.csv",
                header=T)


#details of cleaning in the code in comments found at that link- in summary, get all the typoes
#out and make the date column usable


library(lubridate)
colnames(HPARN)[1]="DATE"
colnames(HPARN)[4]="REPLICATE"
HPARN$newdate<-mdy(HPARN$DATE)
HPARN$year<-year(HPARN$newdate)
HPARN$DOY<-yday(HPARN$newdate)
HPARN<-na.omit(HPARN)
HPARN$TREAT_DESC<-gsub("Early succesional community", "Early successional", HPARN$TREAT_DESC)
HPARN$TREAT_DESC<-gsub("Early successional community", "Early successional", HPARN$TREAT_DESC)
HPARN$TREAT_DESC<-gsub("Early Successional Community", "Early successional", HPARN$TREAT_DESC)
HPARN$TREAT_DESC<-gsub("Early sucessional community", "Early successional", HPARN$TREAT_DESC)
HPARN$TREAT_DESC<-gsub("poplar trees", "Poplar trees", HPARN$TREAT_DESC)
HPARN$TREAT_DESC<-gsub("Succesional", "Successional", HPARN$TREAT_DESC)
HPARN$TREAT_DESC<-gsub("Sucessional", "Successional", HPARN$TREAT_DESC)
HPARN$TREAT_DESC<-gsub("Alfalfa", "Alfalfa*", HPARN$TREAT_DESC)
HPARN$TREAT_DESC<-gsub("Switchgrass", "Alfalfa*", HPARN$TREAT_DESC)
HPARN$TREAT_DESC<-gsub("Biologically based \\(organic\\)", "Organic", HPARN$TREAT_DESC)
HPARN$TREAT_DESC<-gsub("Conventional till", "Conventional", HPARN$TREAT_DESC)
HPARN$TREAT_DESC<-as.factor(HPARN$TREAT_DESC)
HPARN$HABITAT<-as.factor(HPARN$HABITAT)
HPARN$REPLICATE<-as.factor(HPARN$REPLICATE)
HPARN$STATION<-as.factor(HPARN$STATION)
str(HPARN)

###########
#subset the data to include only data before August 10th or the 222 DOY
HPARN= subset(HPARN, DOY > 0 & DOY < 222)
#subset the data to include  1993 or later, because sampling changed to add forests then
HPARN= subset(HPARN, year >= 1993)

library(reshape2)
#tell R where the data is by melting it, assigning IDs to the columns
HPARN1<-melt(HPARN, id=c("DATE","TREAT_DESC","HABITAT","REPLICATE","STATION","newdate", "year", "DOY"))
#cast the data to count up the fireflies
HPARN2<-dcast(HPARN1, year+TREAT_DESC+REPLICATE~., sum)
#cast the data to count the traps
HPARN3<-dcast(HPARN1, year+TREAT_DESC+REPLICATE~., length)
#let's rename these new vectors within the data frame
names(HPARN2)[4]<-"ADULTS"
names(HPARN3)[4]<-"TRAPS"

#rename the data frame and combine the number of traps we counted into it from HPARN3
HPARN_summary<-HPARN2
HPARN_summary$TRAPS<-HPARN3$TRAPS

#create a new variable to account for trapping effort in a given year
HPARN_summary$pertrap<-HPARN_summary$ADULTS/HPARN_summary$TRAPS




####Creating a new treatment classification 

HPARN_summary$TREAT_CAT=ifelse(HPARN_summary$TREAT_DESC=="Coniferous"|
                                 HPARN_summary$TREAT_DESC=="Deciduous"|
                                 HPARN_summary$TREAT_DESC=="Successional", "Forest", 
                               ifelse(HPARN_summary$TREAT_DESC=="Alfalfa*"|
                                        HPARN_summary$TREAT_DESC=="Early successional"|
                                        HPARN_summary$TREAT_DESC=="Poplar trees", "Perennial",
                                      ifelse(HPARN_summary$TREAT_DESC=="Conventional"|
                                               HPARN_summary$TREAT_DESC=="No till"|
                                               HPARN_summary$TREAT_DESC=="Organic"|
                                               HPARN_summary$TREAT_DESC=="Reduced input", "Annual", "Check")))




# First make a dataframe for error bars
HPARN_agg <- ddply(HPARN_summary, .(TREAT_CAT, year), summarise,
                   mean.bugs = mean(pertrap),
                   sd.bugs.pt = sd(pertrap),
                   N = length(pertrap),
                   SE = sd.bugs.pt / sqrt(N))




######################################################################################

#HGLAC

#bring data in from 
HGLAC<-read.csv("data/HGLAC2020comb.csv",
                header=T)


#details of cleaning in the code in comments found at that link- in summary, get all the typoes
#out and make the date column usable


library(lubridate)
colnames(HGLAC)[1]="DATE"
colnames(HGLAC)[4]="REPLICATE"
HGLAC$newdate<-mdy(HGLAC$DATE)
HGLAC$year<-year(HGLAC$newdate)
HGLAC$DOY<-yday(HGLAC$newdate)
HGLAC<-na.omit(HGLAC)
HGLAC$TREAT_DESC<-gsub("Early succesional community", "Early successional", HGLAC$TREAT_DESC)
HGLAC$TREAT_DESC<-gsub("Early successional community", "Early successional", HGLAC$TREAT_DESC)
HGLAC$TREAT_DESC<-gsub("Early Successional Community", "Early successional", HGLAC$TREAT_DESC)
HGLAC$TREAT_DESC<-gsub("Early sucessional community", "Early successional", HGLAC$TREAT_DESC)
HGLAC$TREAT_DESC<-gsub("poplar trees", "Poplar trees", HGLAC$TREAT_DESC)
HGLAC$TREAT_DESC<-gsub("Succesional", "Successional", HGLAC$TREAT_DESC)
HGLAC$TREAT_DESC<-gsub("Sucessional", "Successional", HGLAC$TREAT_DESC)
HGLAC$TREAT_DESC<-gsub("Alfalfa", "Alfalfa*", HGLAC$TREAT_DESC)
HGLAC$TREAT_DESC<-gsub("Switchgrass", "Alfalfa*", HGLAC$TREAT_DESC)
HGLAC$TREAT_DESC<-gsub("Biologically based \\(organic\\)", "Organic", HGLAC$TREAT_DESC)
HGLAC$TREAT_DESC<-gsub("Conventional till", "Conventional", HGLAC$TREAT_DESC)
HGLAC$TREAT_DESC<-as.factor(HGLAC$TREAT_DESC)
HGLAC$HABITAT<-as.factor(HGLAC$HABITAT)
HGLAC$REPLICATE<-as.factor(HGLAC$REPLICATE)
HGLAC$STATION<-as.factor(HGLAC$STATION)
str(HGLAC)

###########
#subset the data to include only data before August 10th or the 222 DOY
HGLAC= subset(HGLAC, DOY > 0 & DOY < 222)
#subset the data to include  1993 or later, because sampling changed to add forests then
HGLAC= subset(HGLAC, year >= 1993)

library(reshape2)
#tell R where the data is by melting it, assigning IDs to the columns
HGLAC1<-melt(HGLAC, id=c("DATE","TREAT_DESC","HABITAT","REPLICATE","STATION","newdate", "year", "DOY"))
#cast the data to count up the fireflies
HGLAC2<-dcast(HGLAC1, year+TREAT_DESC+REPLICATE~., sum)
#cast the data to count the traps
HGLAC3<-dcast(HGLAC1, year+TREAT_DESC+REPLICATE~., length)
#let's rename these new vectors within the data frame
names(HGLAC2)[4]<-"ADULTS"
names(HGLAC3)[4]<-"TRAPS"

#rename the data frame and combine the number of traps we counted into it from HGLAC3
HGLAC_summary<-HGLAC2
HGLAC_summary$TRAPS<-HGLAC3$TRAPS

#create a new variable to account for trapping effort in a given year
HGLAC_summary$pertrap<-HGLAC_summary$ADULTS/HGLAC_summary$TRAPS




####Creating a new treatment classification 

HGLAC_summary$TREAT_CAT=ifelse(HGLAC_summary$TREAT_DESC=="Coniferous"|
                                 HGLAC_summary$TREAT_DESC=="Deciduous"|
                                 HGLAC_summary$TREAT_DESC=="Successional", "Forest", 
                               ifelse(HGLAC_summary$TREAT_DESC=="Alfalfa*"|
                                        HGLAC_summary$TREAT_DESC=="Early successional"|
                                        HGLAC_summary$TREAT_DESC=="Poplar trees", "Perennial",
                                      ifelse(HGLAC_summary$TREAT_DESC=="Conventional"|
                                               HGLAC_summary$TREAT_DESC=="No till"|
                                               HGLAC_summary$TREAT_DESC=="Organic"|
                                               HGLAC_summary$TREAT_DESC=="Reduced input", "Annual", "Check")))




# First make a dataframe for error bars
HGLAC_agg <- ddply(HGLAC_summary, .(TREAT_CAT, year), summarise,
                   mean.bugs = mean(pertrap),
                   sd.bugs.pt = sd(pertrap),
                   N = length(pertrap),
                   SE = sd.bugs.pt / sqrt(N))




######################################################################################

#HCONV

#bring data in from 
HCONV<-read.csv("data/HCONV2020comb.csv",
                header=T)


#details of cleaning in the code in comments found at that link- in summary, get all the typoes
#out and make the date column usable


library(lubridate)
colnames(HCONV)[1]="DATE"
colnames(HCONV)[4]="REPLICATE"
HCONV$newdate<-mdy(HCONV$DATE)
HCONV$year<-year(HCONV$newdate)
HCONV$DOY<-yday(HCONV$newdate)
HCONV<-na.omit(HCONV)
HCONV$TREAT_DESC<-gsub("Early succesional community", "Early successional", HCONV$TREAT_DESC)
HCONV$TREAT_DESC<-gsub("Early successional community", "Early successional", HCONV$TREAT_DESC)
HCONV$TREAT_DESC<-gsub("Early Successional Community", "Early successional", HCONV$TREAT_DESC)
HCONV$TREAT_DESC<-gsub("Early sucessional community", "Early successional", HCONV$TREAT_DESC)
HCONV$TREAT_DESC<-gsub("poplar trees", "Poplar trees", HCONV$TREAT_DESC)
HCONV$TREAT_DESC<-gsub("Succesional", "Successional", HCONV$TREAT_DESC)
HCONV$TREAT_DESC<-gsub("Sucessional", "Successional", HCONV$TREAT_DESC)
HCONV$TREAT_DESC<-gsub("Alfalfa", "Alfalfa*", HCONV$TREAT_DESC)
HCONV$TREAT_DESC<-gsub("Switchgrass", "Alfalfa*", HCONV$TREAT_DESC)
HCONV$TREAT_DESC<-gsub("Biologically based \\(organic\\)", "Organic", HCONV$TREAT_DESC)
HCONV$TREAT_DESC<-gsub("Conventional till", "Conventional", HCONV$TREAT_DESC)
HCONV$TREAT_DESC<-as.factor(HCONV$TREAT_DESC)
HCONV$HABITAT<-as.factor(HCONV$HABITAT)
HCONV$REPLICATE<-as.factor(HCONV$REPLICATE)
HCONV$STATION<-as.factor(HCONV$STATION)
str(HCONV)

###########
#subset the data to include only data before August 10th or the 222 DOY
HCONV= subset(HCONV, DOY > 0 & DOY < 222)
#subset the data to include  1993 or later, because sampling changed to add forests then
HCONV= subset(HCONV, year >= 1993)

library(reshape2)
#tell R where the data is by melting it, assigning IDs to the columns
HCONV1<-melt(HCONV, id=c("DATE","TREAT_DESC","HABITAT","REPLICATE","STATION","newdate", "year", "DOY"))
#cast the data to count up the fireflies
HCONV2<-dcast(HCONV1, year+TREAT_DESC+REPLICATE~., sum)
#cast the data to count the traps
HCONV3<-dcast(HCONV1, year+TREAT_DESC+REPLICATE~., length)
#let's rename these new vectors within the data frame
names(HCONV2)[4]<-"ADULTS"
names(HCONV3)[4]<-"TRAPS"

#rename the data frame and combine the number of traps we counted into it from HCONV3
HCONV_summary<-HCONV2
HCONV_summary$TRAPS<-HCONV3$TRAPS

#create a new variable to account for trapping effort in a given year
HCONV_summary$pertrap<-HCONV_summary$ADULTS/HCONV_summary$TRAPS




####Creating a new treatment classification 

HCONV_summary$TREAT_CAT=ifelse(HCONV_summary$TREAT_DESC=="Coniferous"|
                                 HCONV_summary$TREAT_DESC=="Deciduous"|
                                 HCONV_summary$TREAT_DESC=="Successional", "Forest", 
                               ifelse(HCONV_summary$TREAT_DESC=="Alfalfa*"|
                                        HCONV_summary$TREAT_DESC=="Early successional"|
                                        HCONV_summary$TREAT_DESC=="Poplar trees", "Perennial",
                                      ifelse(HCONV_summary$TREAT_DESC=="Conventional"|
                                               HCONV_summary$TREAT_DESC=="No till"|
                                               HCONV_summary$TREAT_DESC=="Organic"|
                                               HCONV_summary$TREAT_DESC=="Reduced input", "Annual", "Check")))




# First make a dataframe for error bars
HCONV_agg <- ddply(HCONV_summary, .(TREAT_CAT, year), summarise,
                   mean.bugs = mean(pertrap),
                   sd.bugs.pt = sd(pertrap),
                   N = length(pertrap),
                   SE = sd.bugs.pt / sqrt(N))




######################################################################################

#HAXY

#bring data in from 
HAXY<-read.csv("data/HAXY2020comb.csv",
               header=T)


#details of cleaning in the code in comments found at that link- in summary, get all the typoes
#out and make the date column usable


library(lubridate)
colnames(HAXY)[1]="DATE"
colnames(HAXY)[4]="REPLICATE"
HAXY$newdate<-mdy(HAXY$DATE)
HAXY$year<-year(HAXY$newdate)
HAXY$DOY<-yday(HAXY$newdate)
HAXY<-na.omit(HAXY)
HAXY$TREAT_DESC<-gsub("Early succesional community", "Early successional", HAXY$TREAT_DESC)
HAXY$TREAT_DESC<-gsub("Early successional community", "Early successional", HAXY$TREAT_DESC)
HAXY$TREAT_DESC<-gsub("Early Successional Community", "Early successional", HAXY$TREAT_DESC)
HAXY$TREAT_DESC<-gsub("Early sucessional community", "Early successional", HAXY$TREAT_DESC)
HAXY$TREAT_DESC<-gsub("poplar trees", "Poplar trees", HAXY$TREAT_DESC)
HAXY$TREAT_DESC<-gsub("Succesional", "Successional", HAXY$TREAT_DESC)
HAXY$TREAT_DESC<-gsub("Sucessional", "Successional", HAXY$TREAT_DESC)
HAXY$TREAT_DESC<-gsub("Alfalfa", "Alfalfa*", HAXY$TREAT_DESC)
HAXY$TREAT_DESC<-gsub("Switchgrass", "Alfalfa*", HAXY$TREAT_DESC)
HAXY$TREAT_DESC<-gsub("Biologically based \\(organic\\)", "Organic", HAXY$TREAT_DESC)
HAXY$TREAT_DESC<-gsub("Conventional till", "Conventional", HAXY$TREAT_DESC)
HAXY$TREAT_DESC<-as.factor(HAXY$TREAT_DESC)
HAXY$HABITAT<-as.factor(HAXY$HABITAT)
HAXY$REPLICATE<-as.factor(HAXY$REPLICATE)
HAXY$STATION<-as.factor(HAXY$STATION)
str(HAXY)

###########
#subset the data to include only data before August 10th or the 222 DOY
HAXY= subset(HAXY, DOY > 0 & DOY < 222)
#subset the data to include  1993 or later, because sampling changed to add forests then
HAXY= subset(HAXY, year >= 1993)

library(reshape2)
#tell R where the data is by melting it, assigning IDs to the columns
HAXY1<-melt(HAXY, id=c("DATE","TREAT_DESC","HABITAT","REPLICATE","STATION","newdate", "year", "DOY"))
#cast the data to count up the fireflies
HAXY2<-dcast(HAXY1, year+TREAT_DESC+REPLICATE~., sum)
#cast the data to count the traps
HAXY3<-dcast(HAXY1, year+TREAT_DESC+REPLICATE~., length)
#let's rename these new vectors within the data frame
names(HAXY2)[4]<-"ADULTS"
names(HAXY3)[4]<-"TRAPS"

#rename the data frame and combine the number of traps we counted into it from HAXY3
HAXY_summary<-HAXY2
HAXY_summary$TRAPS<-HAXY3$TRAPS

#create a new variable to account for trapping effort in a given year
HAXY_summary$pertrap<-HAXY_summary$ADULTS/HAXY_summary$TRAPS



####Creating a new treatment classification 

HAXY_summary$TREAT_CAT=ifelse(HAXY_summary$TREAT_DESC=="Coniferous"|
                                HAXY_summary$TREAT_DESC=="Deciduous"|
                                HAXY_summary$TREAT_DESC=="Successional", "Forest", 
                              ifelse(HAXY_summary$TREAT_DESC=="Alfalfa*"|
                                       HAXY_summary$TREAT_DESC=="Early successional"|
                                       HAXY_summary$TREAT_DESC=="Poplar trees", "Perennial",
                                     ifelse(HAXY_summary$TREAT_DESC=="Conventional"|
                                              HAXY_summary$TREAT_DESC=="No till"|
                                              HAXY_summary$TREAT_DESC=="Organic"|
                                              HAXY_summary$TREAT_DESC=="Reduced input", "Annual", "Check")))




# First make a dataframe for error bars
HAXY_agg <- ddply(HAXY_summary, .(TREAT_CAT, year), summarise,
                  mean.bugs = mean(pertrap),
                  sd.bugs.pt = sd(pertrap),
                  N = length(pertrap),
                  SE = sd.bugs.pt / sqrt(N))



######################################################################################

#H13

#bring data in from 
H13<-read.csv("data/H132020comb.csv",
              header=T)


#details of cleaning in the code in comments found at that link- in summary, get all the typoes
#out and make the date column usable


library(lubridate)
colnames(H13)[1]="DATE"
colnames(H13)[4]="REPLICATE"
H13$newdate<-mdy(H13$DATE)
H13$year<-year(H13$newdate)
H13$DOY<-yday(H13$newdate)
H13<-na.omit(H13)
H13$TREAT_DESC<-gsub("Early succesional community", "Early successional", H13$TREAT_DESC)
H13$TREAT_DESC<-gsub("Early successional community", "Early successional", H13$TREAT_DESC)
H13$TREAT_DESC<-gsub("Early Successional Community", "Early successional", H13$TREAT_DESC)
H13$TREAT_DESC<-gsub("Early sucessional community", "Early successional", H13$TREAT_DESC)
H13$TREAT_DESC<-gsub("poplar trees", "Poplar trees", H13$TREAT_DESC)
H13$TREAT_DESC<-gsub("Succesional", "Successional", H13$TREAT_DESC)
H13$TREAT_DESC<-gsub("Sucessional", "Successional", H13$TREAT_DESC)
H13$TREAT_DESC<-gsub("Alfalfa", "Alfalfa*", H13$TREAT_DESC)
H13$TREAT_DESC<-gsub("Switchgrass", "Alfalfa*", H13$TREAT_DESC)
H13$TREAT_DESC<-gsub("Biologically based \\(organic\\)", "Organic", H13$TREAT_DESC)
H13$TREAT_DESC<-gsub("Conventional till", "Conventional", H13$TREAT_DESC)
H13$TREAT_DESC<-as.factor(H13$TREAT_DESC)
H13$HABITAT<-as.factor(H13$HABITAT)
H13$REPLICATE<-as.factor(H13$REPLICATE)
H13$STATION<-as.factor(H13$STATION)
str(H13)

###########
#subset the data to include only data before August 10th or the 222 DOY
H13= subset(H13, DOY > 0 & DOY < 222)
#subset the data to include  1993 or later, because sampling changed to add forests then
H13= subset(H13, year >= 1993)

library(reshape2)
#tell R where the data is by melting it, assigning IDs to the columns
H131<-melt(H13, id=c("DATE","TREAT_DESC","HABITAT","REPLICATE","STATION","newdate", "year", "DOY"))
#cast the data to count up the fireflies
H132<-dcast(H131, year+TREAT_DESC+REPLICATE~., sum)
#cast the data to count the traps
H133<-dcast(H131, year+TREAT_DESC+REPLICATE~., length)
#let's rename these new vectors within the data frame
names(H132)[4]<-"ADULTS"
names(H133)[4]<-"TRAPS"

#rename the data frame and combine the number of traps we counted into it from H133
H13_summary<-H132
H13_summary$TRAPS<-H133$TRAPS

#create a new variable to account for trapping effort in a given year
H13_summary$pertrap<-H13_summary$ADULTS/H13_summary$TRAPS



####Creating a new treatment classification 

H13_summary$TREAT_CAT=ifelse(H13_summary$TREAT_DESC=="Coniferous"|
                               H13_summary$TREAT_DESC=="Deciduous"|
                               H13_summary$TREAT_DESC=="Successional", "Forest", 
                             ifelse(H13_summary$TREAT_DESC=="Alfalfa*"|
                                      H13_summary$TREAT_DESC=="Early successional"|
                                      H13_summary$TREAT_DESC=="Poplar trees", "Perennial",
                                    ifelse(H13_summary$TREAT_DESC=="Conventional"|
                                             H13_summary$TREAT_DESC=="No till"|
                                             H13_summary$TREAT_DESC=="Organic"|
                                             H13_summary$TREAT_DESC=="Reduced input", "Annual", "Check")))




# First make a dataframe for error bars
H13_agg <- ddply(H13_summary, .(TREAT_CAT, year), summarise,
                 mean.bugs = mean(pertrap),
                 sd.bugs.pt = sd(pertrap),
                 N = length(pertrap),
                 SE = sd.bugs.pt / sqrt(N))


######################################################################################

#CYCSP

#bring data in from 
CYCSP<-read.csv("data/CYCSP2020comb.csv",
                header=T)


#details of cleaning in the code in comments found at that link- in summary, get all the typoes
#out and make the date column usable


library(lubridate)
colnames(CYCSP)[1]="DATE"
colnames(CYCSP)[4]="REPLICATE"
CYCSP$newdate<-mdy(CYCSP$DATE)
CYCSP$year<-year(CYCSP$newdate)
CYCSP$DOY<-yday(CYCSP$newdate)
CYCSP<-na.omit(CYCSP)
CYCSP$TREAT_DESC<-gsub("Early succesional community", "Early successional", CYCSP$TREAT_DESC)
CYCSP$TREAT_DESC<-gsub("Early successional community", "Early successional", CYCSP$TREAT_DESC)
CYCSP$TREAT_DESC<-gsub("Early Successional Community", "Early successional", CYCSP$TREAT_DESC)
CYCSP$TREAT_DESC<-gsub("Early sucessional community", "Early successional", CYCSP$TREAT_DESC)
CYCSP$TREAT_DESC<-gsub("poplar trees", "Poplar trees", CYCSP$TREAT_DESC)
CYCSP$TREAT_DESC<-gsub("Succesional", "Successional", CYCSP$TREAT_DESC)
CYCSP$TREAT_DESC<-gsub("Sucessional", "Successional", CYCSP$TREAT_DESC)
CYCSP$TREAT_DESC<-gsub("Alfalfa", "Alfalfa*", CYCSP$TREAT_DESC)
CYCSP$TREAT_DESC<-gsub("Switchgrass", "Alfalfa*", CYCSP$TREAT_DESC)
CYCSP$TREAT_DESC<-gsub("Biologically based \\(organic\\)", "Organic", CYCSP$TREAT_DESC)
CYCSP$TREAT_DESC<-gsub("Conventional till", "Conventional", CYCSP$TREAT_DESC)
CYCSP$TREAT_DESC<-as.factor(CYCSP$TREAT_DESC)
CYCSP$HABITAT<-as.factor(CYCSP$HABITAT)
CYCSP$REPLICATE<-as.factor(CYCSP$REPLICATE)
CYCSP$STATION<-as.factor(CYCSP$STATION)
str(CYCSP)

###########
#subset the data to include only data before August 10th or the 222 DOY
CYCSP= subset(CYCSP, DOY > 0 & DOY < 222)
#subset the data to include  1993 or later, because sampling changed to add forests then
CYCSP= subset(CYCSP, year >= 1993)

library(reshape2)
#tell R where the data is by melting it, assigning IDs to the columns
CYCSP1<-melt(CYCSP, id=c("DATE","TREAT_DESC","HABITAT","REPLICATE","STATION","newdate", "year", "DOY"))
#cast the data to count up the fireflies
CYCSP2<-dcast(CYCSP1, year+TREAT_DESC+REPLICATE~., sum)
#cast the data to count the traps
CYCSP3<-dcast(CYCSP1, year+TREAT_DESC+REPLICATE~., length)
#let's rename these new vectors within the data frame
names(CYCSP2)[4]<-"ADULTS"
names(CYCSP3)[4]<-"TRAPS"

#rename the data frame and combine the number of traps we counted into it from CYCSP3
CYCSP_summary<-CYCSP2
CYCSP_summary$TRAPS<-CYCSP3$TRAPS

#create a new variable to account for trapping effort in a given year
CYCSP_summary$pertrap<-CYCSP_summary$ADULTS/CYCSP_summary$TRAPS




####Creating a new treatment classification 

CYCSP_summary$TREAT_CAT=ifelse(CYCSP_summary$TREAT_DESC=="Coniferous"|
                                 CYCSP_summary$TREAT_DESC=="Deciduous"|
                                 CYCSP_summary$TREAT_DESC=="Successional", "Forest", 
                               ifelse(CYCSP_summary$TREAT_DESC=="Alfalfa*"|
                                        CYCSP_summary$TREAT_DESC=="Early successional"|
                                        CYCSP_summary$TREAT_DESC=="Poplar trees", "Perennial",
                                      ifelse(CYCSP_summary$TREAT_DESC=="Conventional"|
                                               CYCSP_summary$TREAT_DESC=="No till"|
                                               CYCSP_summary$TREAT_DESC=="Organic"|
                                               CYCSP_summary$TREAT_DESC=="Reduced input", "Annual", "Check")))




# First make a dataframe for error bars
CYCSP_agg <- ddply(CYCSP_summary, .(TREAT_CAT, year), summarise,
                   mean.bugs = mean(pertrap),
                   sd.bugs.pt = sd(pertrap),
                   N = length(pertrap),
                   SE = sd.bugs.pt / sqrt(N))



######################################################################################

#CTRIF

#bring data in from 
CTRIF<-read.csv("data/CTRIF2020comb.csv",
                header=T)


#details of cleaning in the code in comments found at that link- in summary, get all the typoes
#out and make the date column usable


library(lubridate)
colnames(CTRIF)[1]="DATE"
colnames(CTRIF)[4]="REPLICATE"
CTRIF$newdate<-mdy(CTRIF$DATE)
CTRIF$year<-year(CTRIF$newdate)
CTRIF$DOY<-yday(CTRIF$newdate)
CTRIF<-na.omit(CTRIF)
CTRIF$TREAT_DESC<-gsub("Early succesional community", "Early successional", CTRIF$TREAT_DESC)
CTRIF$TREAT_DESC<-gsub("Early successional community", "Early successional", CTRIF$TREAT_DESC)
CTRIF$TREAT_DESC<-gsub("Early Successional Community", "Early successional", CTRIF$TREAT_DESC)
CTRIF$TREAT_DESC<-gsub("Early sucessional community", "Early successional", CTRIF$TREAT_DESC)
CTRIF$TREAT_DESC<-gsub("poplar trees", "Poplar trees", CTRIF$TREAT_DESC)
CTRIF$TREAT_DESC<-gsub("Succesional", "Successional", CTRIF$TREAT_DESC)
CTRIF$TREAT_DESC<-gsub("Sucessional", "Successional", CTRIF$TREAT_DESC)
CTRIF$TREAT_DESC<-gsub("Alfalfa", "Alfalfa*", CTRIF$TREAT_DESC)
CTRIF$TREAT_DESC<-gsub("Switchgrass", "Alfalfa*", CTRIF$TREAT_DESC)
CTRIF$TREAT_DESC<-gsub("Biologically based \\(organic\\)", "Organic", CTRIF$TREAT_DESC)
CTRIF$TREAT_DESC<-gsub("Conventional till", "Conventional", CTRIF$TREAT_DESC)
CTRIF$TREAT_DESC<-as.factor(CTRIF$TREAT_DESC)
CTRIF$HABITAT<-as.factor(CTRIF$HABITAT)
CTRIF$REPLICATE<-as.factor(CTRIF$REPLICATE)
CTRIF$STATION<-as.factor(CTRIF$STATION)
str(CTRIF)

###########
#subset the data to include only data before August 10th or the 222 DOY
CTRIF= subset(CTRIF, DOY > 0 & DOY < 222)
#subset the data to include  1993 or later, because sampling changed to add forests then
CTRIF= subset(CTRIF, year >= 1993)

library(reshape2)
#tell R where the data is by melting it, assigning IDs to the columns
CTRIF1<-melt(CTRIF, id=c("DATE","TREAT_DESC","HABITAT","REPLICATE","STATION","newdate", "year", "DOY"))
#cast the data to count up the fireflies
CTRIF2<-dcast(CTRIF1, year+TREAT_DESC+REPLICATE~., sum)
#cast the data to count the traps
CTRIF3<-dcast(CTRIF1, year+TREAT_DESC+REPLICATE~., length)
#let's rename these new vectors within the data frame
names(CTRIF2)[4]<-"ADULTS"
names(CTRIF3)[4]<-"TRAPS"

#rename the data frame and combine the number of traps we counted into it from CTRIF3
CTRIF_summary<-CTRIF2
CTRIF_summary$TRAPS<-CTRIF3$TRAPS

#create a new variable to account for trapping effort in a given year
CTRIF_summary$pertrap<-CTRIF_summary$ADULTS/CTRIF_summary$TRAPS




####Creating a new treatment classification 

CTRIF_summary$TREAT_CAT=ifelse(CTRIF_summary$TREAT_DESC=="Coniferous"|
                                 CTRIF_summary$TREAT_DESC=="Deciduous"|
                                 CTRIF_summary$TREAT_DESC=="Successional", "Forest", 
                               ifelse(CTRIF_summary$TREAT_DESC=="Alfalfa*"|
                                        CTRIF_summary$TREAT_DESC=="Early successional"|
                                        CTRIF_summary$TREAT_DESC=="Poplar trees", "Perennial",
                                      ifelse(CTRIF_summary$TREAT_DESC=="Conventional"|
                                               CTRIF_summary$TREAT_DESC=="No till"|
                                               CTRIF_summary$TREAT_DESC=="Organic"|
                                               CTRIF_summary$TREAT_DESC=="Reduced input", "Annual", "Check")))




# First make a dataframe for error bars
CTRIF_agg <- ddply(CTRIF_summary, .(TREAT_CAT, year), summarise,
                   mean.bugs = mean(pertrap),
                   sd.bugs.pt = sd(pertrap),
                   N = length(pertrap),
                   SE = sd.bugs.pt / sqrt(N))




######################################################################################

#CSTIG

#bring data in from 
CSTIG<-read.csv("data/CSTIG2020comb.csv",
                header=T)


#details of cleaning in the code in comments found at that link- in summary, get all the typoes
#out and make the date column usable


library(lubridate)
colnames(CSTIG)[1]="DATE"
colnames(CSTIG)[4]="REPLICATE"
CSTIG$newdate<-mdy(CSTIG$DATE)
CSTIG$year<-year(CSTIG$newdate)
CSTIG$DOY<-yday(CSTIG$newdate)
CSTIG<-na.omit(CSTIG)
CSTIG$TREAT_DESC<-gsub("Early succesional community", "Early successional", CSTIG$TREAT_DESC)
CSTIG$TREAT_DESC<-gsub("Early successional community", "Early successional", CSTIG$TREAT_DESC)
CSTIG$TREAT_DESC<-gsub("Early Successional Community", "Early successional", CSTIG$TREAT_DESC)
CSTIG$TREAT_DESC<-gsub("Early sucessional community", "Early successional", CSTIG$TREAT_DESC)
CSTIG$TREAT_DESC<-gsub("poplar trees", "Poplar trees", CSTIG$TREAT_DESC)
CSTIG$TREAT_DESC<-gsub("Succesional", "Successional", CSTIG$TREAT_DESC)
CSTIG$TREAT_DESC<-gsub("Sucessional", "Successional", CSTIG$TREAT_DESC)
CSTIG$TREAT_DESC<-gsub("Alfalfa", "Alfalfa*", CSTIG$TREAT_DESC)
CSTIG$TREAT_DESC<-gsub("Switchgrass", "Alfalfa*", CSTIG$TREAT_DESC)
CSTIG$TREAT_DESC<-gsub("Biologically based \\(organic\\)", "Organic", CSTIG$TREAT_DESC)
CSTIG$TREAT_DESC<-gsub("Conventional till", "Conventional", CSTIG$TREAT_DESC)
CSTIG$TREAT_DESC<-as.factor(CSTIG$TREAT_DESC)
CSTIG$HABITAT<-as.factor(CSTIG$HABITAT)
CSTIG$REPLICATE<-as.factor(CSTIG$REPLICATE)
CSTIG$STATION<-as.factor(CSTIG$STATION)
str(CSTIG)

###########
#subset the data to include only data before August 10th or the 222 DOY
CSTIG= subset(CSTIG, DOY > 0 & DOY < 222)
#subset the data to include  1993 or later, because sampling changed to add forests then
CSTIG= subset(CSTIG, year >= 1993)

library(reshape2)
#tell R where the data is by melting it, assigning IDs to the columns
CSTIG1<-melt(CSTIG, id=c("DATE","TREAT_DESC","HABITAT","REPLICATE","STATION","newdate", "year", "DOY"))
#cast the data to count up the fireflies
CSTIG2<-dcast(CSTIG1, year+TREAT_DESC+REPLICATE~., sum)
#cast the data to count the traps
CSTIG3<-dcast(CSTIG1, year+TREAT_DESC+REPLICATE~., length)
#let's rename these new vectors within the data frame
names(CSTIG2)[4]<-"ADULTS"
names(CSTIG3)[4]<-"TRAPS"

#rename the data frame and combine the number of traps we counted into it from CSTIG3
CSTIG_summary<-CSTIG2
CSTIG_summary$TRAPS<-CSTIG3$TRAPS

#create a new variable to account for trapping effort in a given year
CSTIG_summary$pertrap<-CSTIG_summary$ADULTS/CSTIG_summary$TRAPS



####Creating a new treatment classification 

CSTIG_summary$TREAT_CAT=ifelse(CSTIG_summary$TREAT_DESC=="Coniferous"|
                                 CSTIG_summary$TREAT_DESC=="Deciduous"|
                                 CSTIG_summary$TREAT_DESC=="Successional", "Forest", 
                               ifelse(CSTIG_summary$TREAT_DESC=="Alfalfa*"|
                                        CSTIG_summary$TREAT_DESC=="Early successional"|
                                        CSTIG_summary$TREAT_DESC=="Poplar trees", "Perennial",
                                      ifelse(CSTIG_summary$TREAT_DESC=="Conventional"|
                                               CSTIG_summary$TREAT_DESC=="No till"|
                                               CSTIG_summary$TREAT_DESC=="Organic"|
                                               CSTIG_summary$TREAT_DESC=="Reduced input", "Annual", "Check")))






######################################################################################

#CMAC

#bring data in from 
CMAC<-read.csv("data/CMAC2020comb.csv",
               header=T)


#details of cleaning in the code in comments found at that link- in summary, get all the typoes
#out and make the date column usable


library(lubridate)
colnames(CMAC)[1]="DATE"
colnames(CMAC)[4]="REPLICATE"
CMAC$newdate<-mdy(CMAC$DATE)
CMAC$year<-year(CMAC$newdate)
CMAC$DOY<-yday(CMAC$newdate)
CMAC<-na.omit(CMAC)
CMAC$TREAT_DESC<-gsub("Early succesional community", "Early successional", CMAC$TREAT_DESC)
CMAC$TREAT_DESC<-gsub("Early successional community", "Early successional", CMAC$TREAT_DESC)
CMAC$TREAT_DESC<-gsub("Early Successional Community", "Early successional", CMAC$TREAT_DESC)
CMAC$TREAT_DESC<-gsub("Early sucessional community", "Early successional", CMAC$TREAT_DESC)
CMAC$TREAT_DESC<-gsub("poplar trees", "Poplar trees", CMAC$TREAT_DESC)
CMAC$TREAT_DESC<-gsub("Succesional", "Successional", CMAC$TREAT_DESC)
CMAC$TREAT_DESC<-gsub("Sucessional", "Successional", CMAC$TREAT_DESC)
CMAC$TREAT_DESC<-gsub("Alfalfa", "Alfalfa*", CMAC$TREAT_DESC)
CMAC$TREAT_DESC<-gsub("Switchgrass", "Alfalfa*", CMAC$TREAT_DESC)
CMAC$TREAT_DESC<-gsub("Biologically based \\(organic\\)", "Organic", CMAC$TREAT_DESC)
CMAC$TREAT_DESC<-gsub("Conventional till", "Conventional", CMAC$TREAT_DESC)
CMAC$TREAT_DESC<-as.factor(CMAC$TREAT_DESC)
CMAC$HABITAT<-as.factor(CMAC$HABITAT)
CMAC$REPLICATE<-as.factor(CMAC$REPLICATE)
CMAC$STATION<-as.factor(CMAC$STATION)
str(CMAC)

###########
#subset the data to include only data before August 10th or the 222 DOY
CMAC= subset(CMAC, DOY > 0 & DOY < 222)
#subset the data to include  1993 or later, because sampling changed to add forests then
CMAC= subset(CMAC, year >= 1993)

library(reshape2)
#tell R where the data is by melting it, assigning IDs to the columns
CMAC1<-melt(CMAC, id=c("DATE","TREAT_DESC","HABITAT","REPLICATE","STATION","newdate", "year", "DOY"))
#cast the data to count up the fireflies
CMAC2<-dcast(CMAC1, year+TREAT_DESC+REPLICATE~., sum)
#cast the data to count the traps
CMAC3<-dcast(CMAC1, year+TREAT_DESC+REPLICATE~., length)
#let's rename these new vectors within the data frame
names(CMAC2)[4]<-"ADULTS"
names(CMAC3)[4]<-"TRAPS"

#rename the data frame and combine the number of traps we counted into it from CMAC3
CMAC_summary<-CMAC2
CMAC_summary$TRAPS<-CMAC3$TRAPS

#create a new variable to account for trapping effort in a given year
CMAC_summary$pertrap<-CMAC_summary$ADULTS/CMAC_summary$TRAPS




####Creating a new treatment classification 

CMAC_summary$TREAT_CAT=ifelse(CMAC_summary$TREAT_DESC=="Coniferous"|
                                CMAC_summary$TREAT_DESC=="Deciduous"|
                                CMAC_summary$TREAT_DESC=="Successional", "Forest", 
                              ifelse(CMAC_summary$TREAT_DESC=="Alfalfa*"|
                                       CMAC_summary$TREAT_DESC=="Early successional"|
                                       CMAC_summary$TREAT_DESC=="Poplar trees", "Perennial",
                                     ifelse(CMAC_summary$TREAT_DESC=="Conventional"|
                                              CMAC_summary$TREAT_DESC=="No till"|
                                              CMAC_summary$TREAT_DESC=="Organic"|
                                              CMAC_summary$TREAT_DESC=="Reduced input", "Annual", "Check")))




# First make a dataframe for error bars
CMAC_agg <- ddply(CMAC_summary, .(TREAT_CAT, year), summarise,
                  mean.bugs = mean(pertrap),
                  sd.bugs.pt = sd(pertrap),
                  N = length(pertrap),
                  SE = sd.bugs.pt / sqrt(N))






######################################################################################

#C7

#bring data in from 
C7<-read.csv("data/C72020comb.csv",
             header=T)


#details of cleaning in the code in comments found at that link- in summary, get all the typoes
#out and make the date column usable


library(lubridate)
colnames(C7)[1]="DATE"
colnames(C7)[4]="REPLICATE"
C7$newdate<-mdy(C7$DATE)
C7$year<-year(C7$newdate)
C7$DOY<-yday(C7$newdate)
C7<-na.omit(C7)
C7$TREAT_DESC<-gsub("Early succesional community", "Early successional", C7$TREAT_DESC)
C7$TREAT_DESC<-gsub("Early successional community", "Early successional", C7$TREAT_DESC)
C7$TREAT_DESC<-gsub("Early Successional Community", "Early successional", C7$TREAT_DESC)
C7$TREAT_DESC<-gsub("Early sucessional community", "Early successional", C7$TREAT_DESC)
C7$TREAT_DESC<-gsub("poplar trees", "Poplar trees", C7$TREAT_DESC)
C7$TREAT_DESC<-gsub("Succesional", "Successional", C7$TREAT_DESC)
C7$TREAT_DESC<-gsub("Sucessional", "Successional", C7$TREAT_DESC)
C7$TREAT_DESC<-gsub("Alfalfa", "Alfalfa*", C7$TREAT_DESC)
C7$TREAT_DESC<-gsub("Switchgrass", "Alfalfa*", C7$TREAT_DESC)
C7$TREAT_DESC<-gsub("Biologically based \\(organic\\)", "Organic", C7$TREAT_DESC)
C7$TREAT_DESC<-gsub("Conventional till", "Conventional", C7$TREAT_DESC)
C7$TREAT_DESC<-as.factor(C7$TREAT_DESC)
C7$HABITAT<-as.factor(C7$HABITAT)
C7$REPLICATE<-as.factor(C7$REPLICATE)
C7$STATION<-as.factor(C7$STATION)
str(C7)

###########
#subset the data to include only data before August 10th or the 222 DOY
C7= subset(C7, DOY > 0 & DOY < 222)
#subset the data to include  1993 or later, because sampling changed to add forests then
C7= subset(C7, year >= 1993)

library(reshape2)
#tell R where the data is by melting it, assigning IDs to the columns
C71<-melt(C7, id=c("DATE","TREAT_DESC","HABITAT","REPLICATE","STATION","newdate", "year", "DOY"))
#cast the data to count up the fireflies
C72<-dcast(C71, year+TREAT_DESC+REPLICATE~., sum)
#cast the data to count the traps
C73<-dcast(C71, year+TREAT_DESC+REPLICATE~., length)
#let's rename these new vectors within the data frame
names(C72)[4]<-"ADULTS"
names(C73)[4]<-"TRAPS"

#rename the data frame and combine the number of traps we counted into it from C73
C7_summary<-C72
C7_summary$TRAPS<-C73$TRAPS

#create a new variable to account for trapping effort in a given year
C7_summary$pertrap<-C7_summary$ADULTS/C7_summary$TRAPS



####Creating a new treatment classification 

C7_summary$TREAT_CAT=ifelse(C7_summary$TREAT_DESC=="Coniferous"|
                              C7_summary$TREAT_DESC=="Deciduous"|
                              C7_summary$TREAT_DESC=="Successional", "Forest", 
                            ifelse(C7_summary$TREAT_DESC=="Alfalfa*"|
                                     C7_summary$TREAT_DESC=="Early successional"|
                                     C7_summary$TREAT_DESC=="Poplar trees", "Perennial",
                                   ifelse(C7_summary$TREAT_DESC=="Conventional"|
                                            C7_summary$TREAT_DESC=="No till"|
                                            C7_summary$TREAT_DESC=="Organic"|
                                            C7_summary$TREAT_DESC=="Reduced input", "Annual", "Check")))




# First make a dataframe for error bars
C7_agg <- ddply(C7_summary, .(TREAT_CAT, year), summarise,
                mean.bugs = mean(pertrap),
                sd.bugs.pt = sd(pertrap),
                N = length(pertrap),
                SE = sd.bugs.pt / sqrt(N))


######################################################################################

#BURSI

#bring data in from 
BURSI<-read.csv("data/BURSI2020comb.csv",
                header=T)


#details of cleaning in the code in comments found at that link- in summary, get all the typoes
#out and make the date column usable


library(lubridate)
colnames(BURSI)[1]="DATE"
colnames(BURSI)[4]="REPLICATE"
BURSI$newdate<-mdy(BURSI$DATE)
BURSI$year<-year(BURSI$newdate)
BURSI$DOY<-yday(BURSI$newdate)
BURSI<-na.omit(BURSI)
BURSI$TREAT_DESC<-gsub("Early succesional community", "Early successional", BURSI$TREAT_DESC)
BURSI$TREAT_DESC<-gsub("Early successional community", "Early successional", BURSI$TREAT_DESC)
BURSI$TREAT_DESC<-gsub("Early Successional Community", "Early successional", BURSI$TREAT_DESC)
BURSI$TREAT_DESC<-gsub("Early sucessional community", "Early successional", BURSI$TREAT_DESC)
BURSI$TREAT_DESC<-gsub("poplar trees", "Poplar trees", BURSI$TREAT_DESC)
BURSI$TREAT_DESC<-gsub("Succesional", "Successional", BURSI$TREAT_DESC)
BURSI$TREAT_DESC<-gsub("Sucessional", "Successional", BURSI$TREAT_DESC)
BURSI$TREAT_DESC<-gsub("Alfalfa", "Alfalfa*", BURSI$TREAT_DESC)
BURSI$TREAT_DESC<-gsub("Switchgrass", "Alfalfa*", BURSI$TREAT_DESC)
BURSI$TREAT_DESC<-gsub("Biologically based \\(organic\\)", "Organic", BURSI$TREAT_DESC)
BURSI$TREAT_DESC<-gsub("Conventional till", "Conventional", BURSI$TREAT_DESC)
BURSI$TREAT_DESC<-as.factor(BURSI$TREAT_DESC)
BURSI$HABITAT<-as.factor(BURSI$HABITAT)
BURSI$REPLICATE<-as.factor(BURSI$REPLICATE)
BURSI$STATION<-as.factor(BURSI$STATION)
str(BURSI)

###########
#subset the data to include only data before August 10th or the 222 DOY
BURSI= subset(BURSI, DOY > 0 & DOY < 222)
#subset the data to include  1993 or later, because sampling changed to add forests then
BURSI= subset(BURSI, year >= 1993)

library(reshape2)
#tell R where the data is by melting it, assigning IDs to the columns
BURSI1<-melt(BURSI, id=c("DATE","TREAT_DESC","HABITAT","REPLICATE","STATION","newdate", "year", "DOY"))
#cast the data to count up the fireflies
BURSI2<-dcast(BURSI1, year+TREAT_DESC+REPLICATE~., sum)
#cast the data to count the traps
BURSI3<-dcast(BURSI1, year+TREAT_DESC+REPLICATE~., length)
#let's rename these new vectors within the data frame
names(BURSI2)[4]<-"ADULTS"
names(BURSI3)[4]<-"TRAPS"

#rename the data frame and combine the number of traps we counted into it from BURSI3
BURSI_summary<-BURSI2
BURSI_summary$TRAPS<-BURSI3$TRAPS

#create a new variable to account for trapping effort in a given year
BURSI_summary$pertrap<-BURSI_summary$ADULTS/BURSI_summary$TRAPS



####Creating a new treatment classification 

BURSI_summary$TREAT_CAT=ifelse(BURSI_summary$TREAT_DESC=="Coniferous"|
                                 BURSI_summary$TREAT_DESC=="Deciduous"|
                                 BURSI_summary$TREAT_DESC=="Successional", "Forest", 
                               ifelse(BURSI_summary$TREAT_DESC=="Alfalfa*"|
                                        BURSI_summary$TREAT_DESC=="Early successional"|
                                        BURSI_summary$TREAT_DESC=="Poplar trees", "Perennial",
                                      ifelse(BURSI_summary$TREAT_DESC=="Conventional"|
                                               BURSI_summary$TREAT_DESC=="No till"|
                                               BURSI_summary$TREAT_DESC=="Organic"|
                                               BURSI_summary$TREAT_DESC=="Reduced input", "Annual", "Check")))




# First make a dataframe for error bars
BURSI_agg <- ddply(BURSI_summary, .(TREAT_CAT, year), summarise,
                   mean.bugs = mean(pertrap),
                   sd.bugs.pt = sd(pertrap),
                   N = length(pertrap),
                   SE = sd.bugs.pt / sqrt(N))



######################################################################################

#ABIPN

#bring data in from 
ABIPN<-read.csv("data/ABIPN2020comb.csv",
                header=T)


#details of cleaning in the code in comments found at that link- in summary, get all the typoes
#out and make the date column usable


library(lubridate)
colnames(ABIPN)[1]="DATE"
colnames(ABIPN)[4]="REPLICATE"
ABIPN$newdate<-mdy(ABIPN$DATE)
ABIPN$year<-year(ABIPN$newdate)
ABIPN$DOY<-yday(ABIPN$newdate)
ABIPN<-na.omit(ABIPN)
ABIPN$TREAT_DESC<-gsub("Early succesional community", "Early successional", ABIPN$TREAT_DESC)
ABIPN$TREAT_DESC<-gsub("Early successional community", "Early successional", ABIPN$TREAT_DESC)
ABIPN$TREAT_DESC<-gsub("Early Successional Community", "Early successional", ABIPN$TREAT_DESC)
ABIPN$TREAT_DESC<-gsub("Early sucessional community", "Early successional", ABIPN$TREAT_DESC)
ABIPN$TREAT_DESC<-gsub("poplar trees", "Poplar trees", ABIPN$TREAT_DESC)
ABIPN$TREAT_DESC<-gsub("Succesional", "Successional", ABIPN$TREAT_DESC)
ABIPN$TREAT_DESC<-gsub("Sucessional", "Successional", ABIPN$TREAT_DESC)
ABIPN$TREAT_DESC<-gsub("Alfalfa", "Alfalfa*", ABIPN$TREAT_DESC)
ABIPN$TREAT_DESC<-gsub("Switchgrass", "Alfalfa*", ABIPN$TREAT_DESC)
ABIPN$TREAT_DESC<-gsub("Biologically based \\(organic\\)", "Organic", ABIPN$TREAT_DESC)
ABIPN$TREAT_DESC<-gsub("Conventional till", "Conventional", ABIPN$TREAT_DESC)
ABIPN$TREAT_DESC<-as.factor(ABIPN$TREAT_DESC)
ABIPN$HABITAT<-as.factor(ABIPN$HABITAT)
ABIPN$REPLICATE<-as.factor(ABIPN$REPLICATE)
ABIPN$STATION<-as.factor(ABIPN$STATION)
str(ABIPN)

###########
#subset the data to include only data before August 10th or the 222 DOY
ABIPN= subset(ABIPN, DOY > 0 & DOY < 222)
#subset the data to include  1993 or later, because sampling changed to add forests then
ABIPN= subset(ABIPN, year >= 1993)

library(reshape2)
#tell R where the data is by melting it, assigning IDs to the columns
ABIPN1<-melt(ABIPN, id=c("DATE","TREAT_DESC","HABITAT","REPLICATE","STATION","newdate", "year", "DOY"))
#cast the data to count up the fireflies
ABIPN2<-dcast(ABIPN1, year+TREAT_DESC+REPLICATE~., sum)
#cast the data to count the traps
ABIPN3<-dcast(ABIPN1, year+TREAT_DESC+REPLICATE~., length)
#let's rename these new vectors within the data frame
names(ABIPN2)[4]<-"ADULTS"
names(ABIPN3)[4]<-"TRAPS"

#rename the data frame and combine the number of traps we counted into it from ABIPN3
ABIPN_summary<-ABIPN2
ABIPN_summary$TRAPS<-ABIPN3$TRAPS

#create a new variable to account for trapping effort in a given year
ABIPN_summary$pertrap<-ABIPN_summary$ADULTS/ABIPN_summary$TRAPS




####Creating a new treatment classification 

ABIPN_summary$TREAT_CAT=ifelse(ABIPN_summary$TREAT_DESC=="Coniferous"|
                                 ABIPN_summary$TREAT_DESC=="Deciduous"|
                                 ABIPN_summary$TREAT_DESC=="Successional", "Forest", 
                               ifelse(ABIPN_summary$TREAT_DESC=="Alfalfa*"|
                                        ABIPN_summary$TREAT_DESC=="Early successional"|
                                        ABIPN_summary$TREAT_DESC=="Poplar trees", "Perennial",
                                      ifelse(ABIPN_summary$TREAT_DESC=="Conventional"|
                                               ABIPN_summary$TREAT_DESC=="No till"|
                                               ABIPN_summary$TREAT_DESC=="Organic"|
                                               ABIPN_summary$TREAT_DESC=="Reduced input", "Annual", "Check")))




# First make a dataframe for error bars
ABIPN_agg <- ddply(ABIPN_summary, .(TREAT_CAT, year), summarise,
                   mean.bugs = mean(pertrap),
                   sd.bugs.pt = sd(pertrap),
                   N = length(pertrap),
                   SE = sd.bugs.pt / sqrt(N))


######################################
#get all this data into a single dataframe

invasive<-rbind(HAXY_summary, C7_summary,HVAR_summary, PQUA_summary)
native<-rbind(HPARN_summary,HGLAC_summary,HCONV_summary,H13_summary,CYCSP_summary,
              CTRIF_summary,CSTIG_summary, CMAC_summary,BURSI_summary,ABIPN_summary)
all_lb<-rbind(invasive, native)

invasivetot <- ddply(invasive, .(year, TREAT_DESC, TREAT_CAT, REPLICATE), summarise,
                   ADULTS = sum(ADULTS),
                   TRAPS=max(TRAPS))
nativetot <- ddply(native, .(year, TREAT_DESC, TREAT_CAT,  REPLICATE), summarise,
                      ADULTS = sum(ADULTS),
                      TRAPS=max(TRAPS))
all_tot <- ddply(all_lb, .(year, TREAT_DESC, TREAT_CAT,  REPLICATE), summarise,
                 ADULTS = sum(ADULTS),
                 TRAPS=max(TRAPS))


######################################################################################

#now let's make a stacked timeseries figure with a GAM for decade for each of these species

#gam will include sampling correction but that's it- we want to see the raw 
#population trend for each species
library(mgcv)
library(visreg)
library(ggplot2)
library(tidymv)




#graphical parameters
smooth.param<-0.5



##########################################
# make ABIPN figure

#get average number of traps per year per observation
newd <- with(ABIPN_summary,
             data.frame(year = seq(min(year), max(year), length = 1000),
                        TRAPS = 50))
knots<-round(length(unique(ABIPN_summary$year))/6) #only allow max of 1 knot every ~6 years

ABIPN.gam0<-gam(ADULTS~s(year, sp=smooth.param, k=knots)+offset(log(TRAPS)),
                data=ABIPN_summary, family="quasipoisson")
summary(ABIPN.gam0)
ABIPN.pred<-predict.gam(ABIPN.gam0, newd, se.fit = T, type="response")
ABIPN.pred<-cbind(newd,ABIPN.pred)
ABIPN.pred$lower<-ABIPN.pred$fit-2*ABIPN.pred$se.fit
ABIPN.pred$upper<-ABIPN.pred$fit+2*ABIPN.pred$se.fit

jitter<-position_jitter(width = 0.1, height = 0.02)

ABIPN.year<-ggplot(data=ABIPN.pred, aes(year, fit))+
  geom_point(data=ABIPN_summary, aes(year, ADULTS), position = jitter, pch=21, size=1, fill="lightgrey")+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='paleturquoise', alpha=0.6)+
  geom_line()+
  theme_classic()+
  xlim(1993, 2020)+
  xlab(NULL)+ylab("")+
  scale_y_continuous(trans='pseudo_log',limits=c(-0.13, NA))
ABIPN.year

#######
#by plant community or (or community group)
ABIPN.gam1<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=TREAT_DESC)+offset(log(TRAPS)),
                data=ABIPN_summary, family="quasipoisson")
summary(ABIPN.gam1)

visreg(ABIPN.gam1, "year", "TREAT_DESC", ylab="residual captures", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_DESC, ncol = 4)

ABIPN.gam2<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=as.factor(TREAT_CAT))+offset(log(TRAPS)),
                data=ABIPN_summary, family="quasipoisson")
summary(ABIPN.gam2)

visreg(ABIPN.gam2, "year", "TREAT_CAT", ylab="residual captures", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_CAT, ncol = 4)

#last 10 year trend
ABIPN10<-subset(ABIPN_summary, year >= 2011)

abi10mod<-lm(ADULTS~year, data=ABIPN10)

summary(abi10mod)


##########################################
# make BURSI figure

#get average number of traps per year per observation
newd <- with(BURSI_summary,
             data.frame(year = seq(min(year), max(year), length = 1000),
                        TRAPS = 50))
knots<-round(length(unique(BURSI_summary$year))/6) #only allow max of 1 knot every ~6 years
BURSI.gam0<-gam(ADULTS~s(year, sp=smooth.param, k=knots)+offset(log(TRAPS)),
                data=BURSI_summary, family="quasipoisson")
summary(BURSI.gam0)
BURSI.pred<-predict.gam(BURSI.gam0, newd, se.fit = T, type="response")
BURSI.pred<-cbind(newd,BURSI.pred)
BURSI.pred$lower<-BURSI.pred$fit-2*BURSI.pred$se.fit
BURSI.pred$upper<-BURSI.pred$fit+2*BURSI.pred$se.fit

jitter<-position_jitter(width = 0.1, height = 0.02)

BURSI.year<-ggplot(data=BURSI.pred, aes(year, fit))+
  geom_point(data=BURSI_summary, aes(year, ADULTS), position = jitter, pch=21, size=1, fill="lightgrey")+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='paleturquoise', alpha=0.6)+
  geom_line()+
  theme_classic()+
  xlim(1993, 2020)+
  xlab(NULL)+ylab("")+
  scale_y_continuous(trans='pseudo_log',limits=c(-0.1, NA))
BURSI.year

#######
#by plant community or (or community group)
BURSI.gam1<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=TREAT_DESC)+offset(log(TRAPS)),
                data=BURSI_summary, family="quasipoisson")
summary(BURSI.gam1)

visreg(BURSI.gam1, "year", "TREAT_DESC", ylab="residual captures", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_DESC, ncol = 4)

BURSI.gam2<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=as.factor(TREAT_CAT))+offset(log(TRAPS)),
                data=BURSI_summary, family="quasipoisson")
summary(BURSI.gam2)

visreg(BURSI.gam2, "year", "TREAT_CAT", ylab="residual captures", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_CAT, ncol = 4)

#last 10 year trend
BURSI10<-subset(BURSI_summary, year >= 2011)

bursi10mod<-lm(ADULTS~year, data=BURSI10)

summary(bursi10mod)

##########################################
# make C7 figure

#get average number of traps per year per observation
newd <- with(C7_summary,
             data.frame(year = seq(min(year), max(year), length = 1000),
                        TRAPS = 50))
knots<-round(length(unique(C7_summary$year))/6) #only allow max of 1 knot every ~6 years
C7.gam0<-gam(ADULTS~s(year, sp=smooth.param, k=knots)+offset(log(TRAPS)),
             data=C7_summary, family="quasipoisson")
summary(C7.gam0)
C7.pred<-predict.gam(C7.gam0, newd, se.fit = T, type="response")
C7.pred<-cbind(newd,C7.pred)
C7.pred$lower<-C7.pred$fit-2*C7.pred$se.fit
C7.pred$upper<-C7.pred$fit+2*C7.pred$se.fit

jitter<-position_jitter(width = 0.1, height = 0.02)

C7.year<-ggplot(data=C7.pred, aes(year, fit))+
  geom_point(data=C7_summary, aes(year, ADULTS), position = jitter, pch=21, size=1, fill="lightgrey")+
  geom_ribbon(aes(ymin=lower, ymax=upper),  fill='salmon1', alpha=0.6)+
  geom_line()+
  theme_classic()+
  xlim(1993, 2020)+
  xlab(NULL)+ylab("")+
  scale_y_continuous(trans='pseudo_log',limits=c(-0.1, NA))
C7.year

#######
#by plant community or (or community group)
C7.gam1<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=TREAT_DESC)+offset(log(TRAPS)),
                data=C7_summary, family="quasipoisson")
summary(C7.gam1)

visreg(C7.gam1, "year", "TREAT_DESC", ylab="residual captures", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_DESC, ncol = 4)

C7.gam2<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=as.factor(TREAT_CAT))+offset(log(TRAPS)),
                data=C7_summary, family="quasipoisson")
summary(C7.gam2)

visreg(C7.gam2, "year", "TREAT_CAT", ylab="residual captures", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_CAT, ncol = 4)

#last 10 year trend
C710<-subset(C7_summary, year >= 2011)

c710mod<-lm(ADULTS~year, data=C710)

summary(c710mod)

##########################################
# make CMAC figure

#get average number of traps per year per observation
newd <- with(CMAC_summary,
             data.frame(year = seq(min(year), max(year), length = 1000),
                        TRAPS = 50))
knots<-round(length(unique(CMAC_summary$year))/6) #only allow max of 1 knot every ~6 years
CMAC.gam0<-gam(ADULTS~s(year, sp=smooth.param, k=knots)+offset(log(TRAPS)),
               data=CMAC_summary, family="quasipoisson")
summary(CMAC.gam0)
CMAC.pred<-predict.gam(CMAC.gam0, newd, se.fit = T, type="response")
CMAC.pred<-cbind(newd,CMAC.pred)
CMAC.pred$lower<-CMAC.pred$fit-2*CMAC.pred$se.fit
CMAC.pred$upper<-CMAC.pred$fit+2*CMAC.pred$se.fit

jitter<-position_jitter(width = 0.1, height = 0.02)

CMAC.year<-ggplot(data=CMAC.pred, aes(year, fit))+
  geom_point(data=CMAC_summary, aes(year, ADULTS), position = jitter, pch=21, size=1, fill="lightgrey")+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='paleturquoise', alpha=0.6)+
  geom_line()+
  theme_classic()+
  xlim(1993, 2020)+
  xlab(NULL)+ylab("")+
  scale_y_continuous(trans='pseudo_log',limits=c(-0.1, NA))
CMAC.year

#######
#by plant community or (or community group)
CMAC.gam1<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=TREAT_DESC)+offset(log(TRAPS)),
             data=CMAC_summary, family="quasipoisson")
summary(CMAC.gam1)

visreg(CMAC.gam1, "year", "TREAT_DESC", ylab="residual captures", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_DESC, ncol = 4)

CMAC.gam2<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=as.factor(TREAT_CAT))+offset(log(TRAPS)),
             data=C7_summary, family="quasipoisson")
summary(CMAC.gam2)

visreg(CMAC.gam2, "year", "TREAT_CAT", ylab="residual captures", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_CAT, ncol = 4)

#last 10 year trend
CMAC10<-subset(CMAC_summary, year >= 2011)

cmac10mod<-lm(ADULTS~year, data=CMAC10)

summary(CMAC10mod)

##########################################
# make CSTIG figure

#get average number of traps per year per observation
newd <- with(CSTIG_summary,
             data.frame(year = seq(min(year), max(year), length = 1000),
                        TRAPS = 50))
knots<-round(length(unique(CSTIG_summary$year))/6) #only allow max of 1 knot every ~6 years
CSTIG.gam0<-gam(ADULTS~s(year, sp=smooth.param, k=knots)+offset(log(TRAPS)),
                data=CSTIG_summary, family="quasipoisson")
summary(CSTIG.gam0)
CSTIG.pred<-predict.gam(CSTIG.gam0, newd, se.fit = T, type="response")
CSTIG.pred<-cbind(newd,CSTIG.pred)
CSTIG.pred$lower<-CSTIG.pred$fit-2*CSTIG.pred$se.fit
CSTIG.pred$upper<-CSTIG.pred$fit+2*CSTIG.pred$se.fit

jitter<-position_jitter(width = 0.1, height = 0.02)

CSTIG.year<-ggplot(data=CSTIG.pred, aes(year, fit))+
  geom_point(data=CSTIG_summary, aes(year, ADULTS), position = jitter, pch=21, size=1, fill="lightgrey")+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='paleturquoise', alpha=0.6)+
  geom_line()+
  theme_classic()+
  xlim(1993, 2020)+
  xlab(NULL)+ylab("")+
  scale_y_continuous(trans='pseudo_log',limits=c(-0.1, NA))
CSTIG.year

#######
#by plant community or (or community group)
CSTIG.gam1<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=TREAT_DESC)+offset(log(TRAPS)),
               data=CSTIG_summary, family="quasipoisson")
summary(CSTIG.gam1)

visreg(CSTIG.gam1, "year", "TREAT_DESC", ylab="residual captures", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_DESC, ncol = 4)

CSTIG.gam2<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=as.factor(TREAT_CAT))+offset(log(TRAPS)),
               data=CSTIG_summary, family="quasipoisson")
summary(CSTIG.gam2)

visreg(CSTIG.gam2, "year", "TREAT_CAT", ylab="residual captures", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_CAT, ncol = 4)

##########################################
# make CTRIF figure

#get average number of traps per year per observation
newd <- with(CTRIF_summary,
             data.frame(year = seq(min(year), max(year), length = 1000),
                        TRAPS = 50))
knots<-round(length(unique(CTRIF_summary$year))/6) #only allow max of 1 knot every ~6 years
CTRIF.gam0<-gam(ADULTS~s(year, sp=smooth.param, k=knots)+offset(log(TRAPS)),
                data=CTRIF_summary, family="quasipoisson")
summary(CTRIF.gam0)
CTRIF.pred<-predict.gam(CTRIF.gam0, newd, se.fit = T, type="response")
CTRIF.pred<-cbind(newd,CTRIF.pred)
CTRIF.pred$lower<-CTRIF.pred$fit-2*CTRIF.pred$se.fit
CTRIF.pred$upper<-CTRIF.pred$fit+2*CTRIF.pred$se.fit

jitter<-position_jitter(width = 0.1, height = 0.02)

CTRIF.year<-ggplot(data=CTRIF.pred, aes(year, fit))+
  geom_point(data=CTRIF_summary, aes(year, ADULTS), position = jitter, pch=21, size=1, fill="lightgrey")+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='paleturquoise', alpha=0.6)+
  geom_line()+
  theme_classic()+
  xlim(1993, 2020)+
  xlab(NULL)+ylab("")+
  scale_y_continuous(trans='pseudo_log',limits=c(-0.1, NA))
CTRIF.year

#######
#by plant community or (or community group)
CTRIF.gam1<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=TREAT_DESC)+offset(log(TRAPS)),
                data=CTRIF_summary, family="quasipoisson")
summary(CTRIF.gam1)

visreg(CTRIF.gam1, "year", "TREAT_DESC", ylab="residual captures", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_DESC, ncol = 4)

CTRIF.gam2<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=as.factor(TREAT_CAT))+offset(log(TRAPS)),
                data=CTRIF_summary, family="quasipoisson")
summary(CTRIF.gam2)

visreg(CTRIF.gam2, "year", "TREAT_CAT", ylab="residual captures", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_CAT, ncol = 4)

##########################################
# make CYCSP figure

#get average number of traps per year per observation
newd <- with(CYCSP_summary,
             data.frame(year = seq(min(year), max(year), length = 1000),
                        TRAPS = 50))
knots<-round(length(unique(CYCSP_summary$year))/6) #only allow max of 1 knot every ~6 years
CYCSP.gam0<-gam(ADULTS~s(year, sp=smooth.param, k=knots)+offset(log(TRAPS)),
                data=CYCSP_summary, family="quasipoisson")
summary(CYCSP.gam0)
CYCSP.pred<-predict.gam(CYCSP.gam0, newd, se.fit = T, type="response")
CYCSP.pred<-cbind(newd,CYCSP.pred)
CYCSP.pred$lower<-CYCSP.pred$fit-2*CYCSP.pred$se.fit
CYCSP.pred$upper<-CYCSP.pred$fit+2*CYCSP.pred$se.fit

jitter<-position_jitter(width = 0.1, height = 0.02)

CYCSP.year<-ggplot(data=CYCSP.pred, aes(year, fit))+
  geom_point(data=CYCSP_summary, aes(year, ADULTS), position = jitter, pch=21, size=1, fill="lightgrey")+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='paleturquoise', alpha=0.6)+
  geom_line()+
  theme_classic()+
  xlim(1993, 2020)+
  xlab(NULL)+ylab("")+
  scale_y_continuous(trans='pseudo_log',limits=c(-0.1, NA))
CYCSP.year

#######
#by plant community or (or community group)
CYCSP.gam1<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=TREAT_DESC)+offset(log(TRAPS)),
                data=CYCSP_summary, family="quasipoisson")
summary(CYCSP.gam1)

visreg(CYCSP.gam1, "year", "TREAT_DESC", ylab="residual captures", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_DESC, ncol = 4)

CYCSP.gam2<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=as.factor(TREAT_CAT))+offset(log(TRAPS)),
                data=CYCSP_summary, family="quasipoisson")
summary(CYCSP.gam2)

visreg(CYCSP.gam2, "year", "TREAT_CAT", ylab="residual captures", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_CAT, ncol = 4)



##########################################
# make H13 figure

#get average number of traps per year per observation
newd <- with(H13_summary,
             data.frame(year = seq(min(year), max(year), length = 1000),
                        TRAPS = 50))
knots<-round(length(unique(H13_summary$year))/6) #only allow max of 1 knot every ~6 years
H13.gam0<-gam(ADULTS~s(year, sp=smooth.param, k=knots)+offset(log(TRAPS)),
              data=H13_summary, family="quasipoisson")
summary(H13.gam0)
H13.pred<-predict.gam(H13.gam0, newd, se.fit = T, type="response")
H13.pred<-cbind(newd,H13.pred)
H13.pred$lower<-H13.pred$fit-2*H13.pred$se.fit
H13.pred$upper<-H13.pred$fit+2*H13.pred$se.fit

jitter<-position_jitter(width = 0.1, height = 0.02)

H13.year<-ggplot(data=H13.pred, aes(year, fit))+
  geom_point(data=H13_summary, aes(year, ADULTS), position = jitter, pch=21, size=1, fill="lightgrey")+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='paleturquoise', alpha=0.6)+
  geom_line()+
  theme_classic()+
  xlim(1993, 2020)+
  xlab(NULL)+ylab("")+
  scale_y_continuous(trans='pseudo_log',limits=c(-0.1, NA))
H13.year

#######
#by plant community or (or community group)
H13.gam1<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=TREAT_DESC)+offset(log(TRAPS)),
                data=H13_summary, family="quasipoisson")
summary(H13.gam1)

visreg(H13.gam1, "year", "TREAT_DESC", ylab="residual captures", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_DESC, ncol = 4)

H13.gam2<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=as.factor(TREAT_CAT))+offset(log(TRAPS)),
                data=H13_summary, family="quasipoisson")
summary(H13.gam2)

visreg(H13.gam2, "year", "TREAT_CAT", ylab="residual captures", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_CAT, ncol = 4)



##########################################
# make HAXY figure

#get average number of traps per year per observation
newd <- with(HAXY_summary,
             data.frame(year = seq(min(year), max(year), length = 1000),
                        TRAPS = 50))
knots<-round(length(unique(HAXY_summary$year))/6) #only allow max of 1 knot every ~6 years
HAXY.gam0<-gam(ADULTS~s(year, sp=smooth.param, k=knots)+offset(log(TRAPS)),
               data=HAXY_summary, family="quasipoisson")
summary(HAXY.gam0)
HAXY.pred<-predict.gam(HAXY.gam0, newd, se.fit = T, type="response")
HAXY.pred<-cbind(newd,HAXY.pred)
HAXY.pred$lower<-HAXY.pred$fit-2*HAXY.pred$se.fit
HAXY.pred$upper<-HAXY.pred$fit+2*HAXY.pred$se.fit

jitter<-position_jitter(width = 0.1, height = 0.02)

HAXY.year<-ggplot(data=HAXY.pred, aes(year, fit))+
  geom_point(data=HAXY_summary, aes(year, ADULTS), position = jitter, pch=21, size=1, fill="lightgrey")+
  geom_ribbon(aes(ymin=lower, ymax=upper),  fill='salmon1', alpha=0.6)+
  geom_line()+
  theme_classic()+
  xlim(1993, 2020)+
  xlab(NULL)+ylab("")+
  scale_y_continuous(trans='pseudo_log',limits=c(-0.1, NA))
HAXY.year

#######
#by plant community or (or community group)
HAXY.gam1<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=TREAT_DESC)+offset(log(TRAPS)),
              data=HAXY_summary, family="quasipoisson")
summary(HAXY.gam1)

visreg(HAXY.gam1, "year", "TREAT_DESC", ylab="residual captures", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_DESC, ncol = 4)

HAXY.gam2<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=as.factor(TREAT_CAT))+offset(log(TRAPS)),
              data=HAXY_summary, family="quasipoisson")
summary(HAXY.gam2)

visreg(HAXY.gam2, "year", "TREAT_CAT", ylab="residual captures", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_CAT, ncol = 4)


##########################################
# make HCONV figure

#get average number of traps per year per observation
newd <- with(HCONV_summary,
             data.frame(year = seq(min(year), max(year), length = 1000),
                        TRAPS = 50))
knots<-round(length(unique(HCONV_summary$year))/6) #only allow max of 1 knot every ~6 years
HCONV.gam0<-gam(ADULTS~s(year, sp=smooth.param, k=knots)+offset(log(TRAPS)),
                data=HCONV_summary, family="quasipoisson")
summary(HCONV.gam0)
HCONV.pred<-predict.gam(HCONV.gam0, newd, se.fit = T, type="response")
HCONV.pred<-cbind(newd,HCONV.pred)
HCONV.pred$lower<-HCONV.pred$fit-2*HCONV.pred$se.fit
HCONV.pred$upper<-HCONV.pred$fit+2*HCONV.pred$se.fit

jitter<-position_jitter(width = 0.1, height = 0.02)

HCONV.year<-ggplot(data=HCONV.pred, aes(year, fit))+
  geom_point(data=HCONV_summary, aes(year, ADULTS), position = jitter, pch=21, size=1, fill="lightgrey")+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='paleturquoise', alpha=0.6)+
  geom_line()+
  theme_classic()+
  xlim(1993, 2020)+
  xlab(NULL)+ylab("")+
  scale_y_continuous(trans='pseudo_log',limits=c(-0.1, NA))
HCONV.year

#######
#by plant community or (or community group)
HCONV.gam1<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=TREAT_DESC)+offset(log(TRAPS)),
               data=HCONV_summary, family="quasipoisson")
summary(HCONV.gam1)

visreg(HCONV.gam1, "year", "TREAT_DESC", ylab="residual captures", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_DESC, ncol = 4)

HCONV.gam2<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=as.factor(TREAT_CAT))+offset(log(TRAPS)),
               data=HCONV_summary, family="quasipoisson")
summary(HCONV.gam2)

visreg(HCONV.gam2, "year", "TREAT_CAT", ylab="residual captures", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_CAT, ncol = 4)
##########################################
# make HGLAC figure

#get average number of traps per year per observation
newd <- with(HGLAC_summary,
             data.frame(year = seq(min(year), max(year), length = 1000),
                        TRAPS = 50))
knots<-round(length(unique(HGLAC_summary$year))/6) #only allow max of 1 knot every ~6 years
HGLAC.gam0<-gam(ADULTS~s(year, sp=smooth.param, k=knots)+offset(log(TRAPS)),
                data=HGLAC_summary, family="quasipoisson")
summary(HGLAC.gam0)
HGLAC.pred<-predict.gam(HGLAC.gam0, newd, se.fit = T, type="response")
HGLAC.pred<-cbind(newd,HGLAC.pred)
HGLAC.pred$lower<-HGLAC.pred$fit-2*HGLAC.pred$se.fit
HGLAC.pred$upper<-HGLAC.pred$fit+2*HGLAC.pred$se.fit

jitter<-position_jitter(width = 0.1, height = 0.02)

HGLAC.year<-ggplot(data=HGLAC.pred, aes(year, fit))+
  geom_point(data=HGLAC_summary, aes(year, ADULTS), position = jitter, pch=21, size=1, fill="lightgrey")+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='paleturquoise', alpha=0.6)+
  geom_line()+
  theme_classic()+
  xlim(1993, 2020)+
  xlab(NULL)+ylab("")+
  scale_y_continuous(trans='pseudo_log',limits=c(-0.1, NA))
HGLAC.year

#######
#by plant community or (or community group)
HGLAC.gam1<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=TREAT_DESC)+offset(log(TRAPS)),
                data=HGLAC_summary, family="quasipoisson")
summary(HGLAC.gam1)

visreg(HGLAC.gam1, "year", "TREAT_DESC", ylab="residual captures", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_DESC, ncol = 4)

HGLAC.gam2<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=as.factor(TREAT_CAT))+offset(log(TRAPS)),
                data=HGLAC_summary, family="quasipoisson")
summary(HGLAC.gam2)

visreg(HGLAC.gam2, "year", "TREAT_CAT", ylab="residual captures", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_CAT, ncol = 4)

##########################################
# make HPARN figure

#get average number of traps per year per observation
newd <- with(HPARN_summary,
             data.frame(year = seq(min(year), max(year), length = 1000),
                        TRAPS = 50))
knots<-round(length(unique(HPARN_summary$year))/6) #only allow max of 1 knot every ~6 years
HPARN.gam0<-gam(ADULTS~s(year, sp=smooth.param, k=knots)+offset(log(TRAPS)),
                data=HPARN_summary, family="quasipoisson")
summary(HPARN.gam0)
HPARN.pred<-predict.gam(HPARN.gam0, newd, se.fit = T, type="response")
HPARN.pred<-cbind(newd,HPARN.pred)
HPARN.pred$lower<-HPARN.pred$fit-2*HPARN.pred$se.fit
HPARN.pred$upper<-HPARN.pred$fit+2*HPARN.pred$se.fit

jitter<-position_jitter(width = 0.1, height = 0.02)

HPARN.year<-ggplot(data=HPARN.pred, aes(year, fit))+
  geom_point(data=HPARN_summary, aes(year, ADULTS), position = jitter, pch=21, size=1, fill="lightgrey")+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='paleturquoise', alpha=0.6)+
  geom_line()+
  theme_classic()+
  xlim(1993, 2020)+
  xlab(NULL)+ylab("")+
  scale_y_continuous(trans='pseudo_log',limits=c(-0.1, NA))
HPARN.year

#######
#by plant community or (or community group)
HPARN.gam1<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=TREAT_DESC)+offset(log(TRAPS)),
                data=HPARN_summary, family="quasipoisson")
summary(HPARN.gam1)

visreg(HPARN.gam1, "year", "TREAT_DESC", ylab="residual captures", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_DESC, ncol = 4)

HPARN.gam2<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=as.factor(TREAT_CAT))+offset(log(TRAPS)),
                data=HPARN_summary, family="quasipoisson")
summary(HPARN.gam2)

visreg(HPARN.gam2, "year", "TREAT_CAT", ylab="residual captures", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_CAT, ncol = 4)
##########################################
# make HVAR figure

#get average number of traps per year per observation
newd <- with(HVAR_summary,
             data.frame(year = seq(min(year), max(year), length = 1000),
                        TRAPS = 50))
knots<-round(length(unique(HVAR_summary$year))/6) #only allow max of 1 knot every ~6 years
HVAR.gam0<-gam(ADULTS~s(year, sp=smooth.param, k=knots)+offset(log(TRAPS)),
               data=HVAR_summary, family="quasipoisson")
summary(HVAR.gam0)
HVAR.pred<-predict.gam(HVAR.gam0, newd, se.fit = T, type="response")
HVAR.pred<-cbind(newd,HVAR.pred)
HVAR.pred$lower<-HVAR.pred$fit-2*HVAR.pred$se.fit
HVAR.pred$upper<-HVAR.pred$fit+2*HVAR.pred$se.fit

jitter<-position_jitter(width = 0.1, height = 0.02)

HVAR.year<-ggplot(data=HVAR.pred, aes(year, fit))+
  geom_point(data=HVAR_summary, aes(year, ADULTS), position = jitter, pch=21, size=1, fill="lightgrey")+
  geom_ribbon(aes(ymin=lower, ymax=upper),  fill='salmon1', alpha=0.6)+
  geom_line()+
  theme_classic()+
  xlim(1993, 2020)+
  xlab(NULL)+ylab("")+
  scale_y_continuous(trans='pseudo_log',limits=c(-0.1, NA))
HVAR.year

#######
#by plant community or (or community group)
HVAR.gam1<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=TREAT_DESC)+offset(log(TRAPS)),
                data=HVAR_summary, family="quasipoisson")
summary(HVAR.gam1)

visreg(HVAR.gam1, "year", "TREAT_DESC", ylab="residual captures", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_DESC, ncol = 4)

HVAR.gam2<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=as.factor(TREAT_CAT))+offset(log(TRAPS)),
                data=HVAR_summary, family="quasipoisson")
summary(HVAR.gam2)

visreg(HVAR.gam2, "year", "TREAT_CAT", ylab="residual captures", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_CAT, ncol = 4)

##########################################
# make PQUA figure

#get average number of traps per year per observation
newd <- with(PQUA_summary,
             data.frame(year = seq(min(year), max(year), length = 1000),
                        TRAPS = 50))
knots<-round(length(unique(PQUA_summary$year))/6) #only allow max of 1 knot every ~6 years
PQUA.gam0<-gam(ADULTS~s(year, sp=smooth.param, k=knots)+offset(log(TRAPS)),
               data=PQUA_summary, family="quasipoisson")
summary(PQUA.gam0)
PQUA.pred<-predict.gam(PQUA.gam0, newd, se.fit = T, type="response")
PQUA.pred<-cbind(newd,PQUA.pred)
PQUA.pred$lower<-PQUA.pred$fit-2*PQUA.pred$se.fit
PQUA.pred$upper<-PQUA.pred$fit+2*PQUA.pred$se.fit

jitter<-position_jitter(width = 0.1, height = 0.02)

PQUA.year<-ggplot(data=PQUA.pred, aes(year, fit))+
  geom_point(data=PQUA_summary, aes(year, ADULTS), position = jitter, pch=21, size=1, fill="lightgrey")+
  geom_ribbon(aes(ymin=lower, ymax=upper),  fill='salmon1', alpha=0.6)+
  geom_line()+
    theme_classic()+
  xlim(1993, 2020)+
  xlab(NULL)+ylab("")+
  scale_y_continuous(trans='pseudo_log',limits=c(-0.1, NA))
PQUA.year

#######
#by plant community or (or community group)
PQUA.gam1<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=TREAT_DESC)+offset(log(TRAPS)),
               data=PQUA_summary, family="quasipoisson")
summary(PQUA.gam1)

visreg(PQUA.gam1, "year", "TREAT_DESC", ylab="residual captures", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_DESC, ncol = 4)

PQUA.gam2<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=as.factor(TREAT_CAT))+offset(log(TRAPS)),
               data=PQUA_summary, family="quasipoisson")
summary(PQUA.gam2)

visreg(PQUA.gam2, "year", "TREAT_CAT", ylab="residual captures", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_CAT, ncol = 4)

##########################################
# make all native figure

#get average number of traps per year per observation
newd <- with(nativetot,
             data.frame(year = seq(min(year), max(year), length = 1000),
                        TRAPS = 50))
knots<-round(length(unique(nativetot$year))/6) #only allow max of 1 knot every ~6 years
native.gam0<-gam(ADULTS~s(year, sp=smooth.param, k=knots)+offset(log(TRAPS)),
                 data=nativetot, family="quasipoisson")
summary(native.gam0)
native.pred<-predict.gam(native.gam0, newd, se.fit = T, type="response")
native.pred<-cbind(newd,native.pred)
native.pred$lower<-native.pred$fit-2*native.pred$se.fit
native.pred$upper<-native.pred$fit+2*native.pred$se.fit

jitter<-position_jitter(width = 0.1, height = 0.02)

native.year<-ggplot(data=native.pred, aes(year, fit))+
  geom_point(data=native, aes(year, ADULTS), position = jitter, pch=21, size=1, fill="lightgrey")+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='paleturquoise', alpha=0.6)+
  geom_line()+
  
  theme_classic()+
  xlim(1993, 2020)+
  xlab(NULL)+ylab("")+
  scale_y_continuous(trans='pseudo_log',limits=c(-0.1, NA))
native.year



##########################################
# make all invasive figure

#get average number of traps per year per observation
newd <- with(invasivetot,
             data.frame(year = seq(min(year), max(year), length = 1000),
                        TRAPS = 50))
knots<-round(length(unique(invasivetot$year))/6) #only allow max of 1 knot every ~6 years
invasive.gam0<-gam(ADULTS~s(year, sp=smooth.param, k=knots)+offset(log(TRAPS)),
                   data=invasivetot, family="quasipoisson")
summary(invasive.gam0)
invasive.pred<-predict.gam(invasive.gam0, newd, se.fit = T, type="response")
invasive.pred<-cbind(newd,invasive.pred)
invasive.pred$lower<-invasive.pred$fit-2*invasive.pred$se.fit
invasive.pred$upper<-invasive.pred$fit+2*invasive.pred$se.fit

jitter<-position_jitter(width = 0.1, height = 0.02)

invasive.year<-ggplot(data=invasive.pred, aes(year, fit))+
  geom_point(data=invasive, aes(year, ADULTS), position = jitter, pch=21, size=1, fill="lightgrey")+
  geom_ribbon(aes(ymin=lower, ymax=upper),  fill='salmon1', alpha=0.6)+
  geom_line()+
  theme_classic()+
  xlim(1993, 2020)+
  xlab(NULL)+ylab("")+
  scale_y_continuous(trans='pseudo_log',limits=c(-0.1, NA))
invasive.year


##########################################
# make all all_ figure

#get average number of traps per year per observation
newd <- with(all_tot,
             data.frame(year = seq(min(year), max(year), length = 1000),
                        TRAPS = 50))
knots<-round(length(unique(all_tot$year))/6) #only allow max of 1 knot every ~6 years
all_gam0<-gam(ADULTS~s(year, sp=smooth.param, k=knots)+offset(log(TRAPS)),
              data=all_tot, family="quasipoisson")
summary(all_gam0)
all_pred<-predict.gam(all_gam0, newd, se.fit = T, type="response")
all_pred<-cbind(newd,all_pred)
all_pred$lower<-all_pred$fit-2*all_pred$se.fit
all_pred$upper<-all_pred$fit+2*all_pred$se.fit

jitter<-position_jitter(width = 0.1, height = 0.02)

all_year<-ggplot(data=all_pred, aes(year, fit))+
  geom_point(data=all_tot, aes(year, ADULTS), position = jitter, pch=21, size=1, fill="lightgrey")+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='grey', alpha=0.6)+
  geom_line()+
  theme_classic()+
  xlim(1993, 2020)+
  xlab(NULL)+ylab("")+
  scale_y_continuous(trans='pseudo_log', limits=c(-0.1, NA))
all_year

########################################
library(cowplot)

timeseries.stack<-plot_grid(ABIPN.year, BURSI.year, C7.year, CMAC.year,
                            CSTIG.year, CTRIF.year, CYCSP.year, H13.year,
                            HAXY.year,HCONV.year, HGLAC.year,HPARN.year,
                            HVAR.year,PQUA.year, 
                            ncol=2, rel_widths=c(1, 1), labels=c('A', 'B', 'C','D', 'E', 'F',
                                                                 'G', 'H', 'I', 'J','K', 'L','M','N'), align="v")
timeseries.stack

pdf("plots/timeseries_stack.pdf", height=12, width=6)
grid.draw(timeseries.stack)
dev.off()

timeseries.grouped<-plot_grid(native.year, invasive.year, all_year, 
                            ncol=1, rel_widths=c(1), labels=c('A', 'B', 'C'), align="v")
timeseries.grouped

pdf("plots/timeseries_grouped.pdf", height=8, width=6)
grid.draw(timeseries.grouped)
dev.off()

#####################
#let's check for differential patterns in the timeseries by plant community treatment

#natives
all_gam_plants<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=TREAT_DESC)+offset(log(TRAPS)),
                    data=nativetot, family="quasipoisson")
summary(all_gam_plants)

visreg(all_gam_plants, "year", "TREAT_DESC", ylab="residual captures", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log', limits=c(-0.1, 10))+
  facet_wrap(~TREAT_DESC, ncol = 4)

all_gam_plants1<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=as.factor(TREAT_CAT))+offset(log(TRAPS)),
                     data=nativetot, family="quasipoisson")
summary(all_gam_plants1)

visreg(all_gam_plants1, "year", "TREAT_CAT", ylab="residual captures", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log', limits=c(-0.1, 10))+
  facet_wrap(~TREAT_CAT, ncol = 4)

#invasives
all_gam_plants<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=TREAT_DESC)+offset(log(TRAPS)),
                    data=invasivetot, family="quasipoisson")
summary(all_gam_plants)

visreg(all_gam_plants, "year", "TREAT_DESC", ylab="residual captures", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log', limits=c(-0.1, 10))+
  facet_wrap(~TREAT_DESC, ncol = 4)

all_gam_plants1<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=as.factor(TREAT_CAT))+offset(log(TRAPS)),
                     data=invasivetot, family="quasipoisson")
summary(all_gam_plants1)

visreg(all_gam_plants1, "year", "TREAT_CAT", ylab="residual captures", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log', limits=c(-0.1, 10))+
  facet_wrap(~TREAT_CAT, ncol = 4)


#all species

all_gam_plants<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=TREAT_DESC)+offset(log(TRAPS)),
              data=all_tot, family="quasipoisson")
summary(all_gam_plants)

visreg(all_gam_plants, "year", "TREAT_DESC", ylab="residual captures", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log', limits=c(-0.1, 10))+
  facet_wrap(~TREAT_DESC, ncol = 4)

all_gam_plants1<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=as.factor(TREAT_CAT))+offset(log(TRAPS)),
                    data=all_tot, family="quasipoisson")
summary(all_gam_plants1)

visreg(all_gam_plants1, "year", "TREAT_CAT", ylab="residual captures", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log', limits=c(-0.1, 10))+
  facet_wrap(~TREAT_CAT, ncol = 4)
