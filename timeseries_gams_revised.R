# start by cleaning data

#######################################

#let's do it all at once

MCSE<-read.csv("data/MCSE_working_alldata.txt",
               header=T)
Forest<-read.csv("data/Forest_working_alldata.txt",
               header=T)

#fix unmatched names
names(Forest)<-names(MCSE)


#append the two tables together

all_lb<-rbind(MCSE, Forest)

names(all_lb)<-toupper(names(all_lb)) #make column names consistent with previous formatting

#Fit date format

library(lubridate)

all_lb$newdate<-mdy_hms(all_lb$DATE)
all_lb$year<-year(all_lb$newdate)
all_lb$DOY<-yday(all_lb$newdate)

#cut NA data
all_lb<-na.omit(all_lb)


#now we fix the known typos and give workable treatment names

all_lb$TREAT_DESC<-gsub("Early succesional community", "Early successional", all_lb$TREAT_DESC)
all_lb$TREAT_DESC<-gsub("Early successional community", "Early successional", all_lb$TREAT_DESC)
all_lb$TREAT_DESC<-gsub("Early Successional Community", "Early successional", all_lb$TREAT_DESC)
all_lb$TREAT_DESC<-gsub("Early sucessional community", "Early successional", all_lb$TREAT_DESC)
all_lb$TREAT_DESC<-gsub("poplar trees", "Poplar trees", all_lb$TREAT_DESC)
all_lb$TREAT_DESC<-gsub("Succesional", "Successional", all_lb$TREAT_DESC)
all_lb$TREAT_DESC<-gsub("Sucessional", "Successional", all_lb$TREAT_DESC)
all_lb$TREAT_DESC<-gsub("Alfalfa", "Alfalfa*", all_lb$TREAT_DESC)
all_lb$TREAT_DESC<-gsub("Switchgrass", "Alfalfa*", all_lb$TREAT_DESC)
all_lb$TREAT_DESC<-gsub("Biologically based \\(organic\\)", "Organic", all_lb$TREAT_DESC)
all_lb$TREAT_DESC<-gsub("Conventional till", "Conventional", all_lb$TREAT_DESC)
all_lb$TREAT_DESC<-as.factor(all_lb$TREAT_DESC)
all_lb$HABITAT<-as.factor(all_lb$HABITAT)
all_lb$REPLICATE<-as.factor(all_lb$REPLICATE)
all_lb$STATION<-as.factor(all_lb$STATION)
str(all_lb)




#need to aggregate the data in a meaningful way by species

###########
#subset the data to include only data before August 10th or the 222 DOY
all_lb= subset(all_lb, DOY > 0 & DOY < 222)
#subset the data to include  1993 or later, because sampling changed to add forests then
all_lb= subset(all_lb, year >= 1993)


#let's start by making the individual data frames like we had before

all_lb1<-all_lb
all_lb1$SPID<-NULL

library(reshape2)
library(plyr)

ABIPN<-all_lb1[which(all_lb$SPID=="ABIPN"),]
BURSI<-all_lb1[which(all_lb$SPID=="BURSI"),]
C7<-all_lb1[which(all_lb$SPID=="C7"),]
CMAC<-all_lb1[which(all_lb$SPID=="CMAC"),]
CSTIG<-all_lb1[which(all_lb$SPID=="CSTIG"),]
CTRIF<-all_lb1[which(all_lb$SPID=="CTRIF"),]
CYCSP<-all_lb1[which(all_lb$SPID=="CYCSP"),]
H13<-all_lb1[which(all_lb$SPID=="H13"),]
HAXY<-all_lb1[which(all_lb$SPID=="HAXY"),]
HCONV<-all_lb1[which(all_lb$SPID=="HCONV"),]
HGLAC<-all_lb1[which(all_lb$SPID=="HGLAC"),]
HPARN<-all_lb1[which(all_lb$SPID=="HPARN"),]
HVAR<-all_lb1[which(all_lb$SPID=="HVAR"),]
PQUA<-all_lb1[which(all_lb$SPID=="PQUA"),]



#####################################################################################

#PQUA

#tell R where the data is by melting it, assigning IDs to the columns

#cast the data to count up the bugs
PQUA2<-dcast(PQUA, year+TREAT_DESC+REPLICATE~., value.var="ADULTS", sum)
#cast the data to count the traps
PQUA3<-dcast(PQUA, year+TREAT_DESC+REPLICATE~., value.var="ADULTS", length)
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






# First make a dataframe for error bars
PQUA_agg <- ddply(PQUA_summary, .(TREAT_CAT, year), summarise,
                  mean.bugs = mean(pertrap),
                  sd.bugs.pt = sd(pertrap),
                  N = length(pertrap),
                  SE = sd.bugs.pt / sqrt(N))

#####################################################################################

#HVAR



#cast the data to count up the fireflies
HVAR2<-dcast(HVAR, year+TREAT_DESC+REPLICATE~.,value.var="ADULTS", sum)
#cast the data to count the traps
HVAR3<-dcast(HVAR, year+TREAT_DESC+REPLICATE~.,value.var="ADULTS", length)
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


#cast the data to count up the fireflies
HPARN2<-dcast(HPARN, year+TREAT_DESC+REPLICATE~.,value.var="ADULTS", sum)
#cast the data to count the traps
HPARN3<-dcast(HPARN, year+TREAT_DESC+REPLICATE~., value.var="ADULTS",length)
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



#cast the data to count up the fireflies
HGLAC2<-dcast(HGLAC, year+TREAT_DESC+REPLICATE~.,value.var="ADULTS", sum)
#cast the data to count the traps
HGLAC3<-dcast(HGLAC, year+TREAT_DESC+REPLICATE~.,value.var="ADULTS", length)
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



#cast the data to count up the fireflies
HCONV2<-dcast(HCONV, year+TREAT_DESC+REPLICATE~.,value.var="ADULTS", sum)
#cast the data to count the traps
HCONV3<-dcast(HCONV, year+TREAT_DESC+REPLICATE~.,value.var="ADULTS", length)
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


#cast the data to count up the fireflies
HAXY2<-dcast(HAXY, year+TREAT_DESC+REPLICATE~.,value.var="ADULTS", sum)
#cast the data to count the traps
HAXY3<-dcast(HAXY, year+TREAT_DESC+REPLICATE~.,value.var="ADULTS", length)
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


#cast the data to count up the fireflies
H132<-dcast(H13, year+TREAT_DESC+REPLICATE~.,value.var="ADULTS", sum)
#cast the data to count the traps
H133<-dcast(H13, year+TREAT_DESC+REPLICATE~.,value.var="ADULTS", length)
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


#cast the data to count up the fireflies
CYCSP2<-dcast(CYCSP, year+TREAT_DESC+REPLICATE~.,value.var="ADULTS", sum)
#cast the data to count the traps
CYCSP3<-dcast(CYCSP, year+TREAT_DESC+REPLICATE~.,value.var="ADULTS", length)
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



#cast the data to count up the fireflies
CTRIF2<-dcast(CTRIF, year+TREAT_DESC+REPLICATE~.,value.var="ADULTS", sum)
#cast the data to count the traps
CTRIF3<-dcast(CTRIF, year+TREAT_DESC+REPLICATE~.,value.var="ADULTS", length)
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


#cast the data to count up the fireflies
CSTIG2<-dcast(CSTIG, year+TREAT_DESC+REPLICATE~.,value.var="ADULTS", sum)
#cast the data to count the traps
CSTIG3<-dcast(CSTIG, year+TREAT_DESC+REPLICATE~.,value.var="ADULTS", length)
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


# First make a dataframe for error bars
CSTIG_agg <- ddply(CSTIG_summary, .(TREAT_CAT, year), summarise,
                   mean.bugs = mean(pertrap),
                   sd.bugs.pt = sd(pertrap),
                   N = length(pertrap),
                   SE = sd.bugs.pt / sqrt(N))



######################################################################################

#CMAC




#cast the data to count up the fireflies
CMAC2<-dcast(CMAC, year+TREAT_DESC+REPLICATE~.,value.var="ADULTS", sum)
#cast the data to count the traps
CMAC3<-dcast(CMAC, year+TREAT_DESC+REPLICATE~.,value.var="ADULTS", length)
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


#cast the data to count up the fireflies
C72<-dcast(C7, year+TREAT_DESC+REPLICATE~.,value.var="ADULTS", sum)
#cast the data to count the traps
C73<-dcast(C7, year+TREAT_DESC+REPLICATE~.,value.var="ADULTS", length)
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



#cast the data to count up the fireflies
BURSI2<-dcast(BURSI, year+TREAT_DESC+REPLICATE~.,value.var="ADULTS", sum)
#cast the data to count the traps
BURSI3<-dcast(BURSI, year+TREAT_DESC+REPLICATE~.,value.var="ADULTS", length)
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



#cast the data to count up the fireflies
ABIPN2<-dcast(ABIPN, year+TREAT_DESC+REPLICATE~.,value.var="ADULTS", sum)
#cast the data to count the traps
ABIPN3<-dcast(ABIPN, year+TREAT_DESC+REPLICATE~.,value.var="ADULTS", length)
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
#get all this data back into a single dataframe

#create weighted by predation potential variable- use values based on Bahlai et al 2013
HAXY_summary$pred<-0.672*HAXY_summary$ADULTS
C7_summary$pred<-0.628*C7_summary$ADULTS
HVAR_summary$pred<-0.237*HVAR_summary$ADULTS
PQUA_summary$pred<-0.200*PQUA_summary$ADULTS
HPARN_summary$pred<-0.320*HPARN_summary$ADULTS
HGLAC_summary$pred<-0.440*HGLAC_summary$ADULTS
HCONV_summary$pred<-0.440*HCONV_summary$ADULTS
H13_summary$pred<-0.189*H13_summary$ADULTS
CYCSP_summary$pred<-0.384*CYCSP_summary$ADULTS
CTRIF_summary$pred<-0.320*CTRIF_summary$ADULTS
CSTIG_summary$pred<-0*CSTIG_summary$ADULTS
CMAC_summary$pred<-0.350*CMAC_summary$ADULTS
BURSI_summary$pred<-0.2*BURSI_summary$ADULTS #based on body size, no literature value found
ABIPN_summary$pred<-0.172*ABIPN_summary$ADULTS



invasive<-rbind(HAXY_summary, C7_summary,HVAR_summary, PQUA_summary)
native<-rbind(HPARN_summary,HGLAC_summary,HCONV_summary,H13_summary,CYCSP_summary,
              CTRIF_summary,CSTIG_summary, CMAC_summary,BURSI_summary,ABIPN_summary)
all_lb<-rbind(invasive, native)

invasivetot <- ddply(invasive, .(year, TREAT_DESC, TREAT_CAT, REPLICATE), summarise,
                   ADULTS = sum(ADULTS),
                   TRAPS=max(TRAPS))
invasivetot$pertrap<-invasivetot$ADULTS/invasivetot$TRAPS

nativetot <- ddply(native, .(year, TREAT_DESC, TREAT_CAT,  REPLICATE), summarise,
                      ADULTS = sum(ADULTS),
                      TRAPS=max(TRAPS))
nativetot$pertrap<-nativetot$ADULTS/nativetot$TRAPS

all_tot <- ddply(all_lb, .(year, TREAT_DESC, TREAT_CAT,  REPLICATE), summarise,
                 ADULTS = sum(ADULTS),
                 TRAPS=max(TRAPS))
all_tot$pertrap<-all_tot$ADULTS/all_tot$TRAPS

pred_tot <- ddply(all_lb, .(year, TREAT_DESC, TREAT_CAT,  REPLICATE), summarise,
                 pred = sum(pred),
                 TRAPS=max(TRAPS))
pred_tot$pertrap<-pred_tot$pred/pred_tot$TRAPS


######################################################################################

#now let's make a stacked timeseries figure with a GAM for decade for each of these species

#gam will include sampling correction but that's it- we want to see the raw 
#population trend for each species
library(mgcv)
library(visreg)
library(ggplot2)
library(tidymv)
library(grid)
library(ggbreak)
library(patchwork)

source("broken_window_script.R")

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
  geom_point(data=ABIPN_summary, aes(year, ADULTS), position = jitter, pch=21, size=1, fill="lightgrey", col="grey70")+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='paleturquoise', alpha=0.6)+
  geom_line()+
  theme_classic()+
  xlim(1993, 2023)+
  xlab(NULL)+ylab("capture trend")+
  scale_y_continuous(limits=c(-0.1, 5*mean(ABIPN_summary$ADULTS)+4))+ 
  annotation_custom(grob = rectGrob(gp = gpar(fill = "grey70", col = "black", alpha = 1)), 
                    xmin = -Inf, xmax = Inf, ymin = 0.87*(5*mean(ABIPN_summary$ADULTS)+4), ymax = 1.05*(5*mean(ABIPN_summary$ADULTS)+4)) +
  # Text on top of the grey box
  annotate("text", x = 2008, y = 0.95*(5*mean(ABIPN_summary$ADULTS)+4), label = "Adalia bipunctata", color = "black", size = 4, fontface = "italic", family="sans")

ABIPN.year

#######
#by plant community or (or community group)
ABIPN.gam1<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=TREAT_DESC)+offset(log(TRAPS)),
                data=ABIPN_summary, family="quasipoisson")
summary(ABIPN.gam1)

visreg(ABIPN.gam1, "year", "TREAT_DESC", ylab="capture trend", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_DESC, ncol = 4)

ABIPN.gam2<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=as.factor(TREAT_CAT))+offset(log(TRAPS)),
                data=ABIPN_summary, family="quasipoisson")
summary(ABIPN.gam2)

visreg(ABIPN.gam2, "year", "TREAT_CAT", ylab="capture trend", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_CAT, ncol = 4)


########Calculations for table #################
#last 10 year trend
ABIPN8<-subset(ABIPN_summary, year >= 2016)

abi8mod<-lm(pertrap~year, data=ABIPN8)

summary(abi8mod)

#whole timeseries trend

abiallmod<-lm(pertrap~year, data=ABIPN_summary)

summary(abiallmod)

#stability time

stability_time(ABIPN_summary[,c(1,6)])

#detection frequency in first five vs last five years


ddply(ABIPN_summary, .(year), summarise,
                     ADULTS = sum(ADULTS))



out<-ddply(ABIPN_summary, .(year), summarise,
           ADULTS = sum(ADULTS),
           TRAPS=sum(TRAPS))

out

sum(out$ADULTS)/sum(out$TRAPS)


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
  geom_point(data=BURSI_summary, aes(year, ADULTS), position = jitter, pch=21, size=1, fill="lightgrey", col="grey70")+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='paleturquoise', alpha=0.6)+
  geom_line()+
  theme_classic()+
  xlim(1993, 2023)+
  xlab(NULL)+ylab("capture trend")+
  scale_y_continuous(limits=c(-0.1, 5*mean(BURSI_summary$ADULTS)+4))+ 
  annotation_custom(grob = rectGrob(gp = gpar(fill = "grey70", col = "black", alpha = 1)), 
                    xmin = -Inf, xmax = Inf, ymin = 0.87*(5*mean(BURSI_summary$ADULTS)+4), ymax = 1.05*(5*mean(BURSI_summary$ADULTS)+4)) +
  # Text on top of the grey box
  annotate("text", x = 2008, y = 0.95*(5*mean(BURSI_summary$ADULTS)+4), label = "Brachiacantha ursina", color = "black", size = 4, fontface = "italic", family="sans")

BURSI.year

#######
#by plant community or (or community group)
BURSI.gam1<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=TREAT_DESC)+offset(log(TRAPS)),
                data=BURSI_summary, family="quasipoisson")
summary(BURSI.gam1)

visreg(BURSI.gam1, "year", "TREAT_DESC", ylab="capture trend", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_DESC, ncol = 4)

BURSI.gam2<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=as.factor(TREAT_CAT))+offset(log(TRAPS)),
                data=BURSI_summary, family="quasipoisson")
summary(BURSI.gam2)

visreg(BURSI.gam2, "year", "TREAT_CAT", ylab="capture trend", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_CAT, ncol = 4)

########Calculations for table #################
#last 8 year trend
BURSI8<-subset(BURSI_summary, year >= 2016)

BURSI8mod<-lm(pertrap~year, data=BURSI8)

summary(BURSI8mod)

#whole timeseries trend

BURSIallmod<-lm(pertrap~year, data=BURSI_summary)

summary(BURSIallmod)

#stability time

stability_time(BURSI_summary[,c(1,6)])

#detection frequency in first five vs last five years


ddply(BURSI_summary, .(year), summarise,
      ADULTS = sum(ADULTS))


out<-ddply(BURSI_summary, .(year), summarise,
           ADULTS = sum(ADULTS),
           TRAPS=sum(TRAPS))

out

sum(out$ADULTS)/sum(out$TRAPS)



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
  geom_point(data=C7_summary, aes(year, ADULTS), position = jitter, pch=21, size=1, fill="lightgrey", col="grey70")+
  geom_ribbon(aes(ymin=lower, ymax=upper),  fill='salmon1', alpha=0.6)+
  geom_line()+
  theme_classic()+
  xlim(1993, 2023)+
  xlab(NULL)+ylab("capture trend")+
  scale_y_continuous(limits=c(-0.1, 5*mean(C7_summary$ADULTS)+4))+ 
  annotation_custom(grob = rectGrob(gp = gpar(fill = "grey70", col = "black", alpha = 1)), 
                    xmin = -Inf, xmax = Inf, ymin = 0.87*(5*mean(C7_summary$ADULTS)+4), ymax = 1.05*(5*mean(C7_summary$ADULTS)+4)) +
  # Text on top of the grey box
  annotate("text", x = 2008, y = 0.95*(5*mean(C7_summary$ADULTS)+4), label = "Coccinella septempunctata", color = "black", size = 4, fontface = "italic", family="sans")


C7.year

#######
#by plant community or (or community group)
C7.gam1<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=TREAT_DESC)+offset(log(TRAPS)),
                data=C7_summary, family="quasipoisson")
summary(C7.gam1)

visreg(C7.gam1, "year", "TREAT_DESC", ylab="capture trend", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_DESC, ncol = 4)

C7.gam2<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=as.factor(TREAT_CAT))+offset(log(TRAPS)),
                data=C7_summary, family="quasipoisson")
summary(C7.gam2)

visreg(C7.gam2, "year", "TREAT_CAT", ylab="capture trend", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_CAT, ncol = 4)

########Calculations for table #################
#last 10 year trend
C78<-subset(C7_summary, year >= 2016)

C78mod<-lm(pertrap~year, data=C78)

summary(C78mod)

#whole timeseries trend

C7allmod<-lm(pertrap~year, data=C7_summary)

summary(C7allmod)

#stability time

stability_time(C7_summary[,c(1,6)])

#detection frequency in first five vs last five years


ddply(C7_summary, .(year), summarise,
      ADULTS = sum(ADULTS))

out<-ddply(C7_summary, .(year), summarise,
           ADULTS = sum(ADULTS),
           TRAPS=sum(TRAPS))

out

sum(out$ADULTS)/sum(out$TRAPS)



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
  geom_point(data=CMAC_summary, aes(year, ADULTS), position = jitter, pch=21, size=1, fill="lightgrey", col="grey70")+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='paleturquoise', alpha=0.6)+
  geom_line()+
  theme_classic()+
  xlim(1993, 2023)+
  xlab(NULL)+ylab("capture trend")+
  scale_y_continuous(limits=c(-0.1, 5*mean(CMAC_summary$ADULTS)+4))+ 
  annotation_custom(grob = rectGrob(gp = gpar(fill = "grey70", col = "black", alpha = 1)), 
                    xmin = -Inf, xmax = Inf, ymin = 0.87*(5*mean(CMAC_summary$ADULTS)+4), ymax = 1.05*(5*mean(CMAC_summary$ADULTS)+4)) +
  # Text on top of the grey box
  annotate("text", x = 2008, y = 0.95*(5*mean(CMAC_summary$ADULTS)+4), label = "Coleomegilla maculata", color = "black", size = 4, fontface = "italic", family="sans")

CMAC.year

#######
#by plant community or (or community group)
CMAC.gam1<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=TREAT_DESC)+offset(log(TRAPS)),
             data=CMAC_summary, family="quasipoisson")
summary(CMAC.gam1)

visreg(CMAC.gam1, "year", "TREAT_DESC", ylab="capture trend", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_DESC, ncol = 4)

CMAC.gam2<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=as.factor(TREAT_CAT))+offset(log(TRAPS)),
             data=CMAC_summary, family="quasipoisson")
summary(CMAC.gam2)

visreg(CMAC.gam2, "year", "TREAT_CAT", ylab="capture trend", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_CAT, ncol = 4)

########Calculations for table #################
#last 8 year trend
CMAC8<-subset(CMAC_summary, year >= 2016)

CMAC8mod<-lm(pertrap~year, data=CMAC8)

summary(CMAC8mod)

#whole timeseries trend

CMACallmod<-lm(pertrap~year, data=CMAC_summary)

summary(CMACallmod)

#stability time

stability_time(CMAC_summary[,c(1,6)])

#detection frequency in first five vs last five years


ddply(CMAC_summary, .(year), summarise,
      ADULTS = sum(ADULTS))

out<-ddply(CMAC_summary, .(year), summarise,
           ADULTS = sum(ADULTS),
           TRAPS=sum(TRAPS))

out

sum(out$ADULTS)/sum(out$TRAPS)

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
  geom_point(data=CSTIG_summary, aes(year, ADULTS), position = jitter, pch=21, size=1, fill="lightgrey", col="grey70")+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='paleturquoise', alpha=0.6)+
  geom_line()+
  theme_classic()+
  xlim(1993, 2023)+
  xlab(NULL)+ylab("capture trend")+
  scale_y_continuous(limits=c(-0.1, 5*mean(CSTIG_summary$ADULTS)+4), breaks = seq(0, 5*mean(CSTIG_summary$ADULTS)+4, by = 1))+
  annotation_custom(grob = rectGrob(gp = gpar(fill = "grey70", col = "black", alpha = 1)), 
                    xmin = -Inf, xmax = Inf, ymin = 0.87*(5*mean(CSTIG_summary$ADULTS)+4), ymax = 1.05*(5*mean(CSTIG_summary$ADULTS)+4)) +
  # Text on top of the grey box
  annotate("text", x = 2008, y = 0.95*(5*mean(CSTIG_summary$ADULTS)+4), label = "Chilochorus stigma", color = "black", size = 4, fontface = "italic", family="sans")

CSTIG.year

#######
#by plant community or (or community group)
CSTIG.gam1<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=TREAT_DESC)+offset(log(TRAPS)),
               data=CSTIG_summary, family="quasipoisson")
summary(CSTIG.gam1)

visreg(CSTIG.gam1, "year", "TREAT_DESC", ylab="capture trend", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_DESC, ncol = 4)

CSTIG.gam2<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=as.factor(TREAT_CAT))+offset(log(TRAPS)),
               data=CSTIG_summary, family="quasipoisson")
summary(CSTIG.gam2)

visreg(CSTIG.gam2, "year", "TREAT_CAT", ylab="capture trend", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_CAT, ncol = 4)

########Calculations for table #################
#last 8 year trend
CSTIG8<-subset(CSTIG_summary, year >= 2016)

CSTIG8mod<-lm(pertrap~year, data=CSTIG8)

summary(CSTIG8mod)

#whole timeseries trend

CSTIGallmod<-lm(pertrap~year, data=CSTIG_summary)

summary(CSTIGallmod)

#stability time

stability_time(CSTIG_summary[,c(1,6)])

#detection frequency in first five vs last five years


ddply(CSTIG_summary, .(year), summarise,
      ADULTS = sum(ADULTS))

out<-ddply(CSTIG_summary, .(year), summarise,
           ADULTS = sum(ADULTS),
           TRAPS=sum(TRAPS))

out

sum(out$ADULTS)/sum(out$TRAPS)


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
  geom_point(data=CTRIF_summary, aes(year, ADULTS), position = jitter, pch=21, size=1, fill="lightgrey", col="grey70")+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='paleturquoise', alpha=0.6)+
  geom_line()+
  theme_classic()+
  xlim(1993, 2023)+
  xlab(NULL)+ylab("capture trend")+
  scale_y_continuous(limits=c(-0.1, 5*mean(CTRIF_summary$ADULTS)+4))+ 
  annotation_custom(grob = rectGrob(gp = gpar(fill = "grey70", col = "black", alpha = 1)), 
                    xmin = -Inf, xmax = Inf, ymin = 0.87*(5*mean(CTRIF_summary$ADULTS)+4), ymax = 1.05*(5*mean(CTRIF_summary$ADULTS)+4)) +
  # Text on top of the grey box
  annotate("text", x = 2008, y = 0.95*(5*mean(CTRIF_summary$ADULTS)+4), label = "Coccinella trifasciata", color = "black", size = 4, fontface = "italic", family="sans")

CTRIF.year

#######
#by plant community or (or community group)
CTRIF.gam1<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=TREAT_DESC)+offset(log(TRAPS)),
                data=CTRIF_summary, family="quasipoisson")
summary(CTRIF.gam1)

visreg(CTRIF.gam1, "year", "TREAT_DESC", ylab="capture trend", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_DESC, ncol = 4)

CTRIF.gam2<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=as.factor(TREAT_CAT))+offset(log(TRAPS)),
                data=CTRIF_summary, family="quasipoisson")
summary(CTRIF.gam2)

visreg(CTRIF.gam2, "year", "TREAT_CAT", ylab="capture trend", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_CAT, ncol = 4)

########Calculations for table #################
#last 8 year trend
CTRIF8<-subset(CTRIF_summary, year >= 2016)

CTRIF8mod<-lm(pertrap~year, data=CTRIF8)

summary(CTRIF8mod)

#whole timeseries trend

CTRIFallmod<-lm(pertrap~year, data=CTRIF_summary)

summary(CTRIFallmod)

#stability time

stability_time(CTRIF_summary[,c(1,6)])

#detection frequency in first five vs last five years


ddply(CTRIF_summary, .(year), summarise,
      ADULTS = sum(ADULTS))


out<-ddply(CTRIF_summary, .(year), summarise,
           ADULTS = sum(ADULTS),
           TRAPS=sum(TRAPS))

out

sum(out$ADULTS)/sum(out$TRAPS)
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
  geom_point(data=CYCSP_summary, aes(year, ADULTS), position = jitter, pch=21, size=1, fill="lightgrey", col="grey70")+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='paleturquoise', alpha=0.6)+
  geom_line()+
  theme_classic()+
  xlim(1993, 2023)+
  xlab(NULL)+ylab("capture trend")+
  scale_y_continuous(limits=c(-0.1, 5*mean(CYCSP_summary$ADULTS)+4), breaks = seq(0, 5*mean(CYCSP_summary$ADULTS)+4, by = 2))+ 
  annotation_custom(grob = rectGrob(gp = gpar(fill = "grey70", col = "black", alpha = 1)), 
                    xmin = -Inf, xmax = Inf, ymin = 0.87*(5*mean(CYCSP_summary$ADULTS)+4), ymax = 1.05*(5*mean(CYCSP_summary$ADULTS)+4)) +
  # Text on top of the grey box
  annotate("text", x = 2008, y = 0.95*(5*mean(CYCSP_summary$ADULTS)+4), label = "Cycloneda munda", color = "black", size = 4, fontface = "italic", family="sans")

CYCSP.year

#######
#by plant community or (or community group)
CYCSP.gam1<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=TREAT_DESC)+offset(log(TRAPS)),
                data=CYCSP_summary, family="quasipoisson")
summary(CYCSP.gam1)

visreg(CYCSP.gam1, "year", "TREAT_DESC", ylab="capture trend", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_DESC, ncol = 4)

CYCSP.gam2<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=as.factor(TREAT_CAT))+offset(log(TRAPS)),
                data=CYCSP_summary, family="quasipoisson")
summary(CYCSP.gam2)

visreg(CYCSP.gam2, "year", "TREAT_CAT", ylab="capture trend", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_CAT, ncol = 4)


########Calculations for table #################
#last 8 year trend
CYCSP8<-subset(CYCSP_summary, year >= 2016)

CYCSP8mod<-lm(pertrap~year, data=CYCSP8)

summary(CYCSP8mod)

#whole timeseries trend

CYCSPallmod<-lm(pertrap~year, data=CYCSP_summary)

summary(CYCSPallmod)

#stability time

stability_time(CYCSP_summary[,c(1,6)])

#detection frequency in first five vs last five years


ddply(CYCSP_summary, .(year), summarise,
      ADULTS = sum(ADULTS))

out<-ddply(CYCSP_summary, .(year), summarise,
           ADULTS = sum(ADULTS),
           TRAPS=sum(TRAPS))

out

sum(out$ADULTS)/sum(out$TRAPS)

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
  geom_point(data=H13_summary, aes(year, ADULTS), position = jitter, pch=21, size=1, fill="lightgrey", col="grey70")+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='paleturquoise', alpha=0.6)+
  geom_line()+
  theme_classic()+
  xlim(1993, 2023)+
  xlab(NULL)+ylab("capture trend")+
  scale_y_continuous(limits=c(-0.1, 5*mean(H13_summary$ADULTS)+4))+ 
  annotation_custom(grob = rectGrob(gp = gpar(fill = "grey70", col = "black", alpha = 1)), 
                    xmin = -Inf, xmax = Inf, ymin = 0.87*(5*mean(H13_summary$ADULTS)+4), ymax = 1.05*(5*mean(H13_summary$ADULTS)+4)) +
  # Text on top of the grey box
  annotate("text", x = 2008, y = 0.95*(5*mean(H13_summary$ADULTS)+4), label = "Hippodamia tredecimpunctata", color = "black", size = 4, fontface = "italic", family="sans")

H13.year

#######
#by plant community or (or community group)
H13.gam1<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=TREAT_DESC)+offset(log(TRAPS)),
                data=H13_summary, family="quasipoisson")
summary(H13.gam1)

visreg(H13.gam1, "year", "TREAT_DESC", ylab="capture trend", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_DESC, ncol = 4)

H13.gam2<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=as.factor(TREAT_CAT))+offset(log(TRAPS)),
                data=H13_summary, family="quasipoisson")
summary(H13.gam2)

visreg(H13.gam2, "year", "TREAT_CAT", ylab="capture trend", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_CAT, ncol = 4)

########Calculations for table #################
#last 8 year trend
H138<-subset(H13_summary, year >= 2016)

H138mod<-lm(pertrap~year, data=H138)

summary(H138mod)

#whole timeseries trend

H13allmod<-lm(pertrap~year, data=H13_summary)

summary(H13allmod)

#stability time

#stability_time(H13_summary[,c(1,6)]) # not run- too many zeroes 

#detection frequency in first five vs last five years


ddply(H13_summary, .(year), summarise,
      ADULTS = sum(ADULTS))

out<-ddply(H13_summary, .(year), summarise,
           ADULTS = sum(ADULTS),
           TRAPS=sum(TRAPS))

out

sum(out$ADULTS)/sum(out$TRAPS)

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
  geom_point(data=HAXY_summary, aes(year, ADULTS), position = jitter, pch=21, size=1, fill="lightgrey", col="grey70")+
  geom_ribbon(aes(ymin=lower, ymax=upper),  fill='salmon1', alpha=0.6)+
  geom_line()+
  theme_classic()+
  xlim(1993, 2023)+
  xlab(NULL)+ylab("capture trend")+
  scale_y_continuous(limits=c(-0.1, 5*mean(HAXY_summary$ADULTS)+4))+ 
  annotation_custom(grob = rectGrob(gp = gpar(fill = "grey70", col = "black", alpha = 1)), 
                    xmin = -Inf, xmax = Inf, ymin = 0.87*(5*mean(HAXY_summary$ADULTS)+4), ymax = 1.05*(5*mean(HAXY_summary$ADULTS)+4)) +
  # Text on top of the grey box
  annotate("text", x = 2008, y = 0.95*(5*mean(HAXY_summary$ADULTS)+4), label = "Harmonia axyridis", color = "black", size = 4, fontface = "italic", family="sans")

HAXY.year


#######
#by plant community or (or community group)
HAXY.gam1<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=TREAT_DESC)+offset(log(TRAPS)),
              data=HAXY_summary, family="quasipoisson")
summary(HAXY.gam1)

visreg(HAXY.gam1, "year", "TREAT_DESC", ylab="capture trend", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_DESC, ncol = 4)

HAXY.gam2<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=as.factor(TREAT_CAT))+offset(log(TRAPS)),
              data=HAXY_summary, family="quasipoisson")
summary(HAXY.gam2)

visreg(HAXY.gam2, "year", "TREAT_CAT", ylab="capture trend", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_CAT, ncol = 4)

########Calculations for table #################
#last 8 year trend
HAXY8<-subset(HAXY_summary, year >= 2016)

HAXY8mod<-lm(pertrap~year, data=HAXY8)

summary(HAXY8mod)

#whole timeseries trend

HAXYallmod<-lm(pertrap~year, data=HAXY_summary)

summary(HAXYallmod)

#stability time

stability_time(HAXY_summary[,c(1,6)])

#detection frequency in first five vs last five years


ddply(HAXY_summary, .(year), summarise,
      ADULTS = sum(ADULTS))

out<-ddply(HAXY_summary, .(year), summarise,
           ADULTS = sum(ADULTS),
           TRAPS=sum(TRAPS))

out

sum(out$ADULTS)/sum(out$TRAPS)
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
  geom_point(data=HCONV_summary, aes(year, ADULTS), position = jitter, pch=21, size=1, fill="lightgrey", col="grey70")+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='paleturquoise', alpha=0.6)+
  geom_line()+
  theme_classic()+
  xlim(1993, 2023)+
  xlab(NULL)+ylab("capture trend")+
  scale_y_continuous(limits=c(-0.1, 5*mean(HCONV_summary$ADULTS)+4))+ 
  annotation_custom(grob = rectGrob(gp = gpar(fill = "grey70", col = "black", alpha = 1)), 
                    xmin = -Inf, xmax = Inf, ymin = 0.87*(5*mean(HCONV_summary$ADULTS)+4), ymax = 1.05*(5*mean(HCONV_summary$ADULTS)+4)) +
  # Text on top of the grey box
  annotate("text", x = 2008, y = 0.95*(5*mean(HCONV_summary$ADULTS)+4), label = "Hippodamia convergens", color = "black", size = 4, fontface = "italic", family="sans")

HCONV.year

#######
#by plant community or (or community group)
HCONV.gam1<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=TREAT_DESC)+offset(log(TRAPS)),
               data=HCONV_summary, family="quasipoisson")
summary(HCONV.gam1)

visreg(HCONV.gam1, "year", "TREAT_DESC", ylab="capture trend", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_DESC, ncol = 4)

HCONV.gam2<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=as.factor(TREAT_CAT))+offset(log(TRAPS)),
               data=HCONV_summary, family="quasipoisson")
summary(HCONV.gam2)

visreg(HCONV.gam2, "year", "TREAT_CAT", ylab="capture trend", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_CAT, ncol = 4)

########Calculations for table #################
#last 8 year trend
HCONV8<-subset(HCONV_summary, year >= 2016)

HCONV8mod<-lm(pertrap~year, data=HCONV8)

summary(HCONV8mod)

#whole timeseries trend

HCONVallmod<-lm(pertrap~year, data=HCONV_summary)

summary(HCONVallmod)

#stability time

#stability_time(HCONV_summary[,c(1,6)])

#detection frequency in first five vs last five years


ddply(HCONV_summary, .(year), summarise,
      ADULTS = sum(ADULTS))

out<-ddply(HCONV_summary, .(year), summarise,
           ADULTS = sum(ADULTS),
           TRAPS=sum(TRAPS))

out

sum(out$ADULTS)/sum(out$TRAPS)

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
  geom_point(data=HGLAC_summary, aes(year, ADULTS), position = jitter, pch=21, size=1, fill="lightgrey", col="grey70")+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='paleturquoise', alpha=0.6)+
  geom_line()+
  theme_classic()+
  xlim(1993, 2023)+
  xlab(NULL)+ylab("capture trend")+
  scale_y_continuous(limits=c(-0.1, 5*mean(HGLAC_summary$ADULTS)+4))+ 
  annotation_custom(grob = rectGrob(gp = gpar(fill = "grey70", col = "black", alpha = 1)), 
                    xmin = -Inf, xmax = Inf, ymin = 0.87*(5*mean(HGLAC_summary$ADULTS)+4), ymax = 1.05*(5*mean(HGLAC_summary$ADULTS)+4)) +
  # Text on top of the grey box
  annotate("text", x = 2008, y = 0.95*(5*mean(HGLAC_summary$ADULTS)+4), label = "Hippodamia glacialis", color = "black", size = 4, fontface = "italic", family="sans")

HGLAC.year

#######
#by plant community or (or community group)
HGLAC.gam1<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=TREAT_DESC)+offset(log(TRAPS)),
                data=HGLAC_summary, family="quasipoisson")
summary(HGLAC.gam1)

visreg(HGLAC.gam1, "year", "TREAT_DESC", ylab="capture trend", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_DESC, ncol = 4)

HGLAC.gam2<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=as.factor(TREAT_CAT))+offset(log(TRAPS)),
                data=HGLAC_summary, family="quasipoisson")
summary(HGLAC.gam2)

visreg(HGLAC.gam2, "year", "TREAT_CAT", ylab="capture trend", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_CAT, ncol = 4)

########Calculations for table #################
#last 8 year trend
HGLAC8<-subset(HGLAC_summary, year >= 2016)

HGLAC8mod<-lm(pertrap~year, data=HGLAC8)

summary(HGLAC8mod)

#whole timeseries trend

HGLACallmod<-lm(pertrap~year, data=HGLAC_summary)

summary(HGLACallmod)

#stability time

stability_time(HGLAC_summary[,c(1,6)])

#detection frequency in first five vs last five years


ddply(HGLAC_summary, .(year), summarise,
      ADULTS = sum(ADULTS))

out<-ddply(HGLAC_summary, .(year), summarise,
           ADULTS = sum(ADULTS),
           TRAPS=sum(TRAPS))

out

sum(out$ADULTS)/sum(out$TRAPS)
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
  geom_point(data=HPARN_summary, aes(year, ADULTS), position = jitter, pch=21, size=1, fill="lightgrey", col="grey70")+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='paleturquoise', alpha=0.6)+
  geom_line()+
  theme_classic()+
  xlim(1993, 2023)+
  xlab(NULL)+ylab("capture trend")+
  scale_y_continuous(limits=c(-0.1, 5*mean(HPARN_summary$ADULTS)+4))+ 
  annotation_custom(grob = rectGrob(gp = gpar(fill = "grey70", col = "black", alpha = 1)), 
                    xmin = -Inf, xmax = Inf, ymin = 0.87*(5*mean(HPARN_summary$ADULTS)+4), ymax = 1.05*(5*mean(HPARN_summary$ADULTS)+4)) +
  # Text on top of the grey box
  annotate("text", x = 2008, y = 0.95*(5*mean(HPARN_summary$ADULTS)+4), label = "Hippodamia parenthesis", color = "black", size = 4, fontface = "italic", family="sans")

HPARN.year

#######
#by plant community or (or community group)
HPARN.gam1<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=TREAT_DESC)+offset(log(TRAPS)),
                data=HPARN_summary, family="quasipoisson")
summary(HPARN.gam1)

visreg(HPARN.gam1, "year", "TREAT_DESC", ylab="capture trend", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_DESC, ncol = 4)

HPARN.gam2<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=as.factor(TREAT_CAT))+offset(log(TRAPS)),
                data=HPARN_summary, family="quasipoisson")
summary(HPARN.gam2)

visreg(HPARN.gam2, "year", "TREAT_CAT", ylab="capture trend", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_CAT, ncol = 4)

########Calculations for table #################
#last 8 year trend
HPARN8<-subset(HPARN_summary, year >= 2016)

HPARN8mod<-lm(pertrap~year, data=HPARN8)

summary(HPARN8mod)

#whole timeseries trend

HPARNallmod<-lm(pertrap~year, data=HPARN_summary)

summary(HPARNallmod)

#stability time

stability_time(HPARN_summary[,c(1,6)])

#detection frequency in first five vs last five years


ddply(HPARN_summary, .(year), summarise,
      ADULTS = sum(ADULTS))

out<-ddply(HPARN_summary, .(year), summarise,
           ADULTS = sum(ADULTS),
           TRAPS=sum(TRAPS))

out

sum(out$ADULTS)/sum(out$TRAPS)

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
  geom_point(data=HVAR_summary, aes(year, ADULTS), position = jitter, pch=21, size=1, fill="lightgrey", col="grey70")+
  geom_ribbon(aes(ymin=lower, ymax=upper),  fill='salmon1', alpha=0.6)+
  geom_line()+
  theme_classic()+
  xlim(1993, 2023)+
  xlab(NULL)+ylab("capture trend")+
  scale_y_continuous(limits=c(-0.1, 5*mean(HVAR_summary$ADULTS)+4), breaks = seq(0, 5*mean(HVAR_summary$ADULTS)+4, by = 2))+ 
  annotation_custom(grob = rectGrob(gp = gpar(fill = "grey70", col = "black", alpha = 1)), 
                    xmin = -Inf, xmax = Inf, ymin = 0.87*(5*mean(HVAR_summary$ADULTS)+4), ymax = 1.05*(5*mean(HVAR_summary$ADULTS)+4)) +
  # Text on top of the grey box
  annotate("text", x = 2008, y = 0.95*(5*mean(HVAR_summary$ADULTS)+4), label = "Hippodamia variegata", color = "black", size = 4, fontface = "italic", family="sans")

HVAR.year

#######
#by plant community or (or community group)
HVAR.gam1<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=TREAT_DESC)+offset(log(TRAPS)),
                data=HVAR_summary, family="quasipoisson")
summary(HVAR.gam1)

visreg(HVAR.gam1, "year", "TREAT_DESC", ylab="capture trend", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_DESC, ncol = 4)

HVAR.gam2<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=as.factor(TREAT_CAT))+offset(log(TRAPS)),
                data=HVAR_summary, family="quasipoisson")
summary(HVAR.gam2)

visreg(HVAR.gam2, "year", "TREAT_CAT", ylab="capture trend", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_CAT, ncol = 4)

########Calculations for table #################
#last 8 year trend
HVAR8<-subset(HVAR_summary, year >= 2016)

HVAR8mod<-lm(pertrap~year, data=HVAR8)

summary(HVAR8mod)

#whole timeseries trend

HVARallmod<-lm(pertrap~year, data=HVAR_summary)

summary(HVARallmod)

#stability time

stability_time(HVAR_summary[,c(1,6)])

#detection frequency in first five vs last five years


ddply(HVAR_summary, .(year), summarise,
      ADULTS = sum(ADULTS))


out<-ddply(HVAR_summary, .(year), summarise,
           ADULTS = sum(ADULTS),
           TRAPS=sum(TRAPS))

out

sum(out$ADULTS)/sum(out$TRAPS)
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
  geom_point(data=PQUA_summary, aes(year, ADULTS), position = jitter, pch=21, size=1, fill="lightgrey", col="grey70")+
  geom_ribbon(aes(ymin=lower, ymax=upper),  fill='salmon1', alpha=0.6)+
  geom_line()+
    theme_classic()+
  xlim(1993, 2023)+
  xlab(NULL)+ylab("capture trend")+
  scale_y_continuous(limits=c(-0.1, 5*mean(PQUA_summary$ADULTS)+4))+ 
  annotation_custom(grob = rectGrob(gp = gpar(fill = "grey70", col = "black", alpha = 1)), 
                    xmin = -Inf, xmax = Inf, ymin = 0.87*(5*mean(PQUA_summary$ADULTS)+4), ymax = 1.05*(5*mean(PQUA_summary$ADULTS)+4)) +
  # Text on top of the grey box
  annotate("text", x = 2008, y = 0.95*(5*mean(PQUA_summary$ADULTS)+4), label = "Propylea quatuordecimpunctata", color = "black", size = 4, fontface = "italic", family="sans")

PQUA.year

#######
#by plant community or (or community group)
PQUA.gam1<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=TREAT_DESC)+offset(log(TRAPS)),
               data=PQUA_summary, family="quasipoisson")
summary(PQUA.gam1)

visreg(PQUA.gam1, "year", "TREAT_DESC", ylab="capture trend", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_DESC, ncol = 4)

PQUA.gam2<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=as.factor(TREAT_CAT))+offset(log(TRAPS)),
               data=PQUA_summary, family="quasipoisson")
summary(PQUA.gam2)

visreg(PQUA.gam2, "year", "TREAT_CAT", ylab="capture trend", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_CAT, ncol = 4)


########Calculations for table #################
#last 8 year trend
PQUA8<-subset(PQUA_summary, year >= 2016)

PQUA8mod<-lm(pertrap~year, data=PQUA8)

summary(PQUA8mod)

#whole timeseries trend

PQUAallmod<-lm(pertrap~year, data=PQUA_summary)

summary(PQUAallmod)

#stability time

stability_time(PQUA_summary[,c(1,6)])

#detection frequency in first five vs last five years


ddply(PQUA_summary, .(year), summarise,
      ADULTS = sum(ADULTS))
out<-ddply(PQUA_summary, .(year), summarise,
           ADULTS = sum(ADULTS),
           TRAPS=sum(TRAPS))

out

sum(out$ADULTS)/sum(out$TRAPS)

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
  geom_vline(xintercept=c(2000.5, 2005.5, 2015.5), colour="cornsilk4", linetype="longdash")+ 
  geom_point(data=native, aes(year, ADULTS), position = jitter, pch=21, size=1, fill="lightgrey", col="grey70")+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='paleturquoise', alpha=0.6)+
  geom_line()+
  theme_classic()+
  xlim(1993, 2023)+
  xlab(NULL)+ylab("capture trend")+
  scale_y_continuous(limits=c(-0.1, 60))+ 
  annotation_custom(grob = rectGrob(gp = gpar(fill = "grey70", col = "black", alpha = 1)), 
    xmin = -Inf, xmax = Inf, ymin = 56, ymax = 63) +
  # Text on top of the grey box
  annotate("text", x = 2008, y = 59, label = "Native species", color = "black", size = 4, fontface = "plain", family="sans")



native.year

#######
#by plant community or (or community group)
nativetot.gam1<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=TREAT_DESC)+offset(log(TRAPS)),
                data=nativetot, family="quasipoisson")
summary(nativetot.gam1)

visreg(nativetot.gam1, "year", "TREAT_DESC", ylab="capture trend", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_DESC, ncol = 4)

nativetot.gam2<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=as.factor(TREAT_CAT))+offset(log(TRAPS)),
                data=nativetot, family="quasipoisson")
summary(nativetot.gam2)

visreg(nativetot.gam2, "year", "TREAT_CAT", ylab="capture trend", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_CAT, ncol = 4)



########Calculations for table #################


#whole timeseries trend

nativeallmod<-lm(pertrap~year, data=nativetot)

summary(nativeallmod)

nativestartall<-coef(nativeallmod)[1] + coef(nativeallmod)[2] * 1993
nativemeanall<-sum(nativetot$ADULTS)/sum(nativetot$TRAPS)

nativeall_perc_change<-100*nativeallmod$coefficients[2]

nativeall_perc_change

mean_textall <- round(nativeall_perc_change, 1)


#detection frequency in first five vs last five years


out<-ddply(nativetot, .(year), summarise,
           ADULTS = sum(ADULTS),
           TRAPS=sum(TRAPS))

out

sum(out$ADULTS)/sum(out$TRAPS)

#last 8 year trend
native8<-subset(nativetot, year >= 2016)

out<-ddply(native8, .(year), summarise,
           ADULTS = sum(ADULTS),
           TRAPS=sum(TRAPS))

native8mod<-lm(pertrap~year, data=native8)

summary(native8mod)

nativestart8<-coef(native8mod)[1] + coef(native8mod)[2] * 2019
nativemean8<-sum(out$ADULTS)/sum(out$TRAPS)

native8_perc_change<-100*native8mod$coefficients[2]

native8_perc_change

######
#4-phase analysis and figure

#until 2000
nativefirst<-subset(nativetot, year <= 2000)

nativefirstmod<-lm(pertrap~year, data=nativefirst)

summary(nativefirstmod)

out<-ddply(nativefirst, .(year), summarise,
           ADULTS = sum(ADULTS),
           TRAPS=sum(TRAPS))

nativefirstmean<-sum(out$ADULTS)/sum(out$TRAPS)

nativefirststart<-coef(nativefirstmod)[1] + coef(nativefirstmod)[2] * 1993

nativefirst_perc_change<-100*nativefirstmod$coefficients[2]
mean_textfirst <- round(nativefirst_perc_change, 1)

end_firstx <- 2000.5
end_nativefirsty <- coef(nativefirstmod)[1] + coef(nativefirstmod)[2] * end_firstx



# 2001-2005
nativesecond<-subset(nativetot, year >= 2000 & year<=2005)

nativesecondmod<-lm(pertrap~year, data=nativesecond)

summary(nativesecondmod)

out<-ddply(nativesecond, .(year), summarise,
           ADULTS = sum(ADULTS),
           TRAPS=sum(TRAPS))

nativesecondmean<-sum(out$ADULTS)/sum(out$TRAPS)

nativesecondstart<-coef(nativesecondmod)[1] + coef(nativesecondmod)[2] * 2001

nativesecond_perc_change<-100*nativesecondmod$coefficients[2]
mean_textsecond <- round(nativesecond_perc_change, 1)

end_secondx <- 2005.5
end_nativesecondy <- coef(nativesecondmod)[1] + coef(nativesecondmod)[2] * end_secondx


# 2006-2015
nativethird<-subset(nativetot, year >= 2006 & year<=2015)

nativethirdmod<-lm(pertrap~year, data=nativethird)

summary(nativethirdmod)

out<-ddply(nativethird, .(year), summarise,
           ADULTS = sum(ADULTS),
           TRAPS=sum(TRAPS))

nativethirdmean<-sum(out$ADULTS)/sum(out$TRAPS)

nativethirdstart<-coef(nativethirdmod)[1] + coef(nativethirdmod)[2] * 2006

nativethird_perc_change<-100*nativethirdmod$coefficients[2]
mean_textthird <- round(nativethird_perc_change, 1)
mean_textthird <-"0.0"

end_thirdx <- 2015.5
end_nativethirdy <- coef(nativethirdmod)[1] + coef(nativethirdmod)[2] * end_thirdx

# 2016-2023
nativefourth<-subset(nativetot, year >= 2016)

nativefourthmod<-lm(pertrap~year, data=nativefourth)

summary(nativefourthmod)

out<-ddply(nativefourth, .(year), summarise,
           ADULTS = sum(ADULTS),
           TRAPS=sum(TRAPS))

nativefourthmean<-sum(out$ADULTS)/sum(out$TRAPS)

nativefourthstart<-coef(nativefourthmod)[1] + coef(nativefourthmod)[2] * 2016

nativefourth_perc_change<-100*nativefourthmod$coefficients[2]
mean_textfourth <- round(nativefourth_perc_change, 1)

end_fourthx <- 2023
end_nativefourthy <- coef(nativefourthmod)[1] + coef(nativefourthmod)[2] * end_fourthx


native_trendplot<-ggplot(data=nativetot, aes(year, pertrap))+
  geom_vline(xintercept=c(2000.5, 2005.5, 2015.5), colour="cornsilk4", linetype="longdash")+
  #shaded rectangles at bottom
  #2000 segment
  annotate('rect', xmin=1993, ymin=0, xmax=2000.5, ymax=nativefirstmean, fill="paleturquoise", alpha=0.5)+
  #2005 segment
  annotate('rect', xmin=2000.5, ymin=0, xmax=2005.5, ymax=nativesecondmean, fill="paleturquoise", alpha=0.5)+
  #2015 segment
  annotate('rect', xmin=2005.5, ymin=0, xmax=2015.5, ymax=nativethirdmean, fill="paleturquoise", alpha=0.5)+
  #2023 segment
  annotate('rect', xmin=2015.5, ymin=0, xmax=2023, ymax=nativefourthmean, fill="paleturquoise", alpha=0.5)+
  #then overall average line
  #overall segment
  annotate('segment', x=1993, y=nativemeanall, xend=2023, yend=nativemeanall, color="darkturquoise",
           lty="twodash", size=1)+
  
  #then regression lines per segment
  #2000 segment
  annotate('segment', x = 1993, y = coef(nativefirstmod)[1] + coef(nativefirstmod)[2] * 1993, 
           xend = end_firstx, yend = end_nativefirsty, color="black", lty="dashed") +
  #2005 segment
  annotate('segment', x = 2000.5, y = coef(nativesecondmod)[1] + coef(nativesecondmod)[2] * 2000.5, 
           xend = end_secondx, yend = end_nativesecondy, color=NULL, lty="dashed") +
  #2015 segment
  annotate('segment', x = 2005.5, y = coef(nativethirdmod)[1] + coef(nativethirdmod)[2] * 2005.5, 
           xend = end_thirdx, yend = end_nativethirdy, color=NULL, lty="dashed") +
  #2023 segment
  annotate('segment', x = 2015.5, y = coef(nativefourthmod)[1] + coef(nativefourthmod)[2] * 2015.5, 
           xend = end_fourthx, yend = end_nativefourthy, color="black", lty="dashed") +
  
  #then overall regression
  #overall segment
  annotate('segment', x = 1993, y = coef(nativeallmod)[1] + coef(nativeallmod)[2] * 1993, 
           xend = 2023, yend = coef(nativeallmod)[1] + coef(nativeallmod)[2] * 2023, color="black", lty="solid", ) +
  
  #then percent change annotation labels
  #2000 segment
  annotate('text', x = (1993 + 2000.5) / 2, y = 0.05, 
           label = mean_textfirst,
           size = 4, 
           color = "black") + 
  #2005 segment
  annotate('text', x = (2000.5+2005.5) / 2, y = 0.05, 
           label = mean_textsecond,
           size = 4, 
           color = NULL) +
  #2015 segment
  annotate('text', x = (2005.5+2015.5) / 2, y = 0.05, 
           label = mean_textthird,
           size = 4, 
           color = NULL) + 
  #2023 segment
  annotate('text', x = (2015.5+2023) / 2, y = 0.05, 
           label = mean_textfourth,
           size = 4, 
           color = "black") + 
  #overall segment
  annotate('text', x = 2012, y = 0.4,
           label = mean_textall,
           size = 7, 
           color = "black") + 
  
  theme_classic()+
  xlim(1993, 2023)+
  xlab(NULL)+ylab("per trap")+
  coord_cartesian(ylim=c(0, 1), clip = "off")
native_trendplot


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
  geom_vline(xintercept=c(2000.5, 2005.5, 2015.5), colour="cornsilk4", linetype="longdash")+ 
  geom_point(data=invasive, aes(year, ADULTS), position = jitter, pch=21, size=1, fill="lightgrey", col="grey70")+
  geom_ribbon(aes(ymin=lower, ymax=upper),  fill='salmon1', alpha=0.6)+
  geom_line()+
  theme_classic()+
  xlim(1993, 2023)+
  xlab(NULL)+ylab("capture trend")+
  scale_y_continuous(limits=c(-0.1, 60))+ 
  annotation_custom(grob = rectGrob(gp = gpar(fill = "grey70", col = "black", alpha = 1)), 
                    xmin = -Inf, xmax = Inf, ymin = 56, ymax = 63) +
  # Text on top of the grey box
  annotate("text", x = 2008, y = 59, label = "Exotic species", color = "black", size = 4, fontface = "plain", family="sans")
invasive.year



#######
#by plant community or (or community group)
invasivetot.gam1<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=TREAT_DESC)+offset(log(TRAPS)),
                      data=invasivetot, family="quasipoisson")
summary(invasivetot.gam1)

visreg(invasivetot.gam1, "year", "TREAT_DESC", ylab="capture trend", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_DESC, ncol = 4)

invasivetot.gam2<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=as.factor(TREAT_CAT))+offset(log(TRAPS)),
                      data=invasivetot, family="quasipoisson")
summary(invasivetot.gam2)

visreg(invasivetot.gam2, "year", "TREAT_CAT", ylab="capture trend", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_CAT, ncol = 4)




#whole timeseries trend

invasiveallmod<-lm(pertrap~year, data=invasivetot)

summary(invasiveallmod)

invasivestartall<-coef(invasiveallmod)[1] + coef(invasiveallmod)[2] * 1993
invasivemeanall<-sum(invasivetot$ADULTS)/sum(invasivetot$TRAPS)

invasiveall_perc_change<-100*invasiveallmod$coefficients[2]

invasiveall_perc_change

mean_textall <- round(invasiveall_perc_change, 1)


#detection frequency in first five vs last five years


out<-ddply(invasivetot, .(year), summarise,
           ADULTS = sum(ADULTS),
           TRAPS=sum(TRAPS))

out

sum(out$ADULTS)/sum(out$TRAPS)

#last 8 year trend
invasive8<-subset(invasivetot, year >= 2016)

out<-ddply(invasive8, .(year), summarise,
           ADULTS = sum(ADULTS),
           TRAPS=sum(TRAPS))

invasive8mod<-lm(pertrap~year, data=invasive8)

summary(invasive8mod)

invasivestart8<-coef(invasive8mod)[1] + coef(invasive8mod)[2] * 2016
invasivemean8<-sum(out$ADULTS)/sum(out$TRAPS)

invasive8_perc_change<-100*invasive8mod$coefficients[2]

invasive8_perc_change


######
#4-phase analysis and figure

#until 2000
invasivefirst<-subset(invasivetot, year <= 2000)

invasivefirstmod<-lm(pertrap~year, data=invasivefirst)

summary(invasivefirstmod)

out<-ddply(invasivefirst, .(year), summarise,
           ADULTS = sum(ADULTS),
           TRAPS=sum(TRAPS))

invasivefirstmean<-sum(out$ADULTS)/sum(out$TRAPS)

invasivefirststart<-coef(invasivefirstmod)[1] + coef(invasivefirstmod)[2] * 1993

invasivefirst_perc_change<-100*invasivefirstmod$coefficients[2]
mean_textfirst <- round(invasivefirst_perc_change, 1)

end_firstx <- 2000.5
end_invasivefirsty <- coef(invasivefirstmod)[1] + coef(invasivefirstmod)[2] * end_firstx



# 2001-2005
invasivesecond<-subset(invasivetot, year >= 2000 & year<=2005)

invasivesecondmod<-lm(pertrap~year, data=invasivesecond)

summary(invasivesecondmod)

out<-ddply(invasivesecond, .(year), summarise,
           ADULTS = sum(ADULTS),
           TRAPS=sum(TRAPS))

invasivesecondmean<-sum(out$ADULTS)/sum(out$TRAPS)

invasivesecondstart<-coef(invasivesecondmod)[1] + coef(invasivesecondmod)[2] * 2001

invasivesecond_perc_change<-100*invasivesecondmod$coefficients[2]
mean_textsecond <- round(invasivesecond_perc_change, 1)

end_secondx <- 2005.5
end_invasivesecondy <- coef(invasivesecondmod)[1] + coef(invasivesecondmod)[2] * end_secondx


# 2006-2015
invasivethird<-subset(invasivetot, year >= 2006 & year<=2015)

invasivethirdmod<-lm(pertrap~year, data=invasivethird)

summary(invasivethirdmod)

out<-ddply(invasivethird, .(year), summarise,
           ADULTS = sum(ADULTS),
           TRAPS=sum(TRAPS))

invasivethirdmean<-sum(out$ADULTS)/sum(out$TRAPS)

invasivethirdstart<-coef(invasivethirdmod)[1] + coef(invasivethirdmod)[2] * 2006

invasivethird_perc_change<-100*invasivethirdmod$coefficients[2]
mean_textthird <- round(invasivethird_perc_change, 1)

end_thirdx <- 2015.5
end_invasivethirdy <- coef(invasivethirdmod)[1] + coef(invasivethirdmod)[2] * end_thirdx

# 2016-2023
invasivefourth<-subset(invasivetot, year >= 2016)

invasivefourthmod<-lm(pertrap~year, data=invasivefourth)

summary(invasivefourthmod)

out<-ddply(invasivefourth, .(year), summarise,
           ADULTS = sum(ADULTS),
           TRAPS=sum(TRAPS))

invasivefourthmean<-sum(out$ADULTS)/sum(out$TRAPS)

invasivefourthstart<-coef(invasivefourthmod)[1] + coef(invasivefourthmod)[2] * 2016

invasivefourth_perc_change<-100*invasivefourthmod$coefficients[2]
mean_textfourth <- round(invasivefourth_perc_change, 1)

end_fourthx <- 2023
end_invasivefourthy <- coef(invasivefourthmod)[1] + coef(invasivefourthmod)[2] * end_fourthx


invasive_trendplot<-ggplot(data=invasivetot, aes(year, pertrap))+
  geom_vline(xintercept=c(2000.5, 2005.5, 2015.5), colour="cornsilk4", linetype="longdash")+
  #shaded rectangles at bottom
  #2000 segment
  annotate('rect', xmin=1993, ymin=0, xmax=2000.5, ymax=invasivefirstmean, fill="salmon1", alpha=0.5)+
  #2005 segment
  annotate('rect', xmin=2000.5, ymin=0, xmax=2005.5, ymax=invasivesecondmean, fill="salmon1", alpha=0.5)+
  #2015 segment
  annotate('rect', xmin=2005.5, ymin=0, xmax=2015.5, ymax=invasivethirdmean, fill="salmon1", alpha=0.5)+
  #2023 segment
  annotate('rect', xmin=2015.5, ymin=0, xmax=2023, ymax=invasivefourthmean, fill="salmon1", alpha=0.5)+
  #then overall average line
  #overall segment
  annotate('segment', x=1993, invasivemeanall, xend=2023, yend=invasivemeanall, color="coral3",
           lty="twodash", size=1)+
  
  #then regression lines per segment
  #2000 segment
  annotate('segment', x = 1993, y = coef(invasivefirstmod)[1] + coef(invasivefirstmod)[2] * 1993, 
           xend = end_firstx, yend = end_invasivefirsty, color=NULL, lty="dashed") +
  #2005 segment
  annotate('segment', x = 2000.5, y = coef(invasivesecondmod)[1] + coef(invasivesecondmod)[2] * 2000.5, 
           xend = end_secondx, yend = end_invasivesecondy, color=NULL, lty="dashed") +
  #2015 segment
  annotate('segment', x = 2005.5, y = coef(invasivethirdmod)[1] + coef(invasivethirdmod)[2] * 2005.5, 
           xend = end_thirdx, yend = end_invasivethirdy, color="black", lty="dashed") +
  #2023 segment
  annotate('segment', x = 2015.5, y = coef(invasivefourthmod)[1] + coef(invasivefourthmod)[2] * 2015.5, 
           xend = end_fourthx, yend = end_invasivefourthy, color="black", lty="dashed") +
  
  #then overall regression
  #overall segment
  annotate('segment', x = 1993, y = coef(invasiveallmod)[1] + coef(invasiveallmod)[2] * 1993, 
           xend = 2023, yend = coef(invasiveallmod)[1] + coef(invasiveallmod)[2] * 2023, color="black",lty="solid", ) +
  
  #then percent change annotation labels
  #2000 segment
  annotate('text', x = (1993 + 2000.5) / 2, y = 0.05, 
           label = mean_textfirst,
           size = 4, 
           color = NULL) + 
  #2005 segment
  annotate('text', x = (2000.5+2005.5) / 2, y = 0.05, 
           label = mean_textsecond,
           size = 4, 
           color = NULL) +
  #2015 segment
  annotate('text', x = (2005.5+2015.5) / 2, y = 0.05, 
           label = mean_textthird,
           size = 4, 
           color = "black") + 
  #2023 segment
  annotate('text', x = (2015.5+2023) / 2, y = 0.05 , 
           label = mean_textfourth,
           size = 4, 
           color = "black") + 
  #overall segment
  annotate('text', x = 2012, y = 0.9,
           label = mean_textall,
           size = 7, 
           color = "black") + 
  
  theme_classic()+
  xlim(1993, 2023)+
  xlab(NULL)+ylab("per trap")+
  coord_cartesian(ylim=c(0, 1), clip = "off")
invasive_trendplot





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
  geom_vline(xintercept=c(2000.5, 2005.5, 2015.5), colour="cornsilk4", linetype="longdash")+
  geom_point(data=all_tot, aes(year, ADULTS), position = jitter, pch=21, size=1, fill="lightgrey", col="grey70")+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='grey', alpha=0.6)+
  geom_line()+
  theme_classic()+
  xlim(1993, 2023)+
  xlab(NULL)+ylab("capture trend")+
  scale_y_continuous(limits=c(-0.1, 60))+ 
  annotation_custom(grob = rectGrob(gp = gpar(fill = "grey70", col = "black", alpha = 1)), 
                    xmin = -Inf, xmax = Inf, ymin = 56, ymax = 63) +
  # Text on top of the grey box
  annotate("text", x = 2008, y = 59, label = "All species", color = "black", size = 4, fontface = "plain", family="sans")

all_year


#######
#by plant community or (or community group)
all_tot.gam1<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=TREAT_DESC)+offset(log(TRAPS)),
                  data=all_tot, family="quasipoisson")
summary(all_tot.gam1)

visreg(all_tot.gam1, "year", "TREAT_DESC", ylab="capture trend", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_DESC, ncol = 4)

all_tot.gam2<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=as.factor(TREAT_CAT))+offset(log(TRAPS)),
                  data=all_tot, family="quasipoisson")
summary(all_tot.gam2)

visreg(all_tot.gam2, "year", "TREAT_CAT", ylab="capture trend", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_CAT, ncol = 4)



########Calculations for table #################

#whole timeseries trend

all_allmod<-lm(pertrap~year, data=all_tot)

summary(all_allmod)

all_startall<-coef(all_allmod)[1] + coef(all_allmod)[2] * 1993
all_meanall<-sum(all_tot$ADULTS)/sum(all_tot$TRAPS)

all_all_perc_change<-100*all_allmod$coefficients[2]

all_all_perc_change

mean_textall <- round(all_all_perc_change, 1)


#detection frequency in first five vs last five years


out<-ddply(all_tot, .(year), summarise,
           ADULTS = sum(ADULTS),
           TRAPS=sum(TRAPS))

out

sum(out$ADULTS)/sum(out$TRAPS)

#last 8 year trend
all_8<-subset(all_tot, year >= 2016)

out<-ddply(all_8, .(year), summarise,
           ADULTS = sum(ADULTS),
           TRAPS=sum(TRAPS))

all_8mod<-lm(pertrap~year, data=all_8)

summary(all_8mod)

all_start8<-coef(all_8mod)[1] + coef(all_8mod)[2] * 2016
all_mean8<-sum(out$ADULTS)/sum(out$TRAPS)

all_8_perc_change<-100*all_8mod$coefficients[2]

all_8_perc_change


######
#4-phase analysis and figure

#until 2000
all_first<-subset(all_tot, year <= 2000)

all_firstmod<-lm(pertrap~year, data=all_first)

summary(all_firstmod)

out<-ddply(all_first, .(year), summarise,
           ADULTS = sum(ADULTS),
           TRAPS=sum(TRAPS))

all_firstmean<-sum(out$ADULTS)/sum(out$TRAPS)

all_firststart<-coef(all_firstmod)[1] + coef(all_firstmod)[2] * 1993

all_first_perc_change<-100*all_firstmod$coefficients[2]
mean_textfirst <- round(all_first_perc_change, 1)

end_firstx <- 2000.5
end_all_firsty <- coef(all_firstmod)[1] + coef(all_firstmod)[2] * end_firstx



# 2001-2005
all_second<-subset(all_tot, year >= 2000 & year<=2005)

all_secondmod<-lm(pertrap~year, data=all_second)

summary(all_secondmod)

out<-ddply(all_second, .(year), summarise,
           ADULTS = sum(ADULTS),
           TRAPS=sum(TRAPS))

all_secondmean<-sum(out$ADULTS)/sum(out$TRAPS)

all_secondstart<-coef(all_secondmod)[1] + coef(all_secondmod)[2] * 2001

all_second_perc_change<-100*all_secondmod$coefficients[2]
mean_textsecond <- round(all_second_perc_change, 1)

end_secondx <- 2005.5
end_all_secondy <- coef(all_secondmod)[1] + coef(all_secondmod)[2] * end_secondx


# 2006-2015
all_third<-subset(all_tot, year >= 2006 & year<=2015)

all_thirdmod<-lm(pertrap~year, data=all_third)

summary(all_thirdmod)

out<-ddply(all_third, .(year), summarise,
           ADULTS = sum(ADULTS),
           TRAPS=sum(TRAPS))

all_thirdmean<-sum(out$ADULTS)/sum(out$TRAPS)

all_thirdstart<-coef(all_thirdmod)[1] + coef(all_thirdmod)[2] * 2006

all_third_perc_change<-100*all_thirdmod$coefficients[2]
mean_textthird <- round(all_third_perc_change, 1)

end_thirdx <- 2015.5
end_all_thirdy <- coef(all_thirdmod)[1] + coef(all_thirdmod)[2] * end_thirdx

# 2016-2023
all_fourth<-subset(all_tot, year >= 2016)

all_fourthmod<-lm(pertrap~year, data=all_fourth)

summary(all_fourthmod)

out<-ddply(all_fourth, .(year), summarise,
           ADULTS = sum(ADULTS),
           TRAPS=sum(TRAPS))

all_fourthmean<-sum(out$ADULTS)/sum(out$TRAPS)

all_fourthstart<-coef(all_fourthmod)[1] + coef(all_fourthmod)[2] * 2016

all_fourth_perc_change<-100*all_fourthmod$coefficients[2]
mean_textfourth <- round(all_fourth_perc_change, 1)

end_fourthx <- 2023
end_all_fourthy <- coef(all_fourthmod)[1] + coef(all_fourthmod)[2] * end_fourthx


all__trendplot<-ggplot(data=all_tot, aes(year, pertrap))+
  geom_vline(xintercept=c(2000.5, 2005.5, 2015.5), colour="cornsilk4", linetype="longdash")+
  #shaded rectangles at bottom
  #2000 segment
  annotate('rect', xmin=1993, ymin=0, xmax=2000.5, ymax=all_firstmean, fill="lightgrey", alpha=0.5)+
  #2005 segment
  annotate('rect', xmin=2000.5, ymin=0, xmax=2005.5, ymax=all_secondmean, fill="lightgrey", alpha=0.5)+
  #2015 segment
  annotate('rect', xmin=2005.5, ymin=0, xmax=2015.5, ymax=all_thirdmean, fill="lightgrey", alpha=0.5)+
  #2023 segment
  annotate('rect', xmin=2015.5, ymin=0, xmax=2023, ymax=all_fourthmean, fill="lightgrey", alpha=0.5)+
  #then overall average line
  #overall segment
  annotate('segment', x=1993, all_meanall, xend=2023, yend=all_meanall, color="darkgrey",
           lty="twodash", size=1)+
  
  #then regression lines per segment
  #2000 segment
  annotate('segment', x = 1993, y = coef(all_firstmod)[1] + coef(all_firstmod)[2] * 1993, 
           xend = end_firstx, yend = end_all_firsty, color="black", lty="dashed") +
  #2005 segment
  annotate('segment', x = 2000.5, y = coef(all_secondmod)[1] + coef(all_secondmod)[2] * 2000.5, 
           xend = end_secondx, yend = end_all_secondy, color=NULL, lty="dashed") +
  #2015 segment
  annotate('segment', x = 2005.5, y = coef(all_thirdmod)[1] + coef(all_thirdmod)[2] * 2005.5, 
           xend = end_thirdx, yend = end_all_thirdy, color="black", lty="dashed") +
  #2023 segment
  annotate('segment', x = 2015.5, y = coef(all_fourthmod)[1] + coef(all_fourthmod)[2] * 2015.5, 
           xend = end_fourthx, yend = end_all_fourthy, color=NULL, lty="dashed") +
  
  #then overall regression
  #overall segment
  annotate('segment', x = 1993, y = coef(all_allmod)[1] + coef(all_allmod)[2] * 1993, 
           xend = 2023, yend = coef(all_allmod)[1] + coef(all_allmod)[2] * 2023, color="black",lty="solid", ) +
  
  #then percent change annotation labels
  #2000 segment
  annotate('text', x = (1993 + 2000.5) / 2, y = 0.05, 
           label = mean_textfirst,
           size = 4, 
           color = "black") + 
  #2005 segment
  annotate('text', x = (2000.5+2005.5) / 2, y = 0.05, 
           label = mean_textsecond,
           size = 4, 
           color = NULL) +
  #2015 segment
  annotate('text', x = (2005.5+2015.5) / 2, y = 0.05, 
           label = mean_textthird,
           size = 4, 
           color = "black") + 
  #2023 segment
  annotate('text', x = (2015.5+2023) / 2, y = 0.05, 
           label = mean_textfourth,
           size = 4, 
           color = NULL) + 
  #overall segment
  annotate('text', x = 2012, y = 1,
           label = mean_textall,
           size = 7, 
           color = "black") + 
  
  theme_classic()+
  xlim(1993, 2023)+
  xlab(NULL)+ylab("per trap")+
  coord_cartesian(ylim=c(0, 1), clip = "off")
all__trendplot

##########################################
# make all predation potential figure

#get average number of traps per year per observation
newd <- with(pred_tot,
             data.frame(year = seq(min(year), max(year), length = 1000),
                        TRAPS = 50))
knots<-round(length(unique(pred_tot$year))/6) #only allow max of 1 knot every ~6 years
pred_gam0<-gam(pred~s(year, sp=smooth.param, k=knots)+offset(log(TRAPS)),
              data=pred_tot, family="quasipoisson")
summary(pred_gam0)
pred_pred<-predict.gam(pred_gam0, newd, se.fit = T, type="response")
pred_pred<-cbind(newd,pred_pred)
pred_pred$lower<-pred_pred$fit-2*pred_pred$se.fit
pred_pred$upper<-pred_pred$fit+2*pred_pred$se.fit

jitter<-position_jitter(width = 0.1, height = 0.02)

pred_year<-ggplot(data=pred_pred, aes(year, fit))+
  geom_vline(xintercept=c(2000.5, 2005.5, 2015.5), colour="cornsilk4", linetype="longdash")+
  geom_point(data=pred_tot, aes(year, pred), position = jitter, pch=21, size=1, fill="lightgrey", col="grey70")+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='blue', alpha=0.6)+
  geom_line()+
  theme_classic()+
  xlim(1993, 2023)+
  xlab(NULL)+ylab("capture trend")+
  scale_y_continuous(limits=c(-0.1, 60))+ 
  annotation_custom(grob = rectGrob(gp = gpar(fill = "grey70", col = "black", alpha = 1)), 
                    xmin = -Inf, xmax = Inf, ymin = 56, ymax = 63) +
  # Text on top of the grey box
  annotate("text", x = 2008, y = 59, label = "Predation potential", color = "black", size = 4, fontface = "plain", family="sans")

pred_year


#######
#by plant community or (or community group)
predtot.gam1<-gam(pred~s(year, sp=smooth.param, k=knots, by=TREAT_DESC)+offset(log(TRAPS)),
                  data=pred_tot, family="quasipoisson")
summary(predtot.gam1)

visreg(predtot.gam1, "year", "TREAT_DESC", ylab="capture trend", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_DESC, ncol = 4)

predtot.gam2<-gam(pred~s(year, sp=smooth.param, k=knots, by=as.factor(TREAT_CAT))+offset(log(TRAPS)),
                  data=pred_tot, family="quasipoisson")
summary(predtot.gam2)

visreg(predtot.gam2, "year", "TREAT_CAT", ylab="capture trend", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log')+
  facet_wrap(~TREAT_CAT, ncol = 4)



########Calculations for table #################
#whole timeseries trend

pred_allmod<-lm(pertrap~year, data=pred_tot)

summary(pred_allmod)

pred_startall<-coef(pred_allmod)[1] + coef(pred_allmod)[2] * 1993
pred_meanall<-sum(pred_tot$pred)/sum(pred_tot$TRAPS)

pred_all_perc_change<-100*pred_allmod$coefficients[2]

pred_all_perc_change

mean_textall <- round(pred_all_perc_change, 1)


#detection frequency in first five vs last five years


out<-ddply(pred_tot, .(year), summarise,
           pred= sum(pred),
           TRAPS=sum(TRAPS))

out

sum(out$pred)/sum(out$TRAPS)

#last 8 year trend
pred_8<-subset(pred_tot, year >= 2016)

out<-ddply(pred_8, .(year), summarise,
           pred = sum(pred),
           TRAPS=sum(TRAPS))

pred_8mod<-lm(pertrap~year, data=pred_8)

summary(pred_8mod)

pred_start8<-coef(pred_8mod)[1] + coef(pred_8mod)[2] * 2016
pred_mean8<-sum(out$pred)/sum(out$TRAPS)

pred_8_perc_change<-100*pred_8mod$coefficients[2]

pred_8_perc_change


######
#4-phase analysis and figure

#until 2000
pred_first<-subset(pred_tot, year <= 2000)

pred_firstmod<-lm(pertrap~year, data=pred_first)

summary(pred_firstmod)

out<-ddply(pred_first, .(year), summarise,
           pred = sum(pred),
           TRAPS=sum(TRAPS))

pred_firstmean<-sum(out$pred)/sum(out$TRAPS)

pred_firststart<-coef(pred_firstmod)[1] + coef(pred_firstmod)[2] * 1993

pred_first_perc_change<-100*pred_firstmod$coefficients[2]
mean_textfirst <- round(pred_first_perc_change, 1)

end_firstx <- 2000.5
end_pred_firsty <- coef(pred_firstmod)[1] + coef(pred_firstmod)[2] * end_firstx



# 2001-2005
pred_second<-subset(pred_tot, year >= 2000 & year<=2005)

pred_secondmod<-lm(pertrap~year, data=pred_second)

summary(pred_secondmod)

out<-ddply(pred_second, .(year), summarise,
           pred = sum(pred),
           TRAPS=sum(TRAPS))

pred_secondmean<-sum(out$pred)/sum(out$TRAPS)

pred_secondstart<-coef(pred_secondmod)[1] + coef(pred_secondmod)[2] * 2001

pred_second_perc_change<-100*pred_secondmod$coefficients[2]
mean_textsecond <- round(pred_second_perc_change, 1)

end_secondx <- 2005.5
end_pred_secondy <- coef(pred_secondmod)[1] + coef(pred_secondmod)[2] * end_secondx


# 2006-2015
pred_third<-subset(pred_tot, year >= 2006 & year<=2015)

pred_thirdmod<-lm(pertrap~year, data=pred_third)

summary(pred_thirdmod)

out<-ddply(pred_third, .(year), summarise,
           pred = sum(pred),
           TRAPS=sum(TRAPS))

pred_thirdmean<-sum(out$pred)/sum(out$TRAPS)

pred_thirdstart<-coef(pred_thirdmod)[1] + coef(pred_thirdmod)[2] * 2006

pred_third_perc_change<-100*pred_thirdmod$coefficients[2]
mean_textthird <- round(pred_third_perc_change, 1)

end_thirdx <- 2015.5
end_pred_thirdy <- coef(pred_thirdmod)[1] + coef(pred_thirdmod)[2] * end_thirdx

# 2016-2023
pred_fourth<-subset(pred_tot, year >= 2016)

pred_fourthmod<-lm(pertrap~year, data=pred_fourth)

summary(pred_fourthmod)

out<-ddply(pred_fourth, .(year), summarise,
           pred = sum(pred),
           TRAPS=sum(TRAPS))

pred_fourthmean<-sum(out$pred)/sum(out$TRAPS)

pred_fourthstart<-coef(pred_fourthmod)[1] + coef(pred_fourthmod)[2] * 2016

pred_fourth_perc_change<-100*pred_fourthmod$coefficients[2]
mean_textfourth <- round(pred_fourth_perc_change, 1)

end_fourthx <- 2023
end_pred_fourthy <- coef(pred_fourthmod)[1] + coef(pred_fourthmod)[2] * end_fourthx


pred__trendplot<-ggplot(data=pred_tot, aes(year, pertrap))+
  geom_vline(xintercept=c(2000.5, 2005.5, 2015.5), colour="cornsilk4", linetype="longdash")+
  #shaded rectangles at bottom
  #2000 segment
  annotate('rect', xmin=1993, ymin=0, xmax=2000.5, ymax=pred_firstmean, fill="blue", alpha=0.5)+
  #2005 segment
  annotate('rect', xmin=2000.5, ymin=0, xmax=2005.5, ymax=pred_secondmean, fill="blue", alpha=0.5)+
  #2015 segment
  annotate('rect', xmin=2005.5, ymin=0, xmax=2015.5, ymax=pred_thirdmean, fill="blue", alpha=0.5)+
  #2023 segment
  annotate('rect', xmin=2015.5, ymin=0, xmax=2023, ymax=pred_fourthmean, fill="blue", alpha=0.5)+
  #then overall average line
  #overall segment
  annotate('segment', x=1993, pred_meanall, xend=2023, yend=pred_meanall, color="darkblue",
           lty="twodash", size=1)+
  
  #then regression lines per segment
  #2000 segment
  annotate('segment', x = 1993, y = coef(pred_firstmod)[1] + coef(pred_firstmod)[2] * 1993, 
           xend = end_firstx, yend = end_pred_firsty, color="black", lty="dashed") +
  #2005 segment
  annotate('segment', x = 2000.5, y = coef(pred_secondmod)[1] + coef(pred_secondmod)[2] * 2000.5, 
           xend = end_secondx, yend = end_pred_secondy, color=NULL, lty="dashed") +
  #2015 segment
  annotate('segment', x = 2005.5, y = coef(pred_thirdmod)[1] + coef(pred_thirdmod)[2] * 2005.5, 
           xend = end_thirdx, yend = end_pred_thirdy, color="black", lty="dashed") +
  #2023 segment
  annotate('segment', x = 2015.5, y = coef(pred_fourthmod)[1] + coef(pred_fourthmod)[2] * 2015.5, 
           xend = end_fourthx, yend = end_pred_fourthy, color="black", lty="dashed") +
  
  #then overall regression
  #overall segment
  annotate('segment', x = 1993, y = coef(pred_allmod)[1] + coef(pred_allmod)[2] * 1993, 
           xend = 2023, yend = coef(pred_allmod)[1] + coef(pred_allmod)[2] * 2023, color="black",lty="solid", ) +
  
  #then percent change annotation labels
  #2000 segment
  annotate('text', x = (1993 + 2000.5) / 2, y = 0.05, 
           label = mean_textfirst,
           size = 4, 
           color = "black") + 
  #2005 segment
  annotate('text', x = (2000.5+2005.5) / 2, y = 0.05, 
           label = mean_textsecond,
           size = 4, 
           color = NULL) +
  #2015 segment
  annotate('text', x = (2005.5+2015.5) / 2, y = 0.05, 
           label = mean_textthird,
           size = 4, 
           color = "black") + 
  #2023 segment
  annotate('text', x = (2015.5+2023) / 2, y = 0.05 , 
           label = mean_textfourth,
           size = 4, 
           color = "black") + 
  #overall segment
  annotate('text', x = 2012, y = 0.7,
           label = mean_textall,
           size = 7, 
           color = "black") + 
  
  theme_classic()+
  xlim(1993, 2023)+
  xlab(NULL)+ylab("per trap")+
  coord_cartesian(ylim=c(0, 1), clip = "off")
pred__trendplot



########################################
library(cowplot)
library(gridExtra)
library(grid)

timeseries.stack<-plot_grid(plot_grid( #natives
                            CMAC.year,CYCSP.year,HPARN.year,BURSI.year, CSTIG.year,HGLAC.year,HCONV.year,
                            ABIPN.year, CTRIF.year,  H13.year,
                            #invasives 
                            C7.year,HAXY.year,PQUA.year, HVAR.year,
                            
                            ncol=2, rel_widths=c(1, 1), labels=c('A', 'B', 'C','D', 'E', 'F',
                                                                 'G', 'H', 'I', 'J','K', 'L','M','N'), align="v"),
                            NULL,  ncol = 1, rel_heights = c(1, 0.03))+ 
  draw_label("  year", x = 0.5, y = 0, vjust = -1, fontface = "bold")
timeseries.stack

pdf("plots/fig3_timeseries_stack.pdf", height=12, width=6)
grid.draw(timeseries.stack)
dev.off()


timeseries.grouped<-plot_grid(as_grob(native.year), invasive.year, 
                                 all_year, pred_year, 
                            ncol=1, rel_widths=c(1), labels=c('A', 'B', 'C', 'D'))
timeseries.grouped



pdf("plots/timeseries_grouped.pdf", height=10, width=6)
grid.draw(timeseries.grouped)
dev.off()


trendplot.grouped<-plot_grid(native_trendplot, invasive_trendplot, all__trendplot, pred__trendplot, 
                              ncol=1, rel_widths=c(1), labels=c('A', 'B', 'C', 'D'), align="v")
trendplot.grouped

pdf("plots/trendplot_grouped.pdf", height=10, width=6)
grid.draw(trendplot.grouped)
dev.off()


series_and_trend_grouped<-plot_grid(plot_grid(native.year, native_trendplot, invasive.year, invasive_trendplot,
                                              all_year,all__trendplot, pred_year, pred__trendplot,
                                              ncol=2, rel_widths=c(1,1), labels=c('A', 'E', 'B', 'F', 'C', 'G', 'D', 'H'),
                                              align="v"),
                                    NULL,  ncol = 1, rel_heights = c(1, 0.03))+
  draw_label("  year", x = 0.5, y = 0, vjust = -1, fontface = "bold")

series_and_trend_grouped

pdf("plots/fig1_series_and_trend_grouped.pdf", height=10, width=10)
grid.draw(series_and_trend_grouped)
dev.off()

#####################
#let's check for differential patterns in the timeseries by plant community treatment

#first let's make TREAT_CAT a factor for all the data so we can reorder the plots
nativetot$TREAT_CAT = factor(nativetot$TREAT_CAT, levels=c('Annual', 'Perennial', 'Forest'))
invasivetot$TREAT_CAT = factor(invasivetot$TREAT_CAT, levels=c('Annual', 'Perennial', 'Forest'))
all_tot$TREAT_CAT = factor(all_tot$TREAT_CAT, levels=c('Annual', 'Perennial', 'Forest'))
pred_tot$TREAT_CAT = factor(all_tot$TREAT_CAT, levels=c('Annual', 'Perennial', 'Forest'))

#natives
all_gam_plants<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=TREAT_DESC)+offset(log(TRAPS)),
                    data=nativetot, family="quasipoisson")
summary(all_gam_plants)

visreg(all_gam_plants, "year", "TREAT_DESC", ylab="capture trend", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log', limits=c(-0.1, 10))+
  facet_wrap(~TREAT_DESC, ncol = 4)

all_gam_plants1<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=as.factor(TREAT_CAT))+offset(log(TRAPS)),
                     data=nativetot, family="quasipoisson")
summary(all_gam_plants1)

nativeplot<-visreg(all_gam_plants1, "year", "TREAT_CAT", ylab="capture trend",
                   gg=TRUE, jitter=F, line=list(col="black"), partial=FALSE, rug=FALSE, 
                   fill=list(fill="paleturquoise", col="paleturquoise"),
                   points=list(cex=1, pch=1))+
  scale_y_continuous()+
  facet_wrap(~TREAT_CAT, ncol = 4)+theme_bw()+
  ggtitle("Native species") + 
  theme(plot.title = element_text(hjust = 0, vjust = 0, size = 14, face = "plain", family = "sans"))

nativeplot

nativeplot1<-visreg(all_gam_plants1, "year", "TREAT_CAT", ylab="capture trend",
                    gg=TRUE, overlay=T, jitter=F, partial=FALSE, rug=FALSE, 
                    points=list(cex=1, pch=1))+
  scale_y_continuous()+theme_bw()+labs(color='Community type', fill='Community type')
nativeplot1

#invasives
all_gam_plants<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=TREAT_DESC)+offset(log(TRAPS)),
                    data=invasivetot, family="quasipoisson")
summary(all_gam_plants)

visreg(all_gam_plants, "year", "TREAT_DESC", ylab="capture trend", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log', limits=c(-0.1, 10))+
  facet_wrap(~TREAT_DESC, ncol = 4)

all_gam_plants1<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=as.factor(TREAT_CAT))+offset(log(TRAPS)),
                     data=invasivetot, family="quasipoisson")
summary(all_gam_plants1)

invasiveplot<-visreg(all_gam_plants1, "year", "TREAT_CAT", ylab="capture trend",
                     gg=TRUE, jitter=F, line=list(col="black"), partial=FALSE, rug=FALSE, 
                     fill=list(fill="salmon1", col="salmon1"),
                     points=NULL)+
  scale_y_continuous()+
  facet_wrap(~TREAT_CAT, ncol = 4)+theme_bw()+
  ggtitle("Exotic species") + 
  theme(plot.title = element_text(hjust = 0, vjust = 0, size = 14, face = "plain", family = "sans"))
invasiveplot

invasiveplot1<-visreg(all_gam_plants1, "year", "TREAT_CAT", ylab="capture trend",
                    gg=TRUE, overlay=T, jitter=F, partial=FALSE, rug=FALSE, 
                    points=list(cex=1, pch=1))+
  scale_y_continuous()+theme_bw()+labs(color='Community type', fill='Community type')
invasiveplot1

#all species

all_gam_plants<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=TREAT_DESC)+offset(log(TRAPS)),
              data=all_tot, family="quasipoisson")
summary(all_gam_plants)

visreg(all_gam_plants, "year", "TREAT_DESC", ylab="capture trend", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log', limits=c(-0.1, 10))+
  facet_wrap(~TREAT_DESC, ncol = 4)

all_gam_plants1<-gam(ADULTS~s(year, sp=smooth.param, k=knots, by=as.factor(TREAT_CAT))+offset(log(TRAPS)),
                    data=all_tot, family="quasipoisson")
summary(all_gam_plants1)

visreg(all_gam_plants1, "year", "TREAT_CAT", ylab="capture trend", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log', limits=c(-0.1, 10))+
  facet_wrap(~TREAT_CAT, ncol = 4)

allplot<-visreg(all_gam_plants1, "year", "TREAT_CAT", ylab="capture trend",
                 gg=TRUE, jitter=F, line=list(col="black"), partial=FALSE, rug=FALSE, 
                 fill=list(fill="lightgrey", col="lightgrey"),
                 points=NULL)+
  scale_y_continuous()+
  facet_wrap(~TREAT_CAT, ncol = 4)+theme_bw()+
  ggtitle("All species") + 
  theme(plot.title = element_text(hjust = 0, vjust = 0, size = 14, face = "plain", family = "sans"))
allplot


allplot1<-visreg(all_gam_plants1, "year", "TREAT_CAT", ylab="relative index of abundance",
                    gg=TRUE, overlay=T, jitter=F, partial=FALSE, rug=FALSE, 
                    points=list(cex=1, pch=1))+
  scale_y_continuous()+theme_bw()+labs(color='Community type', fill='Community type')+
  theme(legend.position = c(0.2,0.3))
allplot1

#predation potential

pred_gam_plants<-gam(pred~s(year, sp=smooth.param, k=knots, by=TREAT_DESC)+offset(log(TRAPS)),
                    data=pred_tot, family="quasipoisson")
summary(pred_gam_plants)

visreg(pred_gam_plants, "year", "TREAT_DESC", ylab="capture trend", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log', limits=c(-0.1, 10))+
  facet_wrap(~TREAT_DESC, ncol = 4)

pred_gam_plants1<-gam(pred~s(year, sp=smooth.param, k=knots, by=as.factor(TREAT_CAT))+offset(log(TRAPS)),
                     data=pred_tot, family="quasipoisson")
summary(pred_gam_plants1)

visreg(pred_gam_plants1, "year", "TREAT_CAT", ylab="capture trend", gg=TRUE)+
  scale_y_continuous(trans='pseudo_log', limits=c(-0.1, 10))+
  facet_wrap(~TREAT_CAT, ncol = 4)

predplot<-visreg(pred_gam_plants1, "year", "TREAT_CAT", ylab="capture trend",
                     gg=TRUE, jitter=F, line=list(col="black"), partial=FALSE, rug=FALSE, 
                     fill=list(fill="blue", col="blue"),
                     points=NULL)+
  scale_y_continuous()+
  facet_wrap(~TREAT_CAT, ncol = 4)+theme_bw()+
  ggtitle("Predation potential") + 
  theme(plot.title = element_text(hjust = 0, vjust = 0, size = 14, face = "plain", family = "sans"))
predplot


#create the by plant community figures

by.community<-plot_grid(nativeplot, invasiveplot, allplot, predplot,
                              ncol=1, rel_widths=c(1), labels=c('A', 'B', 'C', 'D'), align="v")
by.community

pdf("plots/fig2_timeseries_by_community.pdf", height=8, width=8)
grid.draw(by.community)
dev.off()


predplot1<-visreg(pred_gam_plants1, "year", by="TREAT_CAT", ylab="predation potential",
                 gg=TRUE, overlay=T, jitter=F, partial=FALSE, rug=F,
                 points=NULL)+
  scale_y_continuous()+theme_bw()+labs(color='Community type', fill='Community type')#+
  #scale_color_manual(values=c("black", "blue", "green"))+
  #scale_fill_manual(values=alpha(c("black", "blue", "green"),0.2))
predplot1

pdf("plots/native_by_community.pdf", height=4, width=6)
grid.draw(nativeplot1)
dev.off()

pdf("plots/invasive_by_community.pdf", height=4, width=6)
grid.draw(invasiveplot1)
dev.off()

pdf("plots/all_by_community.pdf", height=4, width=4.5)
grid.draw(allplot1)
dev.off()

pdf("plots/pred_by_community.pdf", height=4, width=6)
grid.draw(predplot1)
dev.off()

setEPS()
postscript("plots/all_by_community.eps", height=4, width=4.5)
grid.draw(allplot1)
dev.off()




# First make a dataframe for error bars
alltot_agg <- ddply(all_tot, .(TREAT_CAT, year), summarise,
                   mean.bugs = mean(ADULTS/TRAPS),
                   sd.bugs.pt = sd(ADULTS/TRAPS),
                   N = length(ADULTS/TRAPS),
                   SE = sd.bugs.pt / sqrt(N))