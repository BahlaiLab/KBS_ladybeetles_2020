#graphical parameters
smooth.param<-0.5
knots<-6


##########################################
# make ABIPN figure

#get average number of traps per year per observation
newd <- with(ABIPN_summary,
             data.frame(year = seq(min(year), max(year), length = 1000),
                       TRAPS = 50))

ABIPN.gam0<-gam(ADULTS~s(year, sp=smooth.param, k=6)+offset(log(TRAPS)),
               data=ABIPN_summary)
summary(ABIPN.gam0)
ABIPN.pred<-predict.gam(ABIPN.gam0, newd, se.fit = T, type="link")
ABIPN.pred<-cbind(newd,ABIPN.pred)
ABIPN.pred$lower<-ABIPN.pred$fit-2*ABIPN.pred$se.fit
ABIPN.pred$upper<-ABIPN.pred$fit+2*ABIPN.pred$se.fit

jitter<-position_jitter(width = 0.2, height = 0.02)

ABIPN.year<-ggplot(data=ABIPN.pred, aes(year, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), color='paleturquoise', fill='paleturquoise')+
  geom_line()+
  geom_point(data=ABIPN_summary, aes(year, ADULTS), position = jitter, pch=21, size=2, fill="lightgrey")+
  theme_classic()+
  xlim(1989, 2020)+
  xlab(NULL)+ylab(NULL)+
  scale_y_continuous(trans='pseudo_log')
ABIPN.year


##########################################
# make BURSI figure

#get average number of traps per year per observation
newd <- with(BURSI_summary,
             data.frame(year = seq(min(year), max(year), length = 1000),
                        TRAPS = 50))

BURSI.gam0<-gam(ADULTS~s(year, sp=smooth.param, k=6)+offset(log(TRAPS)),
                data=BURSI_summary)
summary(BURSI.gam0)
BURSI.pred<-predict.gam(BURSI.gam0, newd, se.fit = T, type="link")
BURSI.pred<-cbind(newd,BURSI.pred)
BURSI.pred$lower<-BURSI.pred$fit-2*BURSI.pred$se.fit
BURSI.pred$upper<-BURSI.pred$fit+2*BURSI.pred$se.fit

jitter<-position_jitter(width = 0.2, height = 0.02)

BURSI.year<-ggplot(data=BURSI.pred, aes(year, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), color='paleturquoise', fill='paleturquoise')+
  geom_line()+
  geom_point(data=BURSI_summary, aes(year, ADULTS), position = jitter, pch=21, size=2, fill="lightgrey")+
  theme_classic()+
  xlim(1989, 2020)+
  xlab(NULL)+ylab(NULL)+
  scale_y_continuous(trans='pseudo_log')
BURSI.year




##########################################
# make C7 figure

#get average number of traps per year per observation
newd <- with(C7_summary,
             data.frame(year = seq(min(year), max(year), length = 1000),
                        TRAPS = 50))

C7.gam0<-gam(ADULTS~s(year, sp=smooth.param, k=knots)+offset(log(TRAPS)),
                data=C7_summary)
summary(C7.gam0)
C7.pred<-predict.gam(C7.gam0, newd, se.fit = T, type="link")
C7.pred<-cbind(newd,C7.pred)
C7.pred$lower<-C7.pred$fit-2*C7.pred$se.fit
C7.pred$upper<-C7.pred$fit+2*C7.pred$se.fit

jitter<-position_jitter(width = 0.2, height = 0.02)

C7.year<-ggplot(data=C7.pred, aes(year, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), color='salmon1', fill='salmon1')+
  geom_line()+
  geom_point(data=C7_summary, aes(year, ADULTS), position = jitter, pch=21, size=2, fill="lightgrey")+
  theme_classic()+
  xlim(1989, 2020)+
  xlab(NULL)+ylab(NULL)+
  scale_y_continuous(trans='pseudo_log')
C7.year



##########################################
# make CMAC figure

#get average number of traps per year per observation
newd <- with(CMAC_summary,
             data.frame(year = seq(min(year), max(year), length = 1000),
                        TRAPS = 50))

CMAC.gam0<-gam(ADULTS~s(year, sp=smooth.param, k=knots)+offset(log(TRAPS)),
                data=CMAC_summary)
summary(CMAC.gam0)
CMAC.pred<-predict.gam(CMAC.gam0, newd, se.fit = T, type="link")
CMAC.pred<-cbind(newd,CMAC.pred)
CMAC.pred$lower<-CMAC.pred$fit-2*CMAC.pred$se.fit
CMAC.pred$upper<-CMAC.pred$fit+2*CMAC.pred$se.fit

jitter<-position_jitter(width = 0.2, height = 0.02)

CMAC.year<-ggplot(data=CMAC.pred, aes(year, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), color='paleturquoise', fill='paleturquoise')+
  geom_line()+
  geom_point(data=CMAC_summary, aes(year, ADULTS), position = jitter, pch=21, size=2, fill="lightgrey")+
  theme_classic()+
  xlim(1989, 2020)+
  xlab(NULL)+ylab(NULL)+
  scale_y_continuous(trans='pseudo_log')
CMAC.year

##########################################
# make CSTIG figure

#get average number of traps per year per observation
newd <- with(CSTIG_summary,
             data.frame(year = seq(min(year), max(year), length = 1000),
                        TRAPS = 50))

CSTIG.gam0<-gam(ADULTS~s(year, sp=smooth.param, k=knots)+offset(log(TRAPS)),
                data=CSTIG_summary)
summary(CSTIG.gam0)
CSTIG.pred<-predict.gam(CSTIG.gam0, newd, se.fit = T, type="link")
CSTIG.pred<-cbind(newd,CSTIG.pred)
CSTIG.pred$lower<-CSTIG.pred$fit-2*CSTIG.pred$se.fit
CSTIG.pred$upper<-CSTIG.pred$fit+2*CSTIG.pred$se.fit

jitter<-position_jitter(width = 0.2, height = 0.02)

CSTIG.year<-ggplot(data=CSTIG.pred, aes(year, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), color='paleturquoise', fill='paleturquoise')+
  geom_line()+
  geom_point(data=CSTIG_summary, aes(year, ADULTS), position = jitter, pch=21, size=2, fill="lightgrey")+
  theme_classic()+
  xlim(1989, 2020)+
  xlab(NULL)+ylab(NULL)+
  scale_y_continuous(trans='pseudo_log')
CSTIG.year

##########################################
# make CTRIF figure

#get average number of traps per year per observation
newd <- with(CTRIF_summary,
             data.frame(year = seq(min(year), max(year), length = 1000),
                        TRAPS = 50))

CTRIF.gam0<-gam(ADULTS~s(year, sp=smooth.param, k=knots)+offset(log(TRAPS)),
                data=CTRIF_summary)
summary(CTRIF.gam0)
CTRIF.pred<-predict.gam(CTRIF.gam0, newd, se.fit = T, type="link")
CTRIF.pred<-cbind(newd,CTRIF.pred)
CTRIF.pred$lower<-CTRIF.pred$fit-2*CTRIF.pred$se.fit
CTRIF.pred$upper<-CTRIF.pred$fit+2*CTRIF.pred$se.fit

jitter<-position_jitter(width = 0.2, height = 0.02)

CTRIF.year<-ggplot(data=CTRIF.pred, aes(year, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), color='paleturquoise', fill='paleturquoise')+
  geom_line()+
  geom_point(data=CTRIF_summary, aes(year, ADULTS), position = jitter, pch=21, size=2, fill="lightgrey")+
  theme_classic()+
  xlim(1989, 2020)+
  xlab(NULL)+ylab(NULL)+
  scale_y_continuous(trans='pseudo_log')
CTRIF.year



##########################################
# make CYCSP figure

#get average number of traps per year per observation
newd <- with(CYCSP_summary,
             data.frame(year = seq(min(year), max(year), length = 1000),
                        TRAPS = 50))

CYCSP.gam0<-gam(ADULTS~s(year, sp=smooth.param, k=knots)+offset(log(TRAPS)),
                data=CYCSP_summary)
summary(CYCSP.gam0)
CYCSP.pred<-predict.gam(CYCSP.gam0, newd, se.fit = T, type="link")
CYCSP.pred<-cbind(newd,CYCSP.pred)
CYCSP.pred$lower<-CYCSP.pred$fit-2*CYCSP.pred$se.fit
CYCSP.pred$upper<-CYCSP.pred$fit+2*CYCSP.pred$se.fit

jitter<-position_jitter(width = 0.2, height = 0.02)

CYCSP.year<-ggplot(data=CYCSP.pred, aes(year, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), color='paleturquoise', fill='paleturquoise')+
  geom_line()+
  geom_point(data=CYCSP_summary, aes(year, ADULTS), position = jitter, pch=21, size=2, fill="lightgrey")+
  theme_classic()+
  xlim(1989, 2020)+
  xlab(NULL)+ylab(NULL)+
  scale_y_continuous(trans='pseudo_log')
CYCSP.year

##########################################
# make H13 figure

#get average number of traps per year per observation
newd <- with(H13_summary,
             data.frame(year = seq(min(year), max(year), length = 1000),
                        TRAPS = 50))

H13.gam0<-gam(ADULTS~s(year, sp=smooth.param, k=knots)+offset(log(TRAPS)),
                data=H13_summary)
summary(H13.gam0)
H13.pred<-predict.gam(H13.gam0, newd, se.fit = T, type="link")
H13.pred<-cbind(newd,H13.pred)
H13.pred$lower<-H13.pred$fit-2*H13.pred$se.fit
H13.pred$upper<-H13.pred$fit+2*H13.pred$se.fit

jitter<-position_jitter(width = 0.2, height = 0.02)

H13.year<-ggplot(data=H13.pred, aes(year, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), color='paleturquoise', fill='paleturquoise')+
  geom_line()+
  geom_point(data=H13_summary, aes(year, ADULTS), position = jitter, pch=21, size=2, fill="lightgrey")+
  theme_classic()+
  xlim(1989, 2020)+
  xlab(NULL)+ylab(NULL)+
  scale_y_continuous(trans='pseudo_log')
H13.year


##########################################
# make HAXY figure

#get average number of traps per year per observation
newd <- with(HAXY_summary,
             data.frame(year = seq(min(year), max(year), length = 1000),
                        TRAPS = 50))

HAXY.gam0<-gam(ADULTS~s(year, sp=smooth.param, k=knots)+offset(log(TRAPS)),
                data=HAXY_summary)
summary(HAXY.gam0)
HAXY.pred<-predict.gam(HAXY.gam0, newd, se.fit = T, type="link")
HAXY.pred<-cbind(newd,HAXY.pred)
HAXY.pred$lower<-HAXY.pred$fit-2*HAXY.pred$se.fit
HAXY.pred$upper<-HAXY.pred$fit+2*HAXY.pred$se.fit

jitter<-position_jitter(width = 0.2, height = 0.02)

HAXY.year<-ggplot(data=HAXY.pred, aes(year, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), color='salmon1', fill='salmon1')+
  geom_line()+
  geom_point(data=HAXY_summary, aes(year, ADULTS), position = jitter, pch=21, size=2, fill="lightgrey")+
  theme_classic()+
  xlim(1989, 2020)+
  xlab(NULL)+ylab(NULL)+
  scale_y_continuous(trans='pseudo_log')
HAXY.year


##########################################
# make HCONV figure

#get average number of traps per year per observation
newd <- with(HCONV_summary,
             data.frame(year = seq(min(year), max(year), length = 1000),
                        TRAPS = 50))

HCONV.gam0<-gam(ADULTS~s(year, sp=smooth.param, k=knots)+offset(log(TRAPS)),
                data=HCONV_summary)
summary(HCONV.gam0)
HCONV.pred<-predict.gam(HCONV.gam0, newd, se.fit = T, type="link")
HCONV.pred<-cbind(newd,HCONV.pred)
HCONV.pred$lower<-HCONV.pred$fit-2*HCONV.pred$se.fit
HCONV.pred$upper<-HCONV.pred$fit+2*HCONV.pred$se.fit

jitter<-position_jitter(width = 0.2, height = 0.02)

HCONV.year<-ggplot(data=HCONV.pred, aes(year, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), color='paleturquoise', fill='paleturquoise')+
  geom_line()+
  geom_point(data=HCONV_summary, aes(year, ADULTS), position = jitter, pch=21, size=2, fill="lightgrey")+
  theme_classic()+
  xlim(1989, 2020)+
  xlab(NULL)+ylab(NULL)+
  scale_y_continuous(trans='pseudo_log')
HCONV.year


##########################################
# make HGLAC figure

#get average number of traps per year per observation
newd <- with(HGLAC_summary,
             data.frame(year = seq(min(year), max(year), length = 1000),
                        TRAPS = 50))

HGLAC.gam0<-gam(ADULTS~s(year, sp=smooth.param, k=knots)+offset(log(TRAPS)),
                data=HGLAC_summary)
summary(HGLAC.gam0)
HGLAC.pred<-predict.gam(HGLAC.gam0, newd, se.fit = T, type="link")
HGLAC.pred<-cbind(newd,HGLAC.pred)
HGLAC.pred$lower<-HGLAC.pred$fit-2*HGLAC.pred$se.fit
HGLAC.pred$upper<-HGLAC.pred$fit+2*HGLAC.pred$se.fit

jitter<-position_jitter(width = 0.2, height = 0.02)

HGLAC.year<-ggplot(data=HGLAC.pred, aes(year, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), color='paleturquoise', fill='paleturquoise')+
  geom_line()+
  geom_point(data=HGLAC_summary, aes(year, ADULTS), position = jitter, pch=21, size=2, fill="lightgrey")+
  theme_classic()+
  xlim(1989, 2020)+
  xlab(NULL)+ylab(NULL)+
  scale_y_continuous(trans='pseudo_log')
HGLAC.year
##########################################
# make HPARN figure

#get average number of traps per year per observation
newd <- with(HPARN_summary,
             data.frame(year = seq(min(year), max(year), length = 1000),
                        TRAPS = 50))

HPARN.gam0<-gam(ADULTS~s(year, sp=smooth.param, k=knots)+offset(log(TRAPS)),
                data=HPARN_summary)
summary(HPARN.gam0)
HPARN.pred<-predict.gam(HPARN.gam0, newd, se.fit = T, type="link")
HPARN.pred<-cbind(newd,HPARN.pred)
HPARN.pred$lower<-HPARN.pred$fit-2*HPARN.pred$se.fit
HPARN.pred$upper<-HPARN.pred$fit+2*HPARN.pred$se.fit

jitter<-position_jitter(width = 0.2, height = 0.02)

HPARN.year<-ggplot(data=HPARN.pred, aes(year, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), color='paleturquoise', fill='paleturquoise')+
  geom_line()+
  geom_point(data=HPARN_summary, aes(year, ADULTS), position = jitter, pch=21, size=2, fill="lightgrey")+
  theme_classic()+
  xlim(1989, 2020)+
  xlab(NULL)+ylab(NULL)+
  scale_y_continuous(trans='pseudo_log')
HPARN.year
##########################################
# make HVAR figure

#get average number of traps per year per observation
newd <- with(HVAR_summary,
             data.frame(year = seq(min(year), max(year), length = 1000),
                        TRAPS = 50))

HVAR.gam0<-gam(ADULTS~s(year, sp=smooth.param, k=knots)+offset(log(TRAPS)),
                data=HVAR_summary)
summary(HVAR.gam0)
HVAR.pred<-predict.gam(HVAR.gam0, newd, se.fit = T, type="link")
HVAR.pred<-cbind(newd,HVAR.pred)
HVAR.pred$lower<-HVAR.pred$fit-2*HVAR.pred$se.fit
HVAR.pred$upper<-HVAR.pred$fit+2*HVAR.pred$se.fit

jitter<-position_jitter(width = 0.2, height = 0.02)

HVAR.year<-ggplot(data=HVAR.pred, aes(year, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), color='salmon1', fill='salmon1')+
  geom_line()+
  geom_point(data=HVAR_summary, aes(year, ADULTS), position = jitter, pch=21, size=2, fill="lightgrey")+
  theme_classic()+
  xlim(1989, 2020)+
  xlab(NULL)+ylab(NULL)+
  scale_y_continuous(trans='pseudo_log')
HVAR.year

##########################################
# make PQUA figure

#get average number of traps per year per observation
newd <- with(PQUA_summary,
             data.frame(year = seq(min(year), max(year), length = 1000),
                        TRAPS = 50))

PQUA.gam0<-gam(ADULTS~s(year, sp=smooth.param, k=knots)+offset(log(TRAPS)),
                data=PQUA_summary)
summary(PQUA.gam0)
PQUA.pred<-predict.gam(PQUA.gam0, newd, se.fit = T, type="link")
PQUA.pred<-cbind(newd,PQUA.pred)
PQUA.pred$lower<-PQUA.pred$fit-2*PQUA.pred$se.fit
PQUA.pred$upper<-PQUA.pred$fit+2*PQUA.pred$se.fit

jitter<-position_jitter(width = 0.2, height = 0.02)

PQUA.year<-ggplot(data=PQUA.pred, aes(year, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), color='salmon1', fill='salmon1')+
  geom_line()+
  geom_point(data=PQUA_summary, aes(year, ADULTS), position = jitter, pch=21, size=2, fill="lightgrey")+
  theme_classic()+
  xlim(1989, 2020)+
  xlab(NULL)+ylab(NULL)+
  scale_y_continuous(trans='pseudo_log')
PQUA.year



##########################################
# make all native figure

#get average number of traps per year per observation
newd <- with(nativetot,
             data.frame(year = seq(min(year), max(year), length = 1000),
                        TRAPS = 50))

native.gam0<-gam(ADULTS~s(year, sp=smooth.param, k=knots)+offset(log(TRAPS)),
                data=nativetot)
summary(native.gam0)
native.pred<-predict.gam(native.gam0, newd, se.fit = T, type="link")
native.pred<-cbind(newd,native.pred)
native.pred$lower<-native.pred$fit-2*native.pred$se.fit
native.pred$upper<-native.pred$fit+2*native.pred$se.fit

jitter<-position_jitter(width = 0.2, height = 0.02)

native.year<-ggplot(data=native.pred, aes(year, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), color='paleturquoise', fill='paleturquoise')+
  geom_line()+
  geom_point(data=native, aes(year, ADULTS), position = jitter, pch=21, size=2, fill="lightgrey")+
  theme_classic()+
  xlim(1989, 2020)+
  xlab(NULL)+ylab(NULL)+
  scale_y_continuous(trans='pseudo_log')
native.year



##########################################
# make all invasive figure

#get average number of traps per year per observation
newd <- with(invasivetot,
             data.frame(year = seq(min(year), max(year), length = 1000),
                        TRAPS = 50))

invasive.gam0<-gam(ADULTS~s(year, sp=smooth.param, k=knots)+offset(log(TRAPS)),
                 data=invasivetot)
summary(invasive.gam0)
invasive.pred<-predict.gam(invasive.gam0, newd, se.fit = T, type="link")
invasive.pred<-cbind(newd,invasive.pred)
invasive.pred$lower<-invasive.pred$fit-2*invasive.pred$se.fit
invasive.pred$upper<-invasive.pred$fit+2*invasive.pred$se.fit

jitter<-position_jitter(width = 0.2, height = 0.02)

invasive.year<-ggplot(data=invasive.pred, aes(year, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), color='salmon1', fill='salmon1')+
  geom_line()+
  geom_point(data=invasive, aes(year, ADULTS), position = jitter, pch=21, size=2, fill="lightgrey")+
  theme_classic()+
  xlim(1989, 2020)+
  xlab(NULL)+ylab(NULL)+
  scale_y_continuous(trans='pseudo_log')
invasive.year


##########################################
# make all all_ figure

#get average number of traps per year per observation
newd <- with(all_tot,
             data.frame(year = seq(min(year), max(year), length = 1000),
                        TRAPS = 50))

all_gam0<-gam(ADULTS~s(year, sp=smooth.param, k=knots)+offset(log(TRAPS)),
                 data=all_tot)
summary(all_gam0)
all_pred<-predict.gam(all_gam0, newd, se.fit = T, type="link")
all_pred<-cbind(newd,all_pred)
all_pred$lower<-all_pred$fit-2*all_pred$se.fit
all_pred$upper<-all_pred$fit+2*all_pred$se.fit

jitter<-position_jitter(width = 0.2, height = 0.02)

all_year<-ggplot(data=all_pred, aes(year, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), color='grey', fill='grey')+
  geom_line()+
  geom_point(data=all_tot, aes(year, ADULTS), position = jitter, pch=21, size=2, fill="lightgrey")+
  theme_classic()+
  xlim(1989, 2020)+
  xlab(NULL)+ylab(NULL)+
  scale_y_continuous(trans='pseudo_log')
all_year
