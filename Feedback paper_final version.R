rm(list=ls())
source('D:/PhD Project/Feedback paper/Data and codes/Feedback paper_functions_final version.R', encoding = 'UTF-8')

############################################################################################################################
###################### Import source file and convert to the same timescale: AICC2012 BP 1950 #############################
############################################################################################################################

#Import ice core data
CO2_source <- read.csv("D:/PhD Project/Feedback paper/Data and codes/Original data/CO2_WAIS.csv")
CH4_source<-read.csv("D:/PhD Project/Feedback paper/Data and codes/Original data/CH4_NEEM.csv")
N2O_source<-read.csv("D:/PhD Project/Feedback paper/Data and codes/Original data/N2O_NGRIP.csv")
DO_source <- read.csv("D:/PhD Project/Feedback paper/Data and codes/Original data/DO_timing.csv")
T_Greenland_source <- read.csv("D:/PhD Project/Feedback paper/Data and codes/Original data/T_Greenland_NGRIP.csv")
dD_EDC_source <- read.csv("D:/PhD Project/Feedback paper/Data and codes/Original data/dD_EDC.csv")

#Import the file to convert GICC05 to AICC2012
#AICC2012 is tuned to GICC05 over the last 60 ka
transfer <- read.csv("D:/PhD Project/Feedback paper/Data and codes/Original data/GICC05toAICC2012.csv")
transfer_EDC <- read.csv("D:/PhD Project/Feedback paper/Data and codes/Original data/depth_EDCtoAICC2012.csv")

#remove NAs
CO2_AICC<-na.omit(CO2_source)
N2O_AICC<-na.omit(N2O_source[,-which(colnames(N2O_source)=="N2O_arte")])
CH4_AICC<-na.omit(CH4_source)
T_Greenland_AICC<-na.omit(T_Greenland_source)
dD_EDC_AICC<-na.omit(dD_EDC_source)


#convert timescale
CO2_AICC$AICC2012_BP1950<-convert(CO2_AICC$age_calBP/1.0063,transfer)
CH4_AICC$AICC2012_BP1950<-convert(CH4_AICC$GICC05_BP2000-50,transfer)
DO_AICC<-convert(as.matrix(DO_source[-1,"Age.ka.b2k"]*1000)-50,transfer)
T_Greenland_AICC$AICC2012_BP1950<-convert(T_Greenland_AICC$GICC05_BP2000-50,transfer)
dD_EDC_AICC$AICC2012_BP1950<-convert_depth_age(dD_EDC_AICC$depth,transfer_EDC)

#To simply, here we just keep age (AICC2012 BP 1950), concentrations and their errors (CO2 in ppm, CH4 in ppb, N2O in ppb)
CO2<-CO2_AICC[,c("AICC2012_BP1950", "CO2_blank_gravity_corrected","CO2_se_1sigma")];colnames(CO2)<-c("Age","Conc","Err")
CH4<-CH4_AICC[,c("AICC2012_BP1950","CH4_ppbcalib","CH4_ppberr")];colnames(CH4)<-c("Age","Conc","Err")
N2O<-N2O_AICC[,c("AICC2012_BP1950","N2O_ppb","N2O_err")];colnames(N2O)<-c("Age","Conc","Err")
T_Greenland<-T_Greenland_AICC[,c("AICC2012_BP1950","Temperature")];colnames(T_Greenland)<-c("Age","TGreenland")
T_Greenland$Err<-0
dD<-dD_EDC_AICC[,c("AICC2012_BP1950","EDCbag_excess")];colnames(dD)<-c("Age","Excess")
DO<-DO_AICC

#Use only 50-30 kyr
CO2<-CO2[which(CO2$Age>30000&CO2$Age<50000),]
CH4<-CH4[which(CH4$Age>30000&CH4$Age<50000),]
N2O<-N2O[which(N2O$Age>30000&N2O$Age<50000),]
T_Greenland<-T_Greenland[which(T_Greenland$Age>30000&T_Greenland$Age<50000),]
dD<-dD[which(dD$Age>30000&dD$Age<50000),]

#get average resolution
round(mean(CO2[-1,"Age"]-CO2[-nrow(CO2),"Age"]),digits = 0)
round(mean(CH4[-1,"Age"]-CH4[-nrow(CH4),"Age"]),digits = 0)
round(mean(N2O[-1,"Age"]-N2O[-nrow(N2O),"Age"]),digits = 0)
round(mean(T_Greenland[-1,"Age"]-T_Greenland[-nrow(T_Greenland),"Age"]),digits = 0)
round(mean(dD[-1,"Age"]-dD[-nrow(dD),"Age"]),digits = 0)

summary(CO2[-1,"Age"]-CO2[-nrow(CO2),"Age"])
summary(CH4[-1,"Age"]-CH4[-nrow(CH4),"Age"])
summary(N2O[-1,"Age"]-N2O[-nrow(N2O),"Age"])
summary(T_Greenland[-1,"Age"]-T_Greenland[-nrow(T_Greenland),"Age"])
summary(dD[-1,"Age"]-dD[-nrow(dD),"Age"])

#Plot time series
DO_lab<-seq(1:20)
DOdata<-cbind.data.frame(DO,DO_lab)
xbreak <- 10000*c(3,3.5,4,4.5,5)
DOp<-DO[5:12]
DOpdata<-DOdata[5:12,]

if(!require(ggplot2)){ install.packages("ggplot2");library(ggplot2)}
if(!require(egg)){ install.packages("egg");library(egg)}

setwd("D:/PhD Project/Feedback paper/Data and codes/Output data/Plots")
p1<-ggplot(data = CO2, aes(x = Age, y = Conc))+geom_point(size=0.5)+geom_line(size=0.4)+ 
  geom_vline(xintercept=DOp)+  
  scale_x_continuous(breaks=xbreak,limits=c(30000,50000))+
  labs(y= bquote(CO[2]~'(ppm)'),x=NULL)+
  geom_text(data=DOpdata,aes(x=DO, label=DO_lab, y=Inf),size=4.5, hjust=0.4, vjust=-0.6)+  
  coord_cartesian(clip = "off")+
  theme(plot.margin = margin(20, 2, 2, 2, unit = "pt"))+ 
    theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
          panel.background = element_rect(fill = "white", colour = "black"))+
  annotate("text", y= max(CO2$Conc), x =30000,label="(a)",fontface = 4,size=6)+
  theme(axis.text.y=element_text(size=13),axis.title.y=element_text(size=13))

p2<-ggplot(data = CH4, aes(x = Age, y = Conc))+geom_point(size=0.5)+geom_line(size=0.4)+
  geom_vline(xintercept=DOp)+
  scale_x_continuous(breaks=xbreak,limits=c(30000,50000))+
  labs(y= bquote(CH[4]~'(ppb)'),x=NULL)+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"))+
  coord_cartesian(clip = "off")+
  theme(plot.margin = margin(2, 2, 2, 2, unit = "pt"))+
  annotate("text", y= max(CH4$Conc), x =30000,label="(b)",fontface = 4,size=6)+
  theme(axis.text.y=element_text(size=13),axis.title.y=element_text(size=13))

p3<-ggplot(data = N2O, aes(x = Age, y = Conc))+geom_point(size=0.5)+geom_line(size=0.4)+
  geom_vline(xintercept=DOp)+
  scale_x_continuous(breaks=xbreak,limits=c(30000,50000))+
  labs(y= bquote(N[2]*'O  (ppb)'),x=NULL)+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"))+
  coord_cartesian(clip = "off")+
  theme(plot.margin = margin(2, 2, 2, 2, unit = "pt"))+
  annotate("text", y= max(N2O$Conc), x =30000,label="(c)",fontface = 4,size=6)+
  theme(axis.text.y=element_text(size=13),axis.title.y=element_text(size=13))

p4<-ggplot(data = T_Greenland, aes(x = Age, y = TGreenland))+geom_point(size=0.5)+geom_line(size=0.4)+
  geom_vline(xintercept=DOp)+
  scale_x_continuous(breaks=xbreak,limits=c(30000,50000))+
  labs(y= expression(paste("Greenland temperature "," ", (degree~C))), x = NULL)+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"))+
  coord_cartesian(clip = "off")+
  theme(plot.margin = margin(2, 2, 2, 2, unit = "pt"))+
  annotate("text", y= max(T_Greenland$TGreenland), x =30000,label="(d)",fontface = 4,size=6)+
  theme(axis.text.y=element_text(size=13),axis.title.y=element_text(size=13))

p5<-ggplot(data = dD, aes(x = Age, y = Excess))+geom_point(size=0.5)+geom_line(size=0.4)+ 
  geom_vline(xintercept=DOp)+  
  scale_x_continuous(breaks=xbreak,limits=c(30000,50000),labels=xbreak/1000)+
  labs(y= expression(delta*D~"excess"),x="Age (kyr BP)")+
  theme(axis.text.x=element_text(size=10))+
  coord_cartesian(clip = "off")+
  theme(plot.margin = margin(2, 2, 2, 2, unit = "pt"))+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  annotate("text", y= max(dD$Excess), x =30000,label="(e)",fontface = 4,size=6)+
  theme(axis.text.x=element_text(size=13),axis.text.y=element_text(size=13),
        axis.title.x=element_text(size=13),axis.title.y=element_text(size=13))

p<-ggarrange(p1,p2,p3,p4,p5,ncol = 1)
ggsave(file="D:/PhD Project/Feedback paper/Data and codes/Output data/Plots/Time series.png",p,width=9,height=12)

#################################################################################################################
############################## Get simulated global mean temperature change     #################################
#################################################################################################################
DO_Tsurf <- read.csv("D:/PhD Project/Feedback paper/Data and codes/Original data/DO_Tsurf.csv",row.names=1)
lat<-c(85.8,80.3,74.7,69.2,63.7,58.1,52.6,47.1,41.5,36,30.5,24.9,19.4,13.8,8.3,2.8,-2.8,-8.3,-13.8,-19.4,-24.9,-30.5,-36,-41.5,-47.1,-52.6,-58.1,-63.7,-69.2,-74.7,-80.3,-85.8)
lon <- c(-174.4,-168.8,-163.1,-157.5,-151.9,-146.2,-140.6,-135,-129.4,-123.8,-118.1,-112.5,-106.9,-101.2,-95.6,-90,-84.4,-78.8,-73.1,-67.5,-61.9,-56.2,-50.6,-45,-39.4,-33.8,-28.1,-22.5,-16.9,-11.2,-5.6, 0, 5.6, 11.2, 16.9, 22.5, 28.1, 33.8, 39.4, 45, 50.6, 56.2, 61.9, 67.5, 73.1, 78.8, 84.4, 90, 95.6, 101.2, 106.9, 112.5, 118.1, 123.8, 129.4, 135, 140.6, 146.2, 151.9, 157.5, 163.1, 168.8, 174.4,180)
colnames(DO_Tsurf)<-c("lat",lon)

rows_to_delete<-c(1,1+33*seq(1:(nrow(DO_Tsurf)/33-1)))
DO_Tsurf_age<-DO_Tsurf[-rows_to_delete,]
nyear<-nrow(DO_Tsurf_age)/32 #32 is the number of rows per year map
DO_Tsurf_age$age<-rep(50000:(50000-nyear+1),each=32)
DO_Tsurf_age$lat<-rep(lat,nyear)

if(!require(reshape2)){ install.packages("reshape2");library(reshape2)}
DO_Tsurf_age_melt<-melt(DO_Tsurf_age,id.vars=c("age","lat"))
colnames(DO_Tsurf_age_melt)<-c("age","lat","lon","T")
DO_Tsurf_age_melt$lon<-as.numeric(as.character(DO_Tsurf_age_melt$lon))
write.csv(DO_Tsurf_age_melt,"D:/PhD Project/Feedback paper/Data and codes/Original data/DO_Tsurf_age_melt.csv")

########################## Bin in 25 years
DO_Tsurf_age_melt<-read.csv("D:/PhD Project/Feedback paper/Data and codes/Original data/DO_Tsurf_age_melt.csv",row.names = 1)
DO_Tsurf_age_melt$age<-ceiling(DO_Tsurf_age_melt$age/25)*25 #bin in 25 years
DO_Tsurf_binned<-aggregate(data=DO_Tsurf_age_melt,.~age+lat+lon,FUN=mean) #get the mean in each bin
DO_Tsurf_binned$sd_T<-aggregate(data=DO_Tsurf_age_melt,.~age+lat+lon,FUN=sd)[,"T"] #get the sd in each bin
write.csv(DO_Tsurf_binned,"D:/PhD Project/Feedback paper/Data and codes/Original data/DO_Tsurf_binned.csv")

######################## Get weighted global mean temperature
DO_Tsurf_binned<-read.csv("D:/PhD Project/Feedback paper/Data and codes/Original data/DO_Tsurf_binned.csv",row.names = 1)
DO_Tsurf_global_mean<-DO_Tsurf_binned
DO_Tsurf_global_mean$age<-as.factor(DO_Tsurf_global_mean$age)
Tglobal_w_mean<-data.frame()
for(i in 1:nlevels(DO_Tsurf_global_mean$age)){
  each<-DO_Tsurf_global_mean[which(DO_Tsurf_global_mean$age==levels(DO_Tsurf_global_mean$age)[i]),]
  each$w<-cos(each$lat*2*pi/360)
  Tw<-sum(each$T*each$w)/sum(each$w)
  sd_Tw<-sqrt(sum(each$sd_T^2*each$w^2))/sum(each$w)
  value<-cbind.data.frame(levels(DO_Tsurf_global_mean$age)[i],Tw,sd_Tw)
  colnames(value)<-c("age","Tw","sd_Tw")
  Tglobal_w_mean<-rbind.data.frame(Tglobal_w_mean,value)
}

Tglobal_w_mean$Tw_30ka<-Tglobal_w_mean[which(Tglobal_w_mean$age==30000),"Tw"]
Tglobal_w_mean$Tw_anomaly<-Tglobal_w_mean$Tw-Tglobal_w_mean$Tw_30ka
Tglobal_w_mean$sd_Tw_30ka<-Tglobal_w_mean[which(Tglobal_w_mean$age==30000),"sd_Tw"]
Tglobal_w_mean$sd_Tw_anomaly<-sqrt(Tglobal_w_mean$sd_Tw^2+Tglobal_w_mean$sd_Tw_30ka^2)

Tglobal_w_mean$age<-as.numeric(Tglobal_w_mean$age)
write.csv(Tglobal_w_mean,"D:/PhD Project/Feedback paper/Data and codes/Output data/Tglobal_w_mean.csv")

Tglobal_w_mean<-read.csv("D:/PhD Project/Feedback paper/Data and codes/Output data/Tglobal_w_mean.csv",row.names = 1)

p<-ggplot(data=Tglobal_w_mean,aes(age,Tw_anomaly))+geom_point(size=0.5)+geom_line(size=0.4)+ 
  geom_vline(xintercept=DOp)+  
  scale_x_continuous(breaks=xbreak,limits=c(30000,50000),labels=xbreak/1000)+
  labs(y= "Global mean temperature anomaly to 30 ka (K) ",x="Age (kyr BP)")+
  geom_text(data=DOdata,aes(x=DO, label=DO_lab, y=Inf),size=4.5, hjust=0.4, vjust=-0.6)+  
  coord_cartesian(clip = "off")+
  theme(plot.margin = margin(20, 2, 2, 2, unit = "pt"))+ 
  theme(panel.background = element_rect(fill = "white", colour = "black"))

ggsave(file="D:/PhD Project/Feedback paper/Data and codes/Output data/Plots/Tglobal_w_mean.png",p,width=8,height=5)

#################################################################################################################
############################## Get land/sea warming ratios in LOVECLIM simulations     ##########################
#################################################################################################################
if(!require(maptools)){ install.packages("maptools");library(maptools)}
data(wrld_simpl)
set.seed(0)
points<-DO_Tsurf_binned[,c("lon","lat")]
wrld_simpl@proj4string@projargs<-"+proj=longlat +datum=WGS84 +no_defs"
pts <- SpatialPoints(points, proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
wrld<-over(pts, wrld_simpl)

######################## Get land points
DO_Tsurf_land_all_binned<-DO_Tsurf_binned
DO_Tsurf_land_all_binned<-cbind.data.frame(DO_Tsurf_land_all_binned,wrld)
DO_Tsurf_land_all_binned<-DO_Tsurf_land_all_binned[!is.na(DO_Tsurf_land_all_binned$FIPS),]#get land_all points

#get weighted global mean land_all temperature
DO_Tsurf_land_all_mean<-DO_Tsurf_land_all_binned[,c("age","lat","lon","T")]
DO_Tsurf_land_all_mean$age<-as.factor(DO_Tsurf_land_all_mean$age)

Tland_all_w_mean<-data.frame()
for(i in 1:nlevels(DO_Tsurf_land_all_mean$age)){
  each<-DO_Tsurf_land_all_mean[which(DO_Tsurf_land_all_mean$age==levels(DO_Tsurf_land_all_mean$age)[i]),]
  each$w<-cos(each$lat*2*pi/360)
  Tw<-sum(each$T*each$w)/sum(each$w)
  value<-cbind.data.frame(levels(DO_Tsurf_land_all_mean$age)[i],Tw)
  colnames(value)<-c("age","Tw")
  Tland_all_w_mean<-rbind.data.frame(Tland_all_w_mean,value)
}

Tland_all_w_mean$Tw_30ka<-Tland_all_w_mean[which(Tland_all_w_mean$age==30000),"Tw"]
Tland_all_w_mean$Tw_anomaly<-Tland_all_w_mean$Tw-Tland_all_w_mean$Tw_30ka
Tland_all_w_mean$age<-as.numeric(Tland_all_w_mean$age)


##################### Get ocean points
DO_Tsurf_ocean_binned<-DO_Tsurf_binned
DO_Tsurf_ocean_binned<-cbind.data.frame(DO_Tsurf_ocean_binned,wrld)
DO_Tsurf_ocean_binned<-DO_Tsurf_ocean_binned[is.na(DO_Tsurf_ocean_binned$FIPS),]#get ocean points

#get weighted global mean ocean temperature
DO_Tsurf_ocean_mean<-DO_Tsurf_ocean_binned[,c("age","lat","lon","T")]
DO_Tsurf_ocean_mean$age<-as.factor(DO_Tsurf_ocean_mean$age)

Tocean_w_mean<-data.frame()
for(i in 1:nlevels(DO_Tsurf_ocean_mean$age)){
  each<-DO_Tsurf_ocean_mean[which(DO_Tsurf_ocean_mean$age==levels(DO_Tsurf_ocean_mean$age)[i]),]
  each$w<-cos(each$lat*2*pi/360)
  Tw<-sum(each$T*each$w)/sum(each$w)
  value<-cbind.data.frame(levels(DO_Tsurf_ocean_mean$age)[i],Tw)
  colnames(value)<-c("age","Tw")
  Tocean_w_mean<-rbind.data.frame(Tocean_w_mean,value)
}

Tocean_w_mean$Tw_30ka<-Tocean_w_mean[which(Tocean_w_mean$age==30000),"Tw"]
Tocean_w_mean$Tw_anomaly<-Tocean_w_mean$Tw-Tocean_w_mean$Tw_30ka
Tocean_w_mean$age<-as.numeric(Tocean_w_mean$age)

###################### compare
identical(Tglobal_w_mean$age,Tland_all_w_mean$age)
identical(Tglobal_w_mean$age,Tocean_w_mean$age)

compareT<-cbind.data.frame(Tglobal_w_mean$age,Tglobal_w_mean$Tw_anomaly,Tland_all_w_mean$Tw_anomaly,Tocean_w_mean$Tw_anomaly)
colnames(compareT)<-c("age","Tglobal_anomaly","Tland_all_anomaly","Tocean_anomaly")

p<-ggplot()+
  geom_point(data=compareT,aes(age,Tland_all_anomaly),size=0.5)+geom_line(data=compareT,aes(age,Tland_all_anomaly),size=0.4)+ 
  geom_point(data=compareT,aes(age,Tocean_anomaly),size=0.5,col="red")+geom_line(data=compareT,aes(age,Tocean_anomaly),size=0.4,col="red")+ 
  geom_vline(xintercept=DO)+  
  scale_x_continuous(breaks=xbreak,limits=c(30000,50000),labels=xbreak/1000)+
  labs(y= "Mean temperature anomaly to 30 ka (K) ",x="Age (kyr BP)")+
  geom_text(data=DOdata,aes(x=DO, label=DO_lab, y=Inf),size=4.5, hjust=0.4, vjust=-0.6)+  
  coord_cartesian(clip = "off")+
  theme(plot.margin = margin(20, 2, 2, 2, unit = "pt"))+ 
  theme(panel.background = element_rect(fill = "white", colour = "black"))

ggsave(file="D:/PhD Project/Feedback paper/Data and codes/Output data/Plots/Tland and ocean_w_mean.png",p,width=8,height=5)

sum<-summary(lm(data=compareT,Tland_all_anomaly~Tocean_anomaly-1))
ratio<-sum[["coefficients"]][,"Estimate"]
sd_ratio<-sum[["coefficients"]][,"Std. Error"]
round(ratio,digits = 2);round(sd_ratio*1.96,digits = 2)

ggplot(data=compareT,aes(Tocean_anomaly,Tland_all_anomaly))+theme_bw()+
  geom_point()+geom_abline(slope=ratio,intercept = 0)+
  geom_abline(slope=ratio-sd_ratio*1.96,intercept = 0,linetype="dashed")+
  geom_abline(slope=ratio+sd_ratio*1.96,intercept = 0,linetype="dashed")+
  labs(y= "Mean ocean temperature anomaly to 30 ka (K)",x="Mean land temperature anomaly to 30 ka (K)")

#############################################################################################
###################### Identify minimum and maximum of each event ###########################
#############################################################################################
setwd("D:/PhD Project/Feedback paper/Data and codes/Output data/Plots/Min and Max")
if(!require(ggplot2)){ install.packages("ggplot2");library(ggplot2)}
T_global<-read.csv("D:/PhD Project/Feedback paper/Data and codes/Output data/Tglobal_w_mean.csv",row.names = 1)
T_global<-T_global[,c("age","Tw_anomaly","sd_Tw_anomaly")]
store0<-data.frame(matrix(nrow=length(DO),ncol=2+4*4+2*4))
colnames(store0)<-c("k","DO","Cmin","Cmax","Cmin_err","Cmax_err","Mmin","Mmax","Mmin_err","Mmax_err","Nmin","Nmax","Nmin_err","Nmax_err","Tmin","Tmax","err_Tmin","err_Tmax","tmin_C","tmax_C","tmin_M","tmax_M","tmin_N","tmax_N","tmin_T","tmax_T")

win=2000;
DOdata$DO_Tglobal<-DOdata$DO
DOdata$DO_CO2<-DOdata$DO
DOdata$DO_CH4<-DOdata$DO
DOdata$DO_N2O<-DOdata$DO

DOdata[which(DOdata$DO_lab==9),"DO_Tglobal"]<-DOdata[which(DOdata$DO_lab==9),"DO"]+300
DOdata[which(DOdata$DO_lab==10),"DO_Tglobal"]<-DOdata[which(DOdata$DO_lab==10),"DO"]+400
DOdata[which(DOdata$DO_lab==11),"DO_Tglobal"]<-DOdata[which(DOdata$DO_lab==11),"DO"]+500
DOdata[which(DOdata$DO_lab==12),"DO_Tglobal"]<-DOdata[which(DOdata$DO_lab==12),"DO"]+500

DOdata[which(DOdata$DO_lab==6),"DO_CO2"]<-DOdata[which(DOdata$DO_lab==6),"DO"]+400
DOdata[which(DOdata$DO_lab==5),"DO_N2O"]<-DOdata[which(DOdata$DO_lab==5),"DO"]+400
DOdata[which(DOdata$DO_lab==6),"DO_N2O"]<-DOdata[which(DOdata$DO_lab==6),"DO"]+600
DOdata[which(DOdata$DO_lab==7),"DO_N2O"]<-DOdata[which(DOdata$DO_lab==7),"DO"]+600


for(k in 5:12){
  
  store0[k,c("k","DO")]<-c(k,DOdata[which(DOdata$DO_lab==k),"DO"])
  outputlist_Tglobal<-get_event(data=T_global,DO=DOdata$DO, DOv=DOdata$DO_Tglobal,k,win,min_left=200,min_right=200,duration=500,bin=25,ylab="Global mean temperature anomaly (K)" )
  
  outputlist_CO2<-get_event(data=CO2,DO=DOdata$DO,DOv=DOdata$DO_CO2,k,win,min_left=200,min_right=200,duration=500,bin=25,ylab=bquote(CO[2]~'(ppm)'))
  outputlist_CH4<-get_event(data=CH4,DO=DOdata$DO,DOv=DOdata$DO_CH4,k,win,min_left=200,min_right=200,duration=500,bin=25,ylab=bquote(CH[4]~'(ppb)'))
  outputlist_N2O<-get_event(data=N2O,DO=DOdata$DO,DOv=DOdata$DO_N2O,k,win,min_left=200,min_right=200,duration=600,bin=25,ylab=bquote(N[2]*'O  (ppb)'))
  
  store0[k,c("Cmin","Cmax","Cmin_err","Cmax_err","tmin_C","tmax_C")]<-outputlist_CO2[[1]]
  store0[k,c("Mmin","Mmax","Mmin_err","Mmax_err","tmin_M","tmax_M")]<-outputlist_CH4[[1]]
  store0[k,c("Nmin","Nmax","Nmin_err","Nmax_err","tmin_N","tmax_N")]<-outputlist_N2O[[1]]
  store0[k,c("Tmin","Tmax","err_Tmin","err_Tmax","tmin_T","tmax_T")]<-outputlist_Tglobal[[1]]
  
  p01<-outputlist_CO2[[2]]+theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
                                 axis.text.y=element_text(size=10),axis.title.y=element_text(size=10))
  p02<-outputlist_CH4[[2]]+theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
                                 axis.text.y=element_text(size=10),axis.title.y=element_text(size=10))
  p03<-outputlist_N2O[[2]]+theme(axis.text.x=element_text(size=10),axis.text.y=element_text(size=10),
                                 axis.title.x=element_text(size=10),axis.title.y=element_text(size=10))
  p04<-outputlist_Tglobal[[2]]+theme(axis.text.x=element_text(size=10),axis.text.y=element_text(size=10),
                                         axis.title.x=element_text(size=10),axis.title.y=element_text(size=10))
  
  if(!require(egg)){ install.packages("egg");library(egg)}
  p0<-ggarrange(p01,p02,p03,p04,ncol = 2,labels=c("(a)","(b)","(c)","(d)") )
  ggsave(file=paste("DO",k,".jpeg"),p0,width=9,height=7)
  gc()
  
}


store0$dt_C<-store0$tmin_C-store0$tmax_C
store0$dt_M<-store0$tmin_M-store0$tmax_M
store0$dt_N<-store0$tmin_N-store0$tmax_N
store0$dt_T<-store0$tmin_T-store0$tmax_T

store0$r_C<-(store0$Cmax-store0$Cmin)/(store0$tmin_C-store0$tmax_C)
store0$r_M<-(store0$Mmax-store0$Mmin)/(store0$tmin_M-store0$tmax_M)
store0$r_N<-(store0$Nmax-store0$Nmin)/(store0$tmin_N-store0$tmax_N)
store0$r_T<-(store0$Tmax-store0$Tmin)/(store0$tmin_T-store0$tmax_T)

store<-na.omit(store0)
write.csv(store,"D:/PhD Project/Feedback paper/Data and codes/Output data/store.csv")


#############################################################################################
######################### Get the temperature change map ###################################
#############################################################################################
if(!require(ggmap)){install.packages("ggmap");library(ggmap)}
if(!require(ggsn)){install.packages("ggsn");library(ggsn)}
if(!require(maps)){install.packages("maps");library(maps)}
if(!require(mapdata)){install.packages("mapdata");library(mapdata)}
if(!require(sf)){install.packages("sf");library(sf)}
if(!require(readxl)){ install.packages("readxl");library(readxl)}

store<-read.csv("D:/PhD Project/Feedback paper/Data and codes/Output data/store.csv", row.names=1)
DO_Tsurf_binned<-read.csv("D:/PhD Project/Feedback paper/Data and codes/Original data/DO_Tsurf_binned.csv",row.names = 1)

DO_records <- read_excel("D:/PhD Project/Feedback paper/Data and codes/Original data/DO records.xlsx")
for(i in 1:nrow(DO_records)){
  if(DO_records[i,"trend"]=="warmer"){
    DO_records[i,"arrowstart"]<-DO_records[i,"lat"]-3
    DO_records[i,"arrowend"]<-DO_records[i,"lat"]+3
  }else if(DO_records[i,"trend"]=="cooler"){
    DO_records[i,"arrowstart"]<-DO_records[i,"lat"]+3
    DO_records[i,"arrowend"]<-DO_records[i,"lat"]-3
  }
}
colnames(DO_records)
DO_records[,8:15]<-round(DO_records[,8:15],digits = 1)
nrow(DO_records)

############### Continuous scale

setwd("D:/PhD Project/Feedback paper/Data and codes/Output data/Plots/Map/Continuous scale")
world <- map_data("world") 
cols<-c("dodgerblue2","white","khaki","gold","darkorange","orangered","red")

for(iDO in 5:12){
  
  #get min, max and change
  tmin<-store[which(store$k==iDO),"tmin_T"]
  Tmap_min_iDO<-DO_Tsurf_binned[which(DO_Tsurf_binned$age>=(tmin-25/2)&DO_Tsurf_binned$age<(tmin+25/2)),]
  
  tmax<-store[which(store$k==iDO),"tmax_T"]
  Tmap_max_iDO<-DO_Tsurf_binned[which(DO_Tsurf_binned$age>=(tmax-25/2)&DO_Tsurf_binned$age<(tmax+25/2)),]
  
  print(identical(Tmap_min_iDO$lon,Tmap_max_iDO$lon)) #check whether lon  match
  print(identical(Tmap_min_iDO$lat,Tmap_max_iDO$lat)) #check whether lon  match
  
  Tmap_change_iDO<-cbind.data.frame(Tmap_min_iDO[,c("lat","lon")],Tmap_max_iDO$T-Tmap_min_iDO$T)
  colnames(Tmap_change_iDO)<-c("lat","lon","Tchange")
  print(summary(Tmap_change_iDO$Tchange))
  
  DO_records[,"DO"]<-DO_records[,(8+iDO-5)]
  for(i in 1:nrow(DO_records)){
    each_lat<-as.numeric(DO_records[i,"lat"])
    each_lon<-as.numeric(DO_records[i,"lon"])
    sim_DO<-Tmap_change_iDO[which(Tmap_change_iDO$lat>=(each_lat-180/32/2)&Tmap_change_iDO$lat<=(each_lat+180/32/2)&
                            Tmap_change_iDO$lon>=(each_lon-360/64/2)&Tmap_change_iDO$lon<=(each_lon+360/64/2)),]
    
      DO_records[i,"sim_DO_Tchange"]<-mean(sim_DO$Tchange,na.rm=T)
      
      if(mean(sim_DO$Tchange,na.rm=T)>0){
        DO_records[i,"sim_DO_trend"]<-"warmer"
      }else if(mean(sim_DO$Tchange,na.rm=T)<0){
        DO_records[i,"sim_DO_trend"]<-"cooler"
      }else{
        DO_records[i,"sim_DO_trend"]<-"no change"
      }

  }
  print(sum(DO_records$trend==DO_records$sim_DO_trend))

  DOmap<-ggplot() + theme_dark()+
    geom_point(data = Tmap_change_iDO, aes(x = lon, y = lat, color = Tchange), size = 5.5, shape=15,position="identity")+
    geom_polygon(data = world,aes(x=long, y = lat, group = group),alpha=0,color='grey40') +
    scale_color_gradientn(colours =cols,breaks=c(-8,0,8,16,24,32,40),limits=c(-8,40))+    
    labs(color=paste("DO",iDO,"temperature change (K)  "))+
    scale_x_continuous(breaks=c(-120,-60,0,60,120),labels=c("120¡ãW","60¡ãW","0¡ãE","60¡ãE","120¡ãE"))+
    scale_y_continuous(breaks=c(-60,-30,0,30,60),labels=c("60¡ãS","30¡ãS","0¡ãN","30¡ãN","60¡ãN"))+
    theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=11))+
    theme(legend.position = "bottom",legend.text = element_text(size=12),legend.key.width = unit(1, 'cm'))+
    geom_segment(data=DO_records, aes(x=lon, y=arrowstart, xend=lon, yend=arrowend),
                 arrow = arrow(length = unit(0.3,"cm"), angle = 20),size=1)+
    geom_text(data=DO_records, aes(x = lon, y = lat, label = DO),size=5,nudge_x = 0,nudge_y = -6.6)
  
  gc()
  
  ggsave(file=paste("D-O",iDO,".jpeg"),DOmap,width=8,height=6)
}

################# Discrete scale

setwd("D:/PhD Project/Feedback paper/Data and codes/Output data/Plots/Map/Discrete scale")
world <- map_data("world") 

for(iDO in 5:12){
  
  #get min, max and change
  tmin<-store[which(store$k==iDO),"tmin_T"]
  Tmap_min_iDO<-DO_Tsurf_binned[which(DO_Tsurf_binned$age>=(tmin-25/2)&DO_Tsurf_binned$age<(tmin+25/2)),]
  
  tmax<-store[which(store$k==iDO),"tmax_T"]
  Tmap_max_iDO<-DO_Tsurf_binned[which(DO_Tsurf_binned$age>=(tmax-25/2)&DO_Tsurf_binned$age<(tmax+25/2)),]
  
  Tmap_change_iDO<-cbind.data.frame(Tmap_min_iDO[,c("lat","lon")],Tmap_max_iDO$T-Tmap_min_iDO$T)
  colnames(Tmap_change_iDO)<-c("lat","lon","Tchange")

  DO_records[,"DO"]<-DO_records[,(8+iDO-5)]
  for(i in 1:nrow(DO_records)){
    each_lat<-as.numeric(DO_records[i,"lat"])
    each_lon<-as.numeric(DO_records[i,"lon"])
    sim_DO<-Tmap_change_iDO[which(Tmap_change_iDO$lat>=(each_lat-180/32/2)&Tmap_change_iDO$lat<=(each_lat+180/32/2)&
                                    Tmap_change_iDO$lon>=(each_lon-360/64/2)&Tmap_change_iDO$lon<=(each_lon+360/64/2)),]
    
    DO_records[i,"sim_DO_Tchange"]<-mean(sim_DO$Tchange,na.rm=T)
    
    if(mean(sim_DO$Tchange,na.rm=T)>0){
      DO_records[i,"sim_DO_trend"]<-"warmer"
    }else if(mean(sim_DO$Tchange,na.rm=T)<0){
      DO_records[i,"sim_DO_trend"]<-"cooler"
    }else{
      DO_records[i,"sim_DO_trend"]<-"no change"
    }
    
  }
  print(sum(DO_records$trend==DO_records$sim_DO_trend))
  
  Tmap_change_iDO$valuefactor <- cut(Tmap_change_iDO$Tchange, breaks = c(-6,-4,-2,0,2,4,6,10,40))
  library(RColorBrewer)
  cols=rev(brewer.pal(n = 11, name = "RdBu")[c(1:5,7:9)])
  names(cols)<-levels(Tmap_change_iDO$valuefactor)
  
  DOmap<-ggplot() + theme_bw()+
    geom_point(data = Tmap_change_iDO, aes(x = lon, y = lat, color = valuefactor), size = 5.5, shape=15,position="identity")+
    geom_polygon(data = world,aes(x=long, y = lat, group = group),alpha=0,color='grey40') +
    scale_color_manual(values=cols)+    
    labs(color=paste("DO",iDO,"temperature change (K)  "))+
    scale_x_continuous(breaks=c(-120,-60,0,60,120),labels=c("120¡ãW","60¡ãW","0¡ãE","60¡ãE","120¡ãE"))+
    scale_y_continuous(breaks=c(-60,-30,0,30,60),labels=c("60¡ãS","30¡ãS","0¡ãN","30¡ãN","60¡ãN"))+
    theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=11))+
    theme(legend.position = "bottom",legend.text = element_text(size=12),legend.key.width = unit(1, 'cm'))+
    geom_segment(data=DO_records, aes(x=lon, y=arrowstart, xend=lon, yend=arrowend),
                 arrow = arrow(length = unit(0.3,"cm"), angle = 20),size=1)+
    geom_text(data=DO_records, aes(x = lon, y = lat, label = DO),size=5,nudge_x = 0,nudge_y = -6.6)
  
  gc()
  
  ggsave(file=paste("D-O",iDO,".jpeg"),DOmap,width=8,height=6)
}

#########################################################################################################
##########################  Calculate radiative forcing with errors  ####################################
#########################################################################################################
if(!require(Deriv)){ install.packages("Deriv");library(Deriv)}
store<-read.csv("D:/PhD Project/Feedback paper/Data and codes/Output data/store.csv", row.names=1)

#Use IPCC WG1 AR5  Table 8.SM.1
fC<-function(C,C0,N_mean){(-2.4*10^(-7)*(C-C0)^2+7.2*10^(-4)*(C-C0)-2.1*10^(-4)*N_mean+5.36)*log(C/C0)}
fM<-function(M,M0,M_mean,N_mean){(-1.3*10^(-6)*M_mean-8.2*10^(-6)*N_mean+0.043)*(sqrt(M)-sqrt(M0))}
fN<-function(N,N0,C_mean,M_mean,N_mean){(-8.0*10^(-6)*C_mean+4.2*10^(-6)*N_mean-4.9*10^(-6)*M_mean+0.117)*(sqrt(N)-sqrt(N0))}


#Functions to propagate errors from radiative forcing equation
err_fC<-function(C,C0,N_mean,err_C,err_C0){
  DC<-Deriv(fC,"C");DC0<-Deriv(fC,"C0")
  sqrt( DC(C,C0,N_mean)^2*err_C^2 + DC0(C,C0,N_mean)^2*err_C0^2 )
}
err_fM<-function(M,M0,M_mean,N_mean,err_M,err_M0){
  DM<-Deriv(fM,"M");DM0<-Deriv(fM,"M0");DN0<-Deriv(fM,"N0")
  sqrt( DM(M,M0,M_mean,N_mean)^2*err_M^2 + DM0(M,M0,M_mean,N_mean)^2*err_M0^2 )
}
err_fN<-function(N,N0,C_mean,M_mean,N_mean,err_N,err_N0){
  DN<-Deriv(fN,"N");DM0<-Deriv(fN,"M0");DN0<-Deriv(fN,"N0")
  sqrt( DN(N,N0,C_mean,M_mean,N_mean)^2*err_N^2 + DN0(N,N0,C_mean,M_mean,N_mean)^2*err_N0^2 )
}


RF<-data.frame(matrix(nrow=nrow(store),ncol=2+2*3))
colnames(RF)<-c("k","DO","RC","err_RC","RM","err_RM","RN","err_RN")
RF[,c("k","DO")]<-store[,c("k","DO")]

store$C_mean<-(store$Cmax+store$Cmin)/2
store$M_mean<-(store$Mmax+store$Mmin)/2
store$N_mean<-(store$Nmax+store$Nmin)/2
store$C_mean_err<-sqrt(store$Cmax_err^2+store$Cmin_err^2)/2
store$M_mean_err<-sqrt(store$Mmax_err^2+store$Mmin_err^2)/2
store$N_mean_err<-sqrt(store$Nmax_err^2+store$Nmin_err^2)/2

RF$RC<-fC(store$Cmax,store$Cmin,store$N_mean)
RF$RM<-fM(store$Mmax,store$Mmin,store$M_mean,store$N_mean)
RF$RN<-fN(store$Nmax,store$Nmin,store$C_mean,store$M_mean,store$N_mean)

RF$err_RC<-err_fC(store$Cmax,store$Cmin,store$N_mean,store$Cmax_err,store$Cmin_err)
RF$err_RM<-err_fM(store$Mmax,store$Mmin,store$M_mean,store$N_mean,store$Mmax_err,store$Mmin_err)
RF$err_RN<-err_fN(store$Nmax,store$Nmin,store$C_mean,store$M_mean,store$N_mean,store$Nmax_err,store$Nmin_err)

RF$dT<-store$Tmax-store$Tmin
RF$err_dT<-sqrt(store$err_Tmax^2+store$err_Tmin^2)/2
write.csv(RF,"D:/PhD Project/Feedback paper/Data and codes/Output data/RF.csv")

#############################################################################################
################################### Plot RF and dT ##########################################
#############################################################################################
RF<-read.csv("D:/PhD Project/Feedback paper/Data and codes/Output data/RF.csv",row.names = 1)

plotRF<-RF
plotRF$k<-as.factor(plotRF$k)

plotRF$RCMN<-plotRF$RC+plotRF$RM+plotRF$RN
plotRF$err_RCMN<-sqrt(plotRF$err_RC^2+plotRF$err_RM^2+plotRF$err_RN^2)

#Get the correlation between RF and dT
round(cor(plotRF$RC,plotRF$dT, use="complete.obs"),digits=2)
round(cor(plotRF$RM,plotRF$dT, use="complete.obs"),digits=2)
round(cor(plotRF$RN,plotRF$dT, use="complete.obs"),digits=2)
round(cor(plotRF$RCMN,plotRF$dT, use="complete.obs"),digits=2)

cor.test(plotRF$RC,plotRF$dT, use="complete.obs")
cor.test(plotRF$RM,plotRF$dT, use="complete.obs")
cor.test(plotRF$RN,plotRF$dT, use="complete.obs")
cor.test(plotRF$RCMN,plotRF$dT, use="complete.obs")

#Deming regression
if(!require(deming)){ install.packages("deming");library(deming)}

fit<-deming(data=plotRF,RC~dT-1,xstd=err_dT,ystd=err_RC);print(fit);
bC<-fit[["coefficients"]][2];ci95_bC<-fit[["ci"]][2,];se_bc<-(max(ci95_bC)-min(ci95_bC))/(2*1.96)

fit<-deming(data=plotRF,RM~dT-1,xstd=err_dT,ystd=err_RM);print(fit);
bM<-fit[["coefficients"]][2];ci95_bM<-fit[["ci"]][2,];se_bM<-(max(ci95_bM)-min(ci95_bM))/(2*1.96)

fit<-deming(data=plotRF,RN~dT-1,xstd=err_dT,ystd=err_RN);print(fit);
bN<-fit[["coefficients"]][2];ci95_bN<-fit[["ci"]][2,];se_bN<-(max(ci95_bN)-min(ci95_bN))/(2*1.96)

fit<-deming(data=plotRF,RC+RM+RN~dT-1,xstd=err_dT,ystd=sqrt(err_RC^2+err_RM^2+err_RN^2));print(fit);
bCMN<-fit[["coefficients"]][2];ci95_bCMN<-fit[["ci"]][2,];se_bCMN<-(max(ci95_bCMN)-min(ci95_bCMN))/(2*1.96)

feedback_c<-rbind.data.frame(c(bC,ci95_bC,se_bc),c(bM,ci95_bM,se_bM),c(bN,ci95_bN,se_bN),c(bCMN,ci95_bCMN,se_bCMN))
rownames(feedback_c)<-c("CO2","CH4","N2O","combined");colnames(feedback_c)<-c("value","lowerlim","upperlim","se")
feedback_c$uncertainty<-feedback_c$se*1.96
write.csv(feedback_c,"D:/PhD Project/Feedback paper/Data and codes/Output data/feedback_c.csv")

setwd("D:/PhD Project/Feedback paper/Data and codes/Output data/Plots")
if(!require(ggplot2)){ install.packages("ggplot2");library(ggplot2)}

p1<-ggplot(data=plotRF,aes(dT,RC,label=k))+geom_point()+geom_text(vjust=1.5,hjust=-0.2,size=4)+
  geom_abline(slope=bC)+
  geom_abline(slope=ci95_bC[1],linetype="dashed")+
  geom_abline(slope=ci95_bC[2],linetype="dashed")+
  expand_limits(x=0,y=0)+
  theme_bw()+labs(y= bquote('Radiative forcing of '~ CO[2]~'('~ W ~ m ^-2~')'), 
                  x = "Global mean temperature increase (K)")+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
        axis.text=element_text(size=12),axis.title=element_text(size=12))+
  geom_pointrange(aes(xmin=dT-1.96*err_dT, xmax=dT+1.96*err_dT),size=0.3)+
  geom_pointrange(aes(ymin=RC-1.96*err_RC, ymax=RC+1.96*err_RC),size=0.3)+
  scale_y_continuous(labels = function(x) sprintf("%0.2f", x))

p2<-ggplot(data=plotRF,aes(dT,RM,label=k))+geom_point()+geom_text(vjust=1.5,hjust=-0.2,size=4)+
  geom_abline(slope=bM)+
  geom_abline(slope=ci95_bM[1],linetype="dashed")+
  geom_abline(slope=ci95_bM[2],linetype="dashed")+
  expand_limits(x=0,y=0)+
  theme_bw()+labs(y= bquote('Radiative forcing of '~CH[4]~'('~ W ~ m ^-2~')'), 
                  x = "Global mean temperature increase (K)")+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
        axis.text=element_text(size=12),axis.title=element_text(size=12))+
  geom_pointrange(aes(xmin=dT-1.96*err_dT, xmax=dT+1.96*err_dT),size=0.3)+
  geom_pointrange(aes(ymin=RM-1.96*err_RM, ymax=RM+1.96*err_RM),size=0.3)+
  scale_y_continuous(labels = function(x) sprintf("%0.2f", x))

p3<-ggplot(data=plotRF,aes(dT,RN,label=k))+geom_point()+geom_text(vjust=1.5,hjust=-0.2,size=4)+
  geom_abline(slope=bN)+
  geom_abline(slope=ci95_bN[1],linetype="dashed")+
  geom_abline(slope=ci95_bN[2],linetype="dashed")+
  expand_limits(x=0,y=0)+
  theme_bw()+labs(y= bquote('Radiative forcing of '~N[2]*O~'('~ W ~ m ^-2~')'), 
                  x = "Global mean temperature increase (K)")+
  geom_pointrange(aes(xmin=dT-1.96*err_dT, xmax=dT+1.96*err_dT),size=0.3)+
  geom_pointrange(aes(ymin=RN-1.96*err_RN, ymax=RN+1.96*err_RN),size=0.3)+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=12))+
  scale_y_continuous(labels = function(x) sprintf("%0.2f", x))+
  scale_x_continuous(labels = function(x) sprintf("%0.1f", x))

p4<-ggplot(data=plotRF,aes(dT,RCMN,label=k))+geom_point()+geom_text(vjust=1.5,hjust=-0.2,size=4)+
  geom_abline(slope=bCMN)+
  geom_abline(slope=ci95_bCMN[1],linetype="dashed")+
  geom_abline(slope=ci95_bCMN[2],linetype="dashed")+
  expand_limits(x=0,y=0)+
  theme_bw()+labs(y= bquote('Combined radiative forcing ('~ W ~ m ^-2~')'), 
                  x = "Global mean temperature increase (K)")+
  geom_pointrange(aes(xmin=dT-1.96*err_dT, xmax=dT+1.96*err_dT),size=0.3)+
  geom_pointrange(aes(ymin=RCMN-1.96*err_RCMN, ymax=RCMN+1.96*err_RCMN),size=0.3)+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=12))+
  scale_y_continuous(labels = function(x) sprintf("%0.2f", x))+
  scale_x_continuous(labels = function(x) sprintf("%0.1f", x))

if(!require(egg)){ install.packages("egg");library(egg)}
p<-ggarrange(p1,p2,p3,p4,ncol = 2,labels=c("(a)","(b)","(c)","(d)"),
             label.args = list(hjust=-3.5,vjust= 3,gp = grid::gpar(font = 4,cex=1.2)) )
ggsave(file="Feedback regression.jpeg",p,width=9,height=8)

#############################################################################################
################################### Feedback gain ###########################################
#############################################################################################
#################### ECS in this paper
#ECS, assume differenct sources are independent
ECS<-cbind.data.frame(c(2.8,3.5,3.4),c(0.61,0.61,0.54))
colnames(ECS)<-c("ECS","se_ECS")
ECS$delta_TR<-ECS$ECS/3.7
ECS$se_delta_TR<-ECS$se_ECS/3.7

S<-mean(ECS$delta_TR)
se_S<-sqrt(sum(ECS$se_delta_TR^2))/nrow(ECS)
S;se_S;1.96*se_S
S*3.7;1.96*se_S*3.7
#Calculate feedback gain
value<-feedback_c$value*S
se<-sqrt(S^2*feedback_c$se^2+feedback_c$value^2*se_S^2)
feedback_gain<-cbind.data.frame(value,se)
rownames(feedback_gain)<-c("CO2","CH4","N2O","Combined")
feedback_gain$uncertainty<-feedback_gain$se*1.96
write.csv(feedback_gain,"D:/PhD Project/Feedback paper/Data and codes/Output data/feedback_gain.csv")

round((1/(1-feedback_gain$value+feedback_gain$se*1.96)-1)*100,digits=0)
round((1/(1-feedback_gain$value-feedback_gain$se*1.96)-1)*100,digits=0)

################### ECS in IPCC AR6
S<-3/3.7
se_S<-0.5/1.96/3.7
S;se_S;1.96*se_S
value<-feedback_c$value*S
se<-sqrt(S^2*feedback_c$se^2+feedback_c$value^2*se_S^2)
feedback_gain<-cbind.data.frame(value,se)
rownames(feedback_gain)<-c("CO2","CH4","N2O","Combined")
feedback_gain$uncertainty<-feedback_gain$se*1.96
write.csv(feedback_gain,"D:/PhD Project/Feedback paper/Data and codes/Output data/feedback_gain_AR6.csv")

round((1/(1-feedback_gain$value+feedback_gain$se*1.96)-1)*100,digits=0)
round((1/(1-feedback_gain$value-feedback_gain$se*1.96)-1)*100,digits=0)

#############################################################################################
################################# Results to compare ########################################
#############################################################################################
ECS<-cbind.data.frame(c(2.8,3.5,3.4),c(0.61,0.61,0.54))
colnames(ECS)<-c("ECS","se_ECS")
ECS$delta_TR<-ECS$ECS/3.7
ECS$se_delta_TR<-ECS$se_ECS/3.7

S<-mean(ECS$delta_TR)
se_S<-sqrt(sum(ECS$se_delta_TR^2))/nrow(ECS)
S;se_S;1.96*se_S
S*3.7;1.96*se_S*3.7

compare_gain<-data.frame(matrix(ncol=9))
colnames(compare_gain)<-c("variable","source","paper","c","se_c","uncertainty_c","g","se_g","uncertainty_g")
################# D-O events ###################
feedback_c <- read.csv("D:/PhD Project/Feedback paper/Data and codes/Output data/feedback_c.csv")
feedback_gain <- read.csv("D:/PhD Project/Feedback paper/Data and codes/Output data/feedback_gain.csv")
compare_gain[1:3,c("c","se_c","uncertainty_c")]<-feedback_c[1:3,c("value","se","uncertainty")]
compare_gain[1:3,c("g","se_g","uncertainty_g")]<-feedback_gain[1:3,c("value","se","uncertainty")]
compare_gain[1:3,"variable"]<-c("CO2","CH4","N2O")
compare_gain[1:3,"source"]<-"D-O events"
compare_gain[1:3,"paper"]<-"This paper"

################### The modelled feedbacks ###############
#C4MIP 11 models 
k=4;
compare_gain[k:(k+11-1),"variable"]<-"CO2"
compare_gain[k:(k+11-1),"source"]<-"Models"
compare_gain[k:(k+11-1),"paper"]<-"Friedlingstein et al. (2006)"
#c("HadCM3LC","IPSL-CM2C","IPSL-CM4-LOOP","CSM-1","MPI","LLNL","FRCGC","UMD","UVic-2.7","CLIMBER","BERN-CC")
compare_gain[k:(k+11-1),"g"]<-c(0.31,0.15,0.06,0.04,0.2,0.1,0.21,0.14,0.2,0.1,0.13)
compare_gain[k:(k+11-1),"c"]<-c(0.31/0.0066,0.15/0.0065,0.06/0.0072,0.04/0.0038,0.2/0.0082,0.1/0.0068,
                                0.21/0.0059,0.14/0.0056,0.2/0.0063,0.1/0.0053,0.13/0.0046)*3.7/280

#CMIP5 9 models 
k=(k+11-1)+1
compare_gain[k:(k+9-1),"variable"]<-"CO2"
compare_gain[k:(k+9-1),"source"]<-"Models"
compare_gain[k:(k+9-1),"paper"]<-"Arora et al. (2013)"
#c("MPI","IPSL","BCC","UKMO","UVic","CCCma","NCC","NCAR","JAMSTEC")
compare_gain[k:(k+9-1),"g"]<-c(0.11,0.08,0.09,0.05,0.11,0.12,0.03,0.03,0.18)#CMIP5 gE^, Arora et al. (2013)
gammaA<-c(92.2,64.8,87.6,40.1,85.8,79.7,21.4,23.8,100.7)
betaA<-c(-2.29,-2.04,-2.19,-1.95,-1.75,-1.65,-1.07,-0.96,-1.56)
m<-2.12
#-(gammaL+gammaO)/(m+betaL+betaO)=-(-gammaA)/(m-betaA)
compare_gain[k:(k+9-1),"c"]<-(gammaA/(m-betaA))*(3.7/280)

#CIMP6 11 models
k=(k+9-1)+1
compare_gain[k:(k+11-1),"variable"]<-"CO2"
compare_gain[k:(k+11-1),"source"]<-"Models"
compare_gain[k:(k+11-1),"paper"]<-"Arora et al. (2020)"
#c("ACCESS-ESM1.5","BCC-CSM2-MR","CanESM5","CESM2","CNRM-ESM2-1","IPSL-CM6A-LR","MIROC-ES2L","MPI-ESM1.2-LR","NOAA-GFDL-ESM4","NorESM2-LM","UKESM1-0-LL")
gammaL<-c(-21.1, -163.1, 15.95, -21.6, -83.11, -8.67, -69.57, -5.17 ,-80.06, -20.95, -38.4)
betaL<-c(0.37,1.81,1.28,0.90,1.36,0.62,1.12,0.71,0.93,0.85,0.75)
gammaO<-c(-23.75, -19.94, -14.72, -10.85, -9.38, -12.97, -22.25, -20.11, -21.65, -19.64, -14.07)
betaO<-c(0.90,0.92,0.77,0.71,0.70,0.76,0.73,0.77,0.84,0.78,0.75)
m<-2.12
alpha<-c(0.00546,0.00485,0.00751,0.00637,0.00632,0.00687,0.00436,0.00512,0.00430,0.00410,0.00721)
#-(gammaL+gammaO)/(m+betaL+betaO)
compare_gain[k:(k+11-1),"c"]<- (-(gammaL+gammaO)/(m+betaL+betaO))*(3.7/280)
compare_gain[k:(k+11-1),"g"]<-(-(gammaL+gammaO)*alpha/(m+betaL+betaO))


#Xu-Ri et al. (2012) N2O
k=(k+11-1)+1
#Convert radiative forcing with uncertainites to g, Gedney et al. (2019)
RN_dT<-0.11;se_RN_dT<-0
gN<-S*RN_dT
se_gN<-sqrt(S^2*se_RN_dT^2+RN_dT^2*se_S^2)
gN;se_gN;se_gN*1.96
compare_gain[k,]<-c("N2O","Models","Xu-Ri et al. (2012)",RN_dT,se_RN_dT, se_RN_dT*1.96, gN,se_gN,se_gN*1.96)

#Stocker et al. (2013) LPX-Bern
#Convert radiative forcing with uncertainites to g, Gedney et al. (2019)
k=k+1
RC_dT<-0.07932;se_RC_dT<-0
gC<-S*RC_dT
se_gC<-sqrt(S^2*se_RC_dT^2+RC_dT^2*se_S^2)
gC;se_gC;se_gC*1.96
compare_gain[k,]<-c("CO2","Models","Stocker et al. (2013)", RC_dT,se_RC_dT,se_RC_dT*1.96,gC,se_gC,se_gC*1.96)

k=k+1
RM_dT<-0.01085;se_RM_dT<-0
gM<-S*RM_dT
se_gM<-sqrt(S^2*se_RM_dT^2+RM_dT^2*se_S^2)
gM;se_gM;se_gM*1.96
compare_gain[k,]<-c("CH4","Models","Stocker et al. (2013)", RM_dT,se_RM_dT,se_RM_dT*1.96,gM,se_gM,se_gM*1.96)

k=k+1
RN_dT<-0.02344;se_RN_dT<-0
gN<-S*RN_dT
se_gN<-sqrt(S^2*se_RN_dT^2+RN_dT^2*se_S^2)
gN;se_gN;se_gN*1.96
compare_gain[k,]<-c("N2O","Models","Stocker et al. (2013)", RN_dT,se_RN_dT, se_RN_dT*1.96,gN,se_gN,se_gN*1.96)

#IPCC AR6
k=k+1
RM_dT<-0.03
se_RM_dT<-0.01
gM<-S*RM_dT
se_gM<-sqrt(S^2*se_RM_dT^2+RM_dT^2*se_S^2)
gM;se_gM;se_gM*1.96
compare_gain[k,]<-c("CH4","Models","IPCC AR6", RM_dT,se_RM_dT,se_RM_dT*1.96,gM,se_gM,se_gM*1.96)

k=k+1
RN_dT<-0.02-0.008
se_RN_dT<-sqrt(0.01^2+0.002^2)/1.96 #assuming it to be 95% confidence interval
gN<-S*RN_dT
se_gN<-sqrt(S^2*se_RN_dT^2+RN_dT^2*se_S^2)
gN;se_gN;se_gN*1.96
compare_gain[k,]<-c("N2O","Models","IPCC AR6", RN_dT,se_RN_dT, se_RN_dT*1.96,gN,se_gN,se_gN*1.96)

############### Modern observations ########################
k=k+1
#Convert radiative forcing with uncertainites to g, Gedney et al. (2019)
RM_dT<-(0.01+0.11)/2;se_RM_dT<-(0.11-0.01)/(2*1.96)
gM<-S*RM_dT
se_gM<-sqrt(S^2*se_RM_dT^2+RM_dT^2*se_S^2)
gM;se_gM;se_gM*1.96
compare_gain[k,]<-c("CH4","Modern","Gedney et al. (2019)", RM_dT,se_RM_dT,se_RM_dT*1.96,gM,se_gM,se_gM*1.96)

##################### Little ice age###########################
#Using the 95% range as the 95% confidence interval
PAGES2k <- read.csv("D:/PhD Project/Feedback paper/Data and codes/Original data/PAGES2k temperature.csv")
PAGES2k$T<-(PAGES2k$Full.ensemble.97.5th.percentile+PAGES2k$Full.ensemble.2.5th.percentile)/2
PAGES2k$err_T<-(PAGES2k$Full.ensemble.97.5th.percentile-PAGES2k$Full.ensemble.2.5th.percentile)/(2*1.96)

#Little ice age is cooling instead of warming, so the start date has the maximum
event<-PAGES2k[,c("Year","T","err_T")]
tnow=1200;win=1000;bin=25;max_left=200;max_right=200;duration=300
yearb<-seq((tnow-win)+bin/2,(tnow+win)-bin/2,by=bin)
valueb<-tapply(event$T,cut(event$Year,seq((tnow-win),(tnow+win),by=bin)),mean)
errb<-tapply(event$err_T,cut(event$Year,seq((tnow-win),(tnow+win),by=bin)),function(x){sqrt(sum(x^2))/length(x)})

eventb<-cbind.data.frame(yearb,valueb,errb);colnames(eventb)<-c("Year","Value","Err");eventb<-na.omit(eventb)

eventbmax<-eventb[which(eventb$Year>=(tnow-max_left)&eventb$Year<=(tnow+max_right)),]
Tmax<-max(eventbmax[,"Value"])
Tmax_err<-eventbmax[which.max(eventbmax[,"Value"]),"Err"]
tmax<-eventbmax[which.max(eventbmax[,"Value"]),"Year"]

eventbmin<-eventb[which(eventb$Year<=(tmax+duration)&eventb$Year>=tmax),]
Tmin<-min(eventbmin[,"Value"])
Tmin_err<-eventbmin[which.min(eventbmin[,"Value"]),"Err"]
tmin<-eventbmin[which.min(eventbmin[,"Value"]),"Year"]

tmin-tmax

ggplot(data=eventb,aes(Year,Value))+geom_point()+geom_line()+
  geom_hline(yintercept = Tmax,linetype="dashed")+geom_hline(yintercept = Tmin,linetype="dashed")+
  geom_segment(x=tmin,xend=tmin, y=Tmin-Tmin_err,yend=Tmin+Tmin_err,color="red3",size=0.8)+
  geom_segment(x=tmax,xend=tmax, y=Tmax-Tmax_err,yend=Tmax+Tmax_err,color="red3",size=0.8)

dT_LIA<-Tmax-Tmin
err_dT_LIA<-sqrt(Tmax_err^2+Tmin_err^2)
dT_LIA;err_dT_LIA

###
k=k+1
#Convert amplification to g, Scheffer et al. (2006) 
amp_min<-1.07;gC_min<-1-1/amp_min
amp_max<-1.25;gC_max<-1-1/amp_max
gC<-(gC_min+gC_max)/2;uncertainty_gC<-(gC_max-gC_min)/2;se_gC<-uncertainty_gC/1.96
compare_gain[k,]<-c("CO2","Little Ice Age","Scheffer et al. (2006) using Moberg et al.", 
                    (gC/0.0107)*(3.7/280),(se_gC/0.0107)*(3.7/280),(se_gC*1.96/0.0107)*(3.7/280),gC,se_gC,se_gC*1.96)

amp_min<-1.28;gC_min<-1-1/amp_min
amp_max<-2.93;gC_max<-1-1/amp_max
gC<-(gC_min+gC_max)/2;uncertainty_gC<-(gC_max-gC_min)/2;se_gC<-uncertainty_gC/1.96
compare_gain[k+1,]<-c("CO2","Little Ice Age","Scheffer et al. (2006) using Mann & Jones", 
                      (gC/0.0107)*(3.7/280),(se_gC/0.0107)*(3.7/280),(se_gC*1.96/0.0107)*(3.7/280),gC,se_gC,se_gC*1.96)

###########Scheffer et al. (2006) recalculated
CO2_LIA_Siegenthaler<- read.csv("D:/PhD Project/Feedback paper/Data and codes/Original data/CO2_little ice age_Siegenthaler et al., 2005.csv")
colnames(CO2_LIA_Siegenthaler)<-c("Depth","AgeDomeC","Year","CO2_ppmv","CO2_ppmv_gravitational_corrected","sd_CO2_ppmv")
ggplot(data=CO2_LIA_Siegenthaler,aes(Year,CO2_ppmv))+geom_point()+geom_line()

#Little ice age is cooling instead of warming, so the start date has the maximum
event<-CO2_LIA_Siegenthaler
tnow=1500;win=2000;bin=25;max_left=200;max_right=200;duration=500
yearb<-seq((tnow-win)+bin/2,(tnow+win)-bin/2,by=bin)
valueb<-tapply(event$CO2_ppmv,cut(event$Year,seq((tnow-win),(tnow+win),by=bin)),mean)
errb<-tapply(event$sd_CO2_ppmv,cut(event$Year,seq((tnow-win),(tnow+win),by=bin)),function(x){sqrt(sum(x^2))/length(x)})

eventb<-cbind.data.frame(yearb,valueb,errb);colnames(eventb)<-c("Year","Value","Err");eventb<-na.omit(eventb)

eventbmax<-eventb[which(eventb$Year>=(tnow-max_left)&eventb$Year<=(tnow+max_right)),]
Cmax<-max(eventbmax[,"Value"])
Cmax_err<-eventbmax[which.max(eventbmax[,"Value"]),"Err"]
tmax<-eventbmax[which.max(eventbmax[,"Value"]),"Year"]

eventbmin<-eventb[which(eventb$Year<=(tmax+duration)&eventb$Year>=tmax),]
Cmin<-min(eventbmin[,"Value"])
Cmin_err<-eventbmin[which.min(eventbmin[,"Value"]),"Err"]
tmin<-eventbmin[which.min(eventbmin[,"Value"]),"Year"]

tmin-tmax

ggplot(data=eventb,aes(Year,Value))+geom_point()+geom_line()+
  geom_hline(yintercept = Cmax,linetype="dashed")+geom_hline(yintercept = Cmin,linetype="dashed")+
  geom_segment(x=tmin,xend=tmin, y=Cmin-Cmin_err,yend=Cmin+Cmin_err,color="red3",size=0.8)+
  geom_segment(x=tmax,xend=tmax, y=Cmax-Cmax_err,yend=Cmax+Cmax_err,color="red3",size=0.8)

#get radiative forcing
N_mean<-(288+281)/2
RC_LIA<-fC(Cmax,Cmin,N_mean)
err_RC_LIA<-err_fC(Cmax,Cmin,N_mean,Cmax_err,Cmin_err)
RC_LIA;err_RC_LIA

#ECS, assume differenct sources are independent
S;se_S;1.96*se_S

#get gain
RC_dT<-RC_LIA/dT_LIA
err_RC_dT<-(RC_LIA/dT_LIA)*sqrt(err_RC_LIA^2/RC_LIA^2+err_dT_LIA^2/dT_LIA^2)

gC<-S*RC_dT
err_gC<-sqrt(S^2*err_RC_dT^2+RC_dT^2*se_S^2)
gC;err_gC;err_gC*1.96

compare_gain[k+2,]<-c("CO2","Little Ice Age","Scheffer et al. (2006) recalculation", RC_dT,err_RC_dT,err_RC_dT*1.96,gC,se_gC,se_gC*1.96)


#Convert dC/dT to g, Cox and Jones (2008)
k=k+3
mean_ECS<-mean(ECS$ECS);
se_mean_ECS<-sqrt(sum(ECS$se_ECS^2))/nrow(ECS)
mean_ECS;1.96*se_mean_ECS

d<-mean_ECS/280;se_d<-se_mean_ECS/280
d;se_d;1.96*se_d

dC_dT<-40;se_dC_dT<-20/1.96 

gC<-d*dC_dT
se_gC<-sqrt(d^2*se_dC_dT^2+dC_dT^2*se_d^2)
gC;se_gC;se_gC*1.96
compare_gain[k,]<-c("CO2","Little Ice Age","Cox & Jones (2008) using Moberg et al.", 
                    dC_dT*3.7/280,se_dC_dT*3.7/280,se_dC_dT*1.96*3.7/280,gC,se_gC,se_gC*1.96)

#Convert concentration to radiative forcing, then gains, Khalil and Rasmussen (1989) 
k=k+1
Mmax<-716;Mmin<-678
M_mean<-(Mmax+Mmin)/2
Mmax_err<-16/1.645;Mmin_err<-17/1.645 #the paper use 90% confidence interval, so should be divided by 1.645
Nmax<-288;Nmin<-281
N_mean<-(Nmax+Nmin)/2
Nmax_err<-3/1.645;Nmin_err<-3/1.645#the paper use 90% confidence interval, so should be divided by 1.645
C_mean<-(Cmax+Cmin)/2

dT<-1.05*2/3#If we assume the global mean temperature to be 2/3 of northern hemisphere temperature

RM<-fM(Mmax,Mmin,M_mean,N_mean)
RN<-fN(Nmax,Nmin,C_mean,M_mean,N_mean)
err_RM<-err_fM(Mmax,Mmin,M_mean,N_mean,Mmax_err,Mmin_err)
err_RN<-err_fN(Nmax,Nmin,C_mean,M_mean,N_mean,Nmax_err,Nmin_err)

gM<-S*(RM/dT)
gN<-S*(RN/dT)
err_gM<-sqrt(S^2*(err_RM/dT)^2+(RM/dT)^2*se_S^2)
err_gN<-sqrt(S^2*(err_RN/dT)^2+(RN/dT)^2*se_S^2)

gM;err_gM;err_gM*1.96
gN;err_gN;err_gN*1.96

compare_gain[k,]<-c("CH4","Little Ice Age","Khalil & Rasmussen (1989)",RM/dT, err_RM/dT,1.96*err_RM/dT,gM,err_gM,err_gM*1.96)
compare_gain[k+1,]<-c("N2O","Little Ice Age","Khalil & Rasmussen (1989)", RN/dT,err_RN/dT,1.96*err_RN/dT,gN,err_gN,err_gN*1.96)

##########Khalil and Rasmussen (1989) recalculation
k=k+2
Mmax<-716;Mmin<-678
M_mean<-(Mmax+Mmin)/2
Mmax_err<-16/1.645;Mmin_err<-17/1.645 #the paper use 90% confidence interval, so should be divided by 1.645
Nmax<-288;Nmin<-281
N_mean<-(Nmax+Nmin)/2
Nmax_err<-3/1.645;Nmin_err<-3/1.645#the paper use 90% confidence interval, so should be divided by 1.645
C_mean<-(Cmax+Cmin)/2

dT<-dT_LIA;
err_dT<-err_dT_LIA

RM<-fM(Mmax,Mmin,M_mean,N_mean)
RN<-fN(Nmax,Nmin,C_mean,M_mean,N_mean)
err_RM<-err_fM(Mmax,Mmin,M_mean,N_mean,Mmax_err,Mmin_err)
err_RN<-err_fN(Nmax,Nmin,C_mean,M_mean,N_mean,Nmax_err,Nmin_err)

RM_dT<-RM/dT
RN_dT<-RN/dT
err_RM_dT<-(RM/dT)*sqrt(err_RM^2/RM^2+err_dT^2/dT^2)
err_RN_dT<-(RN/dT)*sqrt(err_RN^2/RN^2+err_dT^2/dT^2)

gM<-S*RM_dT
gN<-S*RN_dT
err_gM<-sqrt(S^2*err_RM_dT^2+RM_dT^2*se_S^2)
err_gN<-sqrt(S^2*err_RN_dT^2+RN_dT^2*se_S^2)

gM;err_gM;err_gM*1.96
gN;err_gN;err_gN*1.96

#already using the global mean temperature 
compare_gain[k,]<-c("CH4","Little Ice Age","Khalil & Rasmussen (1989) recalculation", RM_dT,err_RM_dT,err_RM_dT*1.96,gM,err_gM,err_gM*1.96)
compare_gain[k+1,]<-c("N2O","Little Ice Age","Khalil & Rasmussen (1989) recalculation", RN_dT,err_RN_dT,err_RN_dT*1.96,gN,err_gN,err_gN*1.96)


##Get the 95% confidence interval
row.names(compare_gain)<-seq(1:nrow(compare_gain))
for(j in 4:ncol(compare_gain)){
  compare_gain[,j]<-as.numeric(compare_gain[,j])
}
compare_gain$c_ci95_low<-compare_gain$c - compare_gain$se_c *1.96
compare_gain$c_ci95_high<-compare_gain$c + compare_gain$se_c *1.96
compare_gain$g_ci95_low<-compare_gain$g - compare_gain$se_g *1.96
compare_gain$g_ci95_high<-compare_gain$g + compare_gain$se_g *1.96

write.csv(compare_gain,"D:/PhD Project/Feedback paper/Data and codes/Output data/compare_gain.csv")

#############################################################################################
################################### Plot feedback comparison ###############################
#############################################################################################
compare_gain <- read.csv("D:/PhD Project/Feedback paper/Data and codes/Output data/compare_gain.csv", row.names=1)
setwd("D:/PhD Project/Feedback paper/Data and codes/Output data/Plots")
if(!require(ggplot2)){ install.packages("ggplot2");library(ggplot2)}
plot_gain<-compare_gain
#plot_gain$source<-factor(plot_gain$source,levels =c("D-O events","Models", "Little Ice Age","Modern"))
plot_gain$source<-factor(plot_gain$source,levels =c("Models", "Modern", "Little Ice Age", "D-O events"))
library(gridExtra)
library(grid)

#CO2
plot_gC<-plot_gain[which(plot_gain$variable=="CO2"),]
rownames(plot_gC)<-seq(1:nrow(plot_gC))
plot_gC$sequence<-c(1,rep(2,11),rep(3,9),rep(4,11),5,6,7,8,9)
plot_gC$sequence<-as.factor(plot_gC$sequence)
plot_gC$paper
gClabel<-plot_gC$paper
gClabel[2:6]<-" "
gClabel[8:15]<-" "
gClabel[17:24]<-" "
gClabel[26:32]<-" "
gClabel

min(plot_gC$g,plot_gC$g_ci95_low,na.rm=T)
max(plot_gC$g,plot_gC$g_ci95_high,na.rm=T)
pg_CO2<-ggplot(plot_gC, aes(y=sequence, x=g,shape=source,label=gClabel)) +theme_bw()+
  geom_rect(aes(xmin=plot_gC[source=="D-O events","g_ci95_low"], 
                xmax=plot_gC[source=="D-O events","g_ci95_high"], 
                ymin=-Inf, ymax=Inf),alpha=0.2,fill="gray88")+
  geom_point()+geom_text(hjust=0.27,vjust=1.5,size=3.5)+
  geom_pointrange(xmin=plot_gC$g_ci95_low,xmax=plot_gC$g_ci95_high)+
  theme(axis.title.y=element_blank())+labs(x = NULL)+
  scale_x_continuous(limits=c(-0.05,0.75),breaks=0.1*seq(0,7),labels = function(x) sprintf("%0.2f", x))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),
        axis.text.x=element_text(size=10),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", y= 9, x =-0.05+0.8*0.015,label=bquote(italic("(a)")~CO[2]),size=4)+
  scale_shape_manual(values = c('Models'=17, 'Little Ice Age'=15, 'D-O events'=16))

min(plot_gC$c,plot_gC$c_ci95_low,na.rm=T)
max(plot_gC$c,plot_gC$c_ci95_high,na.rm=T)
pc_CO2<-ggplot(plot_gC, aes(y=sequence, x=c,shape=source,label=gClabel)) +theme_bw()+
  geom_rect(aes(xmin=plot_gC[source=="D-O events","c_ci95_low"], 
                xmax=plot_gC[source=="D-O events","c_ci95_high"], 
                ymin=-Inf, ymax=Inf),alpha=0.2,fill="gray88")+
  geom_point()+geom_text(hjust=0.27,vjust=1.5,size=3.5)+
  geom_pointrange(xmin=plot_gC$c_ci95_low,xmax=plot_gC$c_ci95_high)+
  theme(axis.title.y=element_blank())+labs(x = NULL)+
  scale_x_continuous(limits=c(-0.05,0.85),breaks=0.1*seq(0,8),labels = function(x) sprintf("%0.2f", x))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),
        axis.text.x=element_text(size=10),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", y= 9, x =-0.05+0.9*0.015,label=bquote(italic("(a)")~CO[2]),size=4)+
  scale_shape_manual(values = c('Models'=17, 'Little Ice Age'=15, 'D-O events'=16))


#CH4
plot_gM<-plot_gain[which(plot_gain$variable=="CH4"),]
rownames(plot_gM)<-seq(1:nrow(plot_gM))
plot_gM$sequence<-c(1,2,3,4,5,6)
plot_gM$sequence<-as.factor(plot_gM$sequence)

pg_CH4<-ggplot(plot_gM, aes(y=sequence, x=g,shape=source,label=paper)) +theme_bw()+
  geom_rect(aes(xmin=plot_gM[source=="D-O events","g_ci95_low"], 
                xmax=plot_gM[source=="D-O events","g_ci95_high"], 
                ymin=-Inf, ymax=Inf),alpha=0.2,fill="gray80")+
  geom_point()+geom_text(hjust=0.27,vjust=1.5,check_overlap = TRUE,size=3.5)+
  geom_pointrange(xmin=plot_gM$g_ci95_low,xmax=plot_gM$g_ci95_high)+
  scale_x_continuous(limits=c(0,0.15),breaks=c(0,0.05,0.1,0.15,0.2),labels = function(x) sprintf("%0.2f", x))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),
        axis.text.x=element_text(size=10),axis.title.x=element_blank(),legend.title = element_blank(),legend.position = "bottom")+
  annotate("text", y= 6, x =0.15*0.015,label=bquote(italic("(b)")~CH[4]),size=4)+
  scale_shape_manual(values = c('Models'=17, 'Modern'=3, 'Little Ice Age'=15, 'D-O events'=16))

pc_CH4<-ggplot(plot_gM, aes(y=sequence, x=c,shape=source,label=paper)) +theme_bw()+
  geom_rect(aes(xmin=plot_gM[source=="D-O events","c_ci95_low"], 
                xmax=plot_gM[source=="D-O events","c_ci95_high"], 
                ymin=-Inf, ymax=Inf),alpha=0.2,fill="gray80")+
  geom_point()+geom_text(hjust=0.27,vjust=1.5,check_overlap = TRUE,size=3.5)+
  geom_pointrange(xmin=plot_gM$c_ci95_low,xmax=plot_gM$c_ci95_high)+
  scale_x_continuous(limits=c(0,0.2),breaks=c(0,0.05,0.1,0.15,0.2),labels = function(x) sprintf("%0.2f", x))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),
        axis.text.x=element_text(size=10),axis.title.x=element_blank(),legend.title = element_blank(),legend.position = "bottom")+
  annotate("text", y= 6, x =0.2*0.015,label=bquote(italic("(b)")~CH[4]),size=4)+
  scale_shape_manual(values = c('Models'=17, 'Modern'=3, 'Little Ice Age'=15, 'D-O events'=16))


#N2O
plot_gN<-plot_gain[which(plot_gain$variable=="N2O"),]
rownames(plot_gN)<-seq(1:nrow(plot_gN))
plot_gN$sequence<-c(1,2,3,4,5,6)
plot_gN$sequence<-as.factor(plot_gN$sequence)

pg_N2O<-ggplot(plot_gN, aes(y=sequence, x=g,shape=source,label=paper)) +theme_bw()+
  geom_rect(aes(xmin=plot_gN[source=="D-O events","g_ci95_low"], 
                xmax=plot_gN[source=="D-O events","g_ci95_high"], 
                ymin=-Inf, ymax=Inf),alpha=0.2,fill="gray80")+
  geom_point()+geom_text(hjust=0.27,vjust=1.5,check_overlap = TRUE,size=3.5)+
  geom_pointrange(xmin=plot_gN$g_ci95_low,xmax=plot_gN$g_ci95_high)+
  labs(x = "Gain")+
  scale_x_continuous(limits=c(0,0.15),breaks=c(0,0.05,0.1,0.15,0.2),labels = function(x) sprintf("%0.2f", x))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),
        axis.text.x=element_text(size=10),axis.title.x=element_text(size=10),legend.position = "none")+
  annotate("text", y= 6, x =0.15*0.015,label=bquote(italic("(c)")~N[2]*O),size=4)+
  scale_shape_manual(values = c('Models'=17, 'Little Ice Age'=15, 'D-O events'=16))

pc_N2O<-ggplot(plot_gN, aes(y=sequence, x=c,shape=source,label=paper)) +theme_bw()+
  geom_rect(aes(xmin=plot_gN[source=="D-O events","c_ci95_low"], 
                xmax=plot_gN[source=="D-O events","c_ci95_high"], 
                ymin=-Inf, ymax=Inf),alpha=0.2,fill="gray80")+
  geom_point()+geom_text(hjust=0.27,vjust=1.5,check_overlap = TRUE,size=3.5)+
  geom_pointrange(xmin=plot_gN$c_ci95_low,xmax=plot_gN$c_ci95_high)+
  labs(x = bquote('Feedback strength ('~ W ~ m ^-2~K^-1~')'))+
  scale_x_continuous(limits=c(0,0.2),breaks=c(0,0.05,0.1,0.15,0.2),labels = function(x) sprintf("%0.2f", x))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),
        axis.text.x=element_text(size=10),axis.title.x=element_text(size=10),legend.position = "none")+
  annotate("text", y= 6, x =0.2*0.015,label=bquote(italic("(c)")~N[2]*O),size=4)+
  scale_shape_manual(values = c('Models'=17, 'Little Ice Age'=15, 'D-O events'=16))

legend<-get_legend(pg_CH4)
pg_CH4<-pg_CH4+ theme(legend.position="none")
pg<-grid.arrange(pg_CO2,pg_CH4,pg_N2O,legend,ncol=1,layout_matrix=rbind(rbind(1,1,1,1,1,1,1,1),rbind(2,2,2,2,2),rbind(3,3,3,3,3),4))
ggsave(file="Gain comparison.jpeg",pg,width=7,height=9)

legend<-get_legend(pc_CH4)
pc_CH4<-pc_CH4+ theme(legend.position="none")
pc<-grid.arrange(pc_CO2,pc_CH4,pc_N2O,legend,ncol=1,layout_matrix=rbind(rbind(1,1,1,1,1,1,1,1),rbind(2,2,2,2,2),rbind(3,3,3,3,3),4))
ggsave(file="Feedback comparison.jpeg",pc,width=8,height=9)


