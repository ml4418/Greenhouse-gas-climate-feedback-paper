#Define a function to convert GICC05_BP2000 to AICC2012_BP1950
convert<-function(old,transfer){
  old<-as.matrix(old)
  new<-matrix(nrow=nrow(old),ncol=1)
  for(i in 1:nrow(old)){
    if(old[i]<60000){
      new[i]<-old[i]
    }else{
      age_to_adjust<-old[i]
      row_closest<-which.min(abs(transfer$GICC05AgeBP1950 - age_to_adjust))
      age_closest<-as.numeric(transfer[row_closest,"GICC05AgeBP1950"])
      if(age_to_adjust >= age_closest){
        y1<-as.numeric(transfer[row_closest,"AICC2012AgeBP1950"])
        y2<-as.numeric(transfer[row_closest+1,"AICC2012AgeBP1950"])
        x1<-as.numeric(transfer[row_closest,"GICC05AgeBP1950"])
        x2<-as.numeric(transfer[row_closest+1,"GICC05AgeBP1950"])
        age_adjusted<-y1+(y2-y1)*(age_to_adjust-x1)/(x2-x1)
      }else if(age_to_adjust < age_closest){
        y1<-as.numeric(transfer[row_closest-1,"AICC2012AgeBP1950"])
        y2<-as.numeric(transfer[row_closest,"AICC2012AgeBP1950"])
        x1<-as.numeric(transfer[row_closest-1,"GICC05AgeBP1950"])
        x2<-as.numeric(transfer[row_closest,"GICC05AgeBP1950"])
        age_adjusted<-y1+(y2-y1)*(age_to_adjust-x1)/(x2-x1)
      }
      new[i]<-age_adjusted
      
    }
    
  }
  return(new)
}
#Define a function to convert depth to AICC2012_BP1950
convert_depth_age<-function(depth,transfer){
  depth<-as.matrix(depth)
  age<-matrix(nrow=nrow(depth),ncol=1)
  for(i in 1:nrow(depth)){
      row_closest<-which.min(abs(transfer$depth_m - depth[i]))
      depth_closest<-as.numeric(transfer[row_closest,"depth_m"])
      if(depth[i] >= depth_closest){
        y1<-as.numeric(transfer[row_closest,"ice_age_a_1950"])
        y2<-as.numeric(transfer[row_closest+1,"ice_age_a_1950"])
        x1<-as.numeric(transfer[row_closest,"depth_m"])
        x2<-as.numeric(transfer[row_closest+1,"depth_m"])
        age_adjusted<-y1+(y2-y1)*(depth[i]-x1)/(x2-x1)
      }else if(depth[i] < depth_closest){
        y1<-as.numeric(transfer[row_closest-1,"ice_age_a_1950"])
        y2<-as.numeric(transfer[row_closest,"ice_age_a_1950"])
        x1<-as.numeric(transfer[row_closest-1,"depth_m"])
        x2<-as.numeric(transfer[row_closest,"depth_m"])
        age_adjusted<-y1+(y2-y1)*(depth[i]-x1)/(x2-x1)
      }
      age[i]<-age_adjusted

  }
  return(age)
}
#Get common legend
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

#Define a function to identify minimum and maximum
get_event<-function(data,DO,DOv,k,win,min_left,min_right,duration,bin,ylab){
  
  colnames(data)<-c("Age","Value","Err")
  tnow<-DOv[k]
  
  event<-data[which(data$Age>=(tnow-win)&data$Age<=(tnow+win)),]
  
  ageb<-seq((tnow-win)+bin/2,(tnow+win)-bin/2,by=bin)
  valueb<-tapply(event$Value,cut(event$Age,seq((tnow-win),(tnow+win),by=bin)),mean)
  errb<-tapply(event$Err,cut(event$Age,seq((tnow-win),(tnow+win),by=bin)),function(x){sqrt(sum(x^2))/length(x)})
  
  eventb<-cbind.data.frame(ageb,valueb,errb);colnames(eventb)<-c("Age","Value","Err");eventb<-na.omit(eventb)
  
  eventbmin<-eventb[which(eventb$Age>=(tnow-min_left)&eventb$Age<=(tnow+min_right)),]
  min<-min(eventbmin[,"Value"])
  err_min<-eventbmin[which.min(eventbmin[,"Value"]),"Err"]
  tmin<-eventbmin[which.min(eventbmin[,"Value"]),"Age"]
  
  eventbmax<-eventb[which(eventb$Age>=(tmin-duration)&eventb$Age<=tmin),]
  max<-max(eventbmax[,"Value"])
  err_max<-eventbmax[which.max(eventbmax[,"Value"]),"Err"]
  tmax<-eventbmax[which.max(eventbmax[,"Value"]),"Age"]
  
  if(DOv[k]==DO[k]){
    p<-ggplot(data=eventb,aes(Age,Value))+
      geom_rect(aes(xmin=tmax, xmax=tmin, ymin=min, ymax=max),alpha=0.2,fill="gray80")+
      geom_point(size=1)+geom_line()+theme_bw()+
      xlim((tnow-win),(tnow+win))+
      geom_vline(xintercept = DO[k])+geom_vline(xintercept = DO[k-1])+geom_vline(xintercept = DO[k+1])+
      geom_hline(yintercept = max,linetype="dashed")+geom_hline(yintercept = min,linetype="dashed")+
      geom_text(aes(x=tnow+100,y=max(Value),label=k))+
      geom_text(aes(x=DO[k-1]+100,y=max(Value),label=k-1))+
      geom_text(aes(x=DO[k+1]+100,y=max(Value),label=k+1))+
      geom_text(aes(x=(tmax+tmin)/2,y=(max+min)/2,label=paste(tmin-tmax,"years")))+
      labs(x="Age (yr BP)",y=ylab)+
      geom_segment(x=tmin,xend=tmin, y=min-err_min,yend=min+err_min,color="red3",size=0.8)+
      geom_segment(x=tmax,xend=tmax, y=max-err_max,yend=max+err_max,color="red3",size=0.8)
  }else{
    if(DOv[k]>DO[k]){
      shiftlabel=paste(DOv[k]-DO[k],"years before")
    }else{
      shiftlabel=paste(DO[k]-DOv[k],"years after")
    }
    
    p<-ggplot(data=eventb,aes(Age,Value))+
      geom_rect(aes(xmin=tmax, xmax=tmin, ymin=min, ymax=max),alpha=0.2,fill="gray80")+
      geom_point(size=1)+geom_line()+theme_bw()+
      xlim((DO[k]-win),(DO[k]+win))+
      geom_vline(xintercept=tnow,color="red",linetype="dashed",size=0.5)+
      #geom_text(aes(x=tnow,y=min(Value),label=shiftlabel))+
      geom_vline(xintercept = DO[k])+geom_vline(xintercept = DO[k-1])+geom_vline(xintercept = DO[k+1])+
      geom_hline(yintercept = max,linetype="dashed")+geom_hline(yintercept = min,linetype="dashed")+
      geom_text(aes(x=tnow+100,y=max(Value),label=k))+
      geom_text(aes(x=DO[k-1]+100,y=max(Value),label=k-1))+
      geom_text(aes(x=DO[k+1]+100,y=max(Value),label=k+1))+
      geom_text(aes(x=(tmax+tmin)/2,y=(max+min)/2,label=paste(tmin-tmax,"years")))+
      labs(x="Age (yr BP)",y=ylab)+
      geom_segment(x=tmin,xend=tmin, y=min-err_min,yend=min+err_min,color="red3",size=0.8)+
      geom_segment(x=tmax,xend=tmax, y=max-err_max,yend=max+err_max,color="red3",size=0.8)
  }
  
  outputdata<-cbind.data.frame(min,max,err_min,err_max,tmin,tmax)
  outputlist<-list(outputdata,p)
  return(outputlist)
  
}
#color break function
colbreak<-function(data,colbreaks){
  output<-data.frame()
  for(i in 1:length(data)){
    value<-data[i]
    for(j in 1:(length(colbreaks)-1)){
      if(value>=colbreaks[j]&value<colbreaks[j+1]){
        valuefactor<-paste(colbreaks[j],"~",colbreaks[j+1])
        break
      }else{
        valuefactor<-NA
      }
    }
    output[i,"valuefactor"]<-valuefactor
  }
  return(as.vector(as.matrix(output)))
}
