library(survival)
library(survminer)
library(lubridate)

#surv_data<-anal_data


Get_median_fu<-function(TS){
  ret<-survfit(Surv(TS[,1],1-TS[,2])~1)
  return(summary(ret)$table['median']%>%round(2))
}

Survival_Table<-function(TS,strata,time_point,unit){

survfit(TS~1)%>%
  summary(times=time_point)%>%.[c('time','surv','std.err')]%>%as.data.frame()%>%
  mutate(strata='Overall')%>%
  mutate(value=paste(format(round(surv*100,1),nsmall=1),'±',format(round(std.err*100,1),nsmall=1),'%'))%>%
  select(time,strata,value)%>%
  rbind(.,survfit(TS~strata)%>%
          summary(times=time_point)%>%.[c('strata','time','surv','std.err')]%>%as.data.frame()%>%
          mutate(value=paste(format(round(surv*100,1),nsmall=1),'±',format(round(std.err*100,1),nsmall=1),'%'))%>%
          select(time,strata,value))%>%
  pivot_wider(names_from='time',names_prefix=paste0(unit,' '),values_from='value')

}


S_fit<-survfit(S_TS~Mitral_Repair,data=surv_data)
surv_tbl1<-Survival_Table(surv_data$S_TS,surv_data$Mitral_Repair,c(60,120,180),'Month')

surv_fig1<-ggsurvplot(S_fit,data=surv_data,risk.table=T,pval=T,break.time.by = 60,xlim = c(0, 180),legend.title="",surv.scale = "percent",legend.labs=c('Replacement','Repair'),xlab='Months after operation')
surv_fig1$plot <- surv_fig1$plot + theme(
  axis.title.x = element_text(face='bold'),
  axis.title.y = element_text(face='bold')
)


  
  
C_fit<-survfit(C_TS~Mitral_Repair,data=surv_data)
surv_tbl2<-Survival_Table(surv_data$C_TS,surv_data$Mitral_Repair,c(60,120,180),'Month')

surv_fig2<-ggsurvplot(C_fit,data=surv_data,risk.table=T,pval=T,break.time.by = 60,xlim = c(0, 180),legend.title="",surv.scale = "percent",legend.labs=c('Replacement','Repair'),xlab='Months after operation',ylab='Composite endpoint')
surv_fig2$plot <- surv_fig2$plot + theme(
  axis.title.x = element_text(face='bold'),
  axis.title.y = element_text(face='bold')
)


