library(survival)
library(survminer)
library(lubridate)

#surv_data<-anal_data


S_fit<-survfit(S_TS~Mitral_Repair,data=surv_data)
surv_fig1<-ggsurvplot(S_fit,data=surv_data,risk.table=T,pval=T,break.time.by = 60,xlim = c(0, 180),legend.title="",surv.scale = "percent",legend.labs=c('Replacement','Repair'),xlab='Months after operation')
surv_fig1$plot <- surv_fig1$plot + theme(
  axis.title.x = element_text(face='bold'),
  axis.title.y = element_text(face='bold')
)
C_fit<-survfit(C_TS~Mitral_Repair,data=surv_data)

surv_fig2<-ggsurvplot(C_fit,data=surv_data,risk.table=T,pval=T,break.time.by = 60,xlim = c(0, 180),legend.title="",surv.scale = "percent",legend.labs=c('Replacement','Repair'),xlab='Months after operation',ylab='Composite endpoint')
surv_fig2$plot <- surv_fig2$plot + theme(
  axis.title.x = element_text(face='bold'),
  axis.title.y = element_text(face='bold')
)


