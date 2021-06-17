library(survival)
library(survminer)
library(lubridate)

anal_data$S_fum<-(anal_data$Outcomes_last_FU_date-anal_data$Info_수술일)/dmonths(1)
anal_data$S_TS<-Surv(anal_data$S_fum,anal_data$O_Survival_Death)
S_fit<-survfit(S_TS~Mitral_Repair,data=anal_data)
fig1<-ggsurvplot(S_fit,data=anal_data,risk.table=T,pval=T,break.time.by = 60,xlim = c(0, 180),legend.title="",surv.scale = "percent",legend.labs=c('Replacement','Repair'),xlab='Months after operation')
fig1$plot <- fig1$plot + theme(
  axis.title.x = element_text(face='bold'),
  axis.title.y = element_text(face='bold')
)

anal_data$C_date<-pmin(anal_data$Outcomes_last_FU_date,anal_data$O_Survival_Death_date,anal_data$O_Recurrence_Recur_Dx._date,anal_data$O_Valve_ReOp_date,na.rm=T)
anal_data$C_fum<-(anal_data$C_date-anal_data$Info_수술일)/dmonths(1)
anal_data$C_TS<-Surv(anal_data$C_fum,anal_data$O_Composite)
C_fit<-survfit(C_TS~Mitral_Repair,data=anal_data)

fig2<-ggsurvplot(C_fit,data=anal_data,risk.table=T,pval=T,break.time.by = 60,xlim = c(0, 180),legend.title="",surv.scale = "percent",legend.labs=c('Replacement','Repair'),xlab='Months after operation',ylab='Composite endpoint')
fig2$plot <- fig2$plot + theme(
  axis.title.x = element_text(face='bold'),
  axis.title.y = element_text(face='bold')
)


