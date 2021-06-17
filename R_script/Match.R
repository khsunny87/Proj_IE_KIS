library(MatchIt)
library(cobalt)


#anal_data$S_TS<-Surv(anal_data$S_fum,anal_data$O_Survival_Death)

pre_match<-anal_data%>%
  #mutate(S_fum=(Outcomes_last_FU_date-Info_수술일)/dmonths(1))%>%
  select(Mitral_Repair,Info_Age,Baseline_Ht,Baseline_Wt,Baseline_BMI,Symptom_dyspnea,Symptom_fever,S_fum,O_Survival_Death)%>%
  drop_na()

matched<-pre_match%>%
  matchit(Mitral_Repair~.,data=.,method="optimal",ratio=1)
summary(matched)
plot(matched)
plot(matched,type='hist')
bal.plot(matched, var.name = "distance", which = "both",type = "histogram", mirror = T)


mat_data<-match.data(matched)
plot(mat_data)

mat_data$S_TS<-Surv(mat_data$S_fum,mat_data$O_Survival_Death)
M_S_fit<-survfit(S_TS~Mitral_Repair,data=mat_data)
fig3<-ggsurvplot(M_S_fit,data=mat_data,risk.table=T,pval=T,break.time.by = 60,xlim = c(0, 180),legend.title="",surv.scale = "percent",xlab='Months after operation',legend.labs=c('Replacement','Repair'))
fig3$plot <- fig3$plot + theme(
  axis.title.x = element_text(face='bold'),
  axis.title.y = element_text(face='bold')
)
fig3
