library(MatchIt)
library(cobalt)
library(kableExtra)
library(stringr)

#anal_data$CPB_ACC_time



mat_cov<-c('Info_Age','Baseline_Ht')
#select(Mitral_Repair,Info_Age,Baseline_Ht,Baseline_Wt,Baseline_BMI,Symptom_dyspnea,Symptom_fever,S_fum,O_Survival_Death,C_fum,O_Composite,S_TS,C_TS,CPB_ACC_time,CPB_CPB_time)%>%
pre_match<-anal_data%>%
  drop_na(any_of(mat_cov))


matched<-pre_match%>%
  matchit(as.formula(paste0('Mitral_Repair~',paste0(mat_cov,collapse='+'))),data=.,method="optimal",ratio=1)


  #matchit(Mitral_Repair~Info_Age+Baseline_Ht+Baseline_Wt+Baseline_BMI+Symptom_dyspnea+Symptom_fever,data=.,method="optimal",ratio=1)
  #matchit(Mitral_Repair~Info_Age,data=.,method="optimal",ratio=1)
  #matchit(Mitral_Repair~Info_Age+Baseline_Ht+Baseline_Wt+Baseline_BMI+Symptom_dyspnea+Symptom_fever+CPB_CPB_time+CPB_ACC_time,data=.,method="nearest",caliper=0.05,ratio=1)
  
  #matchit(Mitral_Repair~Info_Age+Baseline_Ht+Baseline_Wt+Baseline_BMI+Symptom_dyspnea+Symptom_fever+CPB_CPB_time+CPB_ACC_time,data=.,method="optimal",ratio=1)
res<-summary(matched)
summary(matched)
print(res$nn%>%kable())
print(cbind(res$sum.all[,1:3],res$sum.matched[,1:3])%>%kable()%>%add_header_above(c(" " = 1, "Raw data" = 3, "Matched data" = 3)))
print(plot(matched))
print(plot(matched,type='hist'))
print(bal.plot(matched, var.name = "distance", which = "both",type = "histogram", mirror = T))
mat_data<-match.data(matched)

mat_data%>%View()

