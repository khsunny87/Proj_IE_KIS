library(MatchIt)
library(cobalt)
library(kableExtra)

#anal_data$CPB_ACC_time
pre_match<-anal_data%>%
  #select(Mitral_Repair,Info_Age,Baseline_Ht,Baseline_Wt,Baseline_BMI,Symptom_dyspnea,Symptom_fever,S_fum,O_Survival_Death,C_fum,O_Composite,S_TS,C_TS,CPB_ACC_time,CPB_CPB_time)%>%
  select(Mitral_Repair,Info_Age,Baseline_Ht,Baseline_Wt,Baseline_BMI,Symptom_dyspnea,Symptom_fever,S_fum,O_Survival_Death,C_fum,O_Composite,S_TS,C_TS)%>%
  drop_na()

matched<-pre_match%>%
  matchit(Mitral_Repair~Info_Age+Baseline_Ht+Baseline_Wt+Baseline_BMI+Symptom_dyspnea+Symptom_fever,data=.,method="optimal",ratio=1)
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


?add_header_above
