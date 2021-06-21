library(MatchIt)
library(cobalt)
library(kableExtra)
library(stringr)




mat_cov<-c('Info_Age','Baseline_Ht','Baseline_Wt','Baseline_BMI',
           'Symptom_dyspnea','Symptom_fever','Symptom_chills','Symptom_myalgia','Symptom_fatigue','Symptom_GW',
           'PMH_HTN','PMH_DM','PMH_CAD','PMH_CVA','PMH_CKD','PMH_dialysis','PMH_Smoking','PMH_Malignancy','PMH_Obesity',
           'Mitral_annular_recon',
           'Duration_Anti2Op')


pre_match<-anal_data%>%
  drop_na(any_of(mat_cov))


matched<-pre_match%>%
  #matchit(as.formula(paste0('Mitral_Repair~',paste0(mat_cov,collapse='+'))),data=.,method="optimal",ratio=1)
  matchit(as.formula(paste0('Mitral_Repair~',paste0(mat_cov,collapse='+'))),data=.,method="nearest",caliper=0.2,ratio=1)
  #matchit(Mitral_Repair~Info_Age+Baseline_Ht+Baseline_Wt+Baseline_BMI+Symptom_dyspnea+Symptom_fever+CPB_CPB_time+CPB_ACC_time,data=.,method="nearest",caliper=0.05,ratio=1)
  
  #matchit(Mitral_Repair~Info_Age+Baseline_Ht+Baseline_Wt+Baseline_BMI+Symptom_dyspnea+Symptom_fever+CPB_CPB_time+CPB_ACC_time,data=.,method="optimal",ratio=1)
res<-summary(matched)
print(res$nn%>%kable()%>% kable_styling())
print(cbind(res$sum.all[,1:3],res$sum.matched[,1:3])%>%kable()%>%add_header_above(c(" " = 1, "Raw data" = 3, "Matched data" = 3)))
print(plot(matched))
print(plot(matched,type='hist'))
print(bal.plot(matched, var.name = "distance", which = "both",type = "histogram", mirror = T))
mat_data<-match.data(matched)


