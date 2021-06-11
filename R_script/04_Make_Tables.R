library(moonBook)
library(forcats)

anal_data%>%
  mutate(Mitral_MVR=fct_relevel(if_else(Mitral_MVR,'Replacement','Repair'),'Repair'))%>%
  select(Mitral_MVR,Info_Sex,Info_Age,starts_with('Baseline_'),starts_with('Symptom_'),starts_with('PMH_'),starts_with('Lab_'),starts_with('CPB_'),starts_with('Concomitant_'),Mitral_MS,Mitral_MR)%>%
  select(-CPB_TCA_time)%>%
  mytable(Mitral_MVR~.,data=.,show.total=T,catMethod=0)%>%
  compress(add.label=F)

  

inc_data%>%
  mytable(Mitral_MVR~PMH_HTN,data=.,show.total=T,catMethod=0)%>%
  compress(add.label=F)
