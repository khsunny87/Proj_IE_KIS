library(tidyr)
library(forcats)
library(lubridate)

num_logical<-function(vec_x){
  
  if(any(!(vec_x %in% c(0,1,NA)))) return(vec_x)
  if(!is.numeric(vec_x)) vec_x<-as.numeric(vec_x)
  
  return(as.logical(vec_x))
  
}


grade<-c('no','minimal','minimal-to-mild','mild','mild-to-moderate','moderate','moderate-to-severe','severe')

inc_data<-raw_data%>%
  filter(Info_Inclusion_list=='TRUE')%>%
  
  mutate(Info_Male=(`Info_성별`=='M'))%>%
  select(-Info_성별)%>%
  rename(Info_Age=`Info_나이(yr)`)%>%
  
  mutate_at(vars(starts_with('Symptom_')),num_logical)%>%
    mutate(Symptom_dyspnea=(Symptom_dyspnea|Symptom_orthopnea))%>%
    select(-Symptom_orthopnea)%>%
    mutate(Symptom_myalgia=(Symptom_myalgia|Symptom_abdominal_pain))%>%
    select(-Symptom_abdominal_pain)%>%
  
  mutate_at(vars(starts_with('PMH_')),num_logical)%>%
    select(-PMH_dyslipidemia,-PMH_prev_MI,-PMH_COPD,-`PMH_비고`)%>%
  
  mutate_at(vars(starts_with('Mitral_')),num_logical)%>%
    mutate(Mitral_Repair=(!Mitral_MVR))%>%
    select(-Mitral_MVR)%>%
  
  mutate_at(vars(starts_with('Concomitant_')),num_logical)%>%

  replace_na(list(Mitral_MR = 'no',Mitral_MS = 'no',Aortic_AR = 'no',Aortic_AS = 'no',Tricuspid_TR = 'no',Tricuspid_TS = 'no'))%>%
    mutate(Mitral_MR=factor(Mitral_MR,order=T,levels=grade))%>%
      mutate(Mitral_gr_mod=(Mitral_MR>='moderate'))%>%
    mutate(Mitral_MS=factor(Mitral_MS,order=T,levels=grade))%>%
    mutate(Aortic_AR=factor(Aortic_AR,order=T,levels=grade))%>%
    mutate(Aortic_AS=factor(Aortic_AS,order=T,levels=grade))%>%
    mutate(Tricuspid_TR=factor(Tricuspid_TR,order=T,levels=grade))%>%
    mutate(Tricuspid_TS=factor(Tricuspid_TS,order=T,levels=grade))%>%
  mutate(Duration_Onset2Op=(`Info_수술일`-`Acute_Sx._onset`)/ddays(1),Duration_Anti2Op=(`Info_수술일`-`Anti_투여날짜`)/ddays(1))%>%
  mutate(O_Composite=(O_Survival_Death|O_Recurrence_Recur|O_Valve_ReOp))#%>%
  #filter(Duration_Onset2Op<=14)
  



  