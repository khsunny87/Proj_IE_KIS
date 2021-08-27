library(tidyr)
library(forcats)
library(survival)
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
  
  #IE_culture 생성
  mutate(IE_culture=(!is.na(`IE_균주`))&(`IE_균주`!='(NG)'))%>%
  mutate(IE_embolism=num_logical(IE_embolism))%>%
  
  

  #PMH
  mutate_at(vars(starts_with('PMH_')),num_logical)%>%
    select(-PMH_dyslipidemia,-PMH_prev_MI,-PMH_COPD,-`PMH_비고`)%>%

  mutate_at(vars(starts_with('Op_Ix_')),num_logical)%>%
  
  mutate_at(vars(starts_with('Mitral_')),num_logical)%>%
    mutate(Mitral_Repair=(!Mitral_MVR))%>%
    select(-Mitral_MVR)%>%
  

  mutate(Aortic_IE_lesion=num_logical(Aortic_IE_lesion))%>%
  mutate(Aortic_Prev_AV_Op=num_logical(Aortic_Prev_AV_Op))%>%
  mutate(Aortic_Concomittant_AV_Op=num_logical(Aortic_Concomittant_AV_Op))%>%
  
  mutate(Tricuspid_IE_lesion=num_logical(Tricuspid_IE_lesion))%>%
  mutate(Tricuspid_Prev_TV_Op=num_logical(Tricuspid_Prev_TV_Op))%>%
  mutate(Tricuspid_Concomittant_TV_Op=num_logical(Tricuspid_Concomittant_TV_Op))%>%
  
  
  mutate_at(vars(starts_with('Concomitant_')),num_logical)%>%

  replace_na(list(Mitral_MR = 'no',Mitral_MS = 'no',Aortic_AR = 'no',Aortic_AS = 'no',Tricuspid_TR = 'no',Tricuspid_TS = 'no'))%>%
    mutate(Mitral_MR=factor(Mitral_MR,order=T,levels=grade))%>%
      mutate(MR_gr_mod=(Mitral_MR>='moderate'))%>%
    mutate(Mitral_MS=factor(Mitral_MS,order=T,levels=grade))%>%
  
    mutate(Aortic_AR=factor(Aortic_AR,order=T,levels=grade))%>%
      mutate(AR_gr_mod=(Aortic_AR>='moderate'))%>%
    mutate(Aortic_AS=factor(Aortic_AS,order=T,levels=grade))%>%
  
    mutate(Tricuspid_TR=factor(Tricuspid_TR,order=T,levels=grade))%>%
      mutate(TR_gr_mod=(Tricuspid_TR>='moderate'))%>%
    mutate(Tricuspid_TS=factor(Tricuspid_TS,order=T,levels=grade))%>%
  
  mutate(Duration_Onset2Op=(`Info_수술일`-`Acute_Sx._onset`)/ddays(1),Duration_Anti2Op=(`Info_수술일`-`Anti_투여날짜`)/ddays(1))%>%
  
  #Survival analysis 관련 변수
  mutate(S_fum=(Outcomes_last_FU_date-Info_수술일)/dmonths(1))%>%
    mutate(S_TS=Surv(S_fum,O_Survival_Death))%>%
  #mutate(O_Composite=(O_Survival_Death|O_Recurrence_Recur|O_Valve_ReOp))%>%
  #  mutate(C_date=pmin(Outcomes_last_FU_date,O_Survival_Death_date,O_Recurrence_Recur_Dx._date,O_Valve_ReOp_date,na.rm=T))%>%
  mutate(O_Composite=(O_Survival_Death|O_Recurrence_Recur|O_Valve_ReOp|O_Valve_B_major|O_Valve_E_major))%>%
    mutate(C_date=pmin(Outcomes_last_FU_date,O_Survival_Death_date,O_Recurrence_Recur_Dx._date,O_Valve_ReOp_date,O_Valve_B_mj_date,O_Valve_E_mj_date,na.rm=T))%>%
  #mutate(O_Composite=(O_Survival_Death|O_Recurrence_Recur|O_Valve_ReOp|O_Valve_B_major|O_Valve_E_major|O_Valve_B_minor|O_Valve_E_minor))%>%
  #  mutate(C_date=pmin(Outcomes_last_FU_date,O_Survival_Death_date,O_Recurrence_Recur_Dx._date,O_Valve_ReOp_date,O_Valve_B_mj_date,O_Valve_E_mj_date,O_Valve_B_mn_date,O_Valve_E_mn_date,na.rm=T))%>%
  
    mutate(C_fum=(C_date-Info_수술일)/dmonths(1))%>%
    mutate(C_TS=Surv(C_fum,O_Composite))





  