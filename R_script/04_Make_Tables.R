library(moonBook)
library(ztable)
library(forcats)

myt_output<-'viewer' #default

print_mytable<-function(myt){
  
  if(myt_output=='viewer'){
    options(ztable.type='viewer')
    myt%>%ztable()%>%print()
    
  } else if(myt_output=='HTML'){
    options(ztable.type='HTML')
    myt%>%ztable()%>%print()
  } else if(myt_output=='PPT'){
    myt%>%mytable2df()%>%knitr::kable(format='pipe',row.names=F)%>%print()
  }
  
}


table1<-anal_data%>%
  select(Mitral_Repair,Info_Male,Info_Age,starts_with('Baseline_'),starts_with('Symptom_'),starts_with('PMH_'),starts_with('Lab_'),starts_with('Duration_'),starts_with('CPB_'),Mitral_annular_recon,starts_with('Concomitant_'),Mitral_MS,Mitral_MR,Mitral_gr_mod)%>%
  select(-CPB_TCA_time)%>%
  mutate(Mitral_Repair=fct_relevel(if_else(Mitral_Repair==1,'Repair','Replacement'),'Repair'))%>%
  mytable(Mitral_Repair~.,data=.,show.total=T,catMethod=0)%>%
  compress(add.label=F)

  


#anal_data%>%
#  mutate(Mitral_MVR=fct_relevel(if_else(Mitral_MVR,'Replacement','Repair'),'Repair'))%>%
#  select(Mitral_MVR,Info_Sex,Info_Age,starts_with('Baseline_'),starts_with('Symptom_'),starts_with('PMH_'),starts_with('Lab_'),starts_with('Duration_'),starts_with('CPB_'),starts_with('Concomitant_'),Mitral_MS,Mitral_MR)%>%
#  select(-CPB_TCA_time)%>%names()%>%data.frame()%>%
#  write_excel_csv('Input/label.csv')
  

