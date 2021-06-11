library(tidyr)

num_logical<-function(vec_x){
  
  if(any(!(vec_x %in% c(0,1,NA)))) return(vec_x)
  if(!is.numeric(vec_x)) vec_x<-as.numeric(vec_x)
  
  return(as.logical(vec_x))
  
}


grade<-c('no','minimal','minimal-to-mild','mild','mild-to-moderate','moderate','moderate-to-severe','severe')

inc_data<-raw_data%>%
  filter(Info_Inclusion_list=='TRUE')%>%
  mutate_at(vars(starts_with('Symptom_')),num_logical)%>%
  mutate_at(vars(starts_with('PMH_')),num_logical)%>%
  mutate_at(vars(starts_with('Mitral_')),num_logical)%>%
  mutate_at(vars(starts_with('Concomitant_')),num_logical)%>%
  mutate(Info_Sex=factor(`Info_성별`,levels=c('M','F')))%>%
  select(-Info_성별)%>%
  rename(Info_Age=`Info_나이(yr)`)%>%
  replace_na(list(Mitral_MR = 'no',Mitral_MS = 'no',Aortic_AR = 'no',Aortic_AS = 'no',Tricuspid_TR = 'no',Tricuspid_TS = 'no'))%>%
    mutate(Mitral_MR=factor(Mitral_MR,levels=grade))%>%
    mutate(Mitral_MS=factor(Mitral_MS,levels=grade))%>%
    mutate(Aortic_AR=factor(Aortic_AR,levels=grade))%>%
    mutate(Aortic_AS=factor(Aortic_AS,levels=grade))%>%
    mutate(Tricuspid_TR=factor(Tricuspid_TR,levels=grade))%>%
    mutate(Tricuspid_TS=factor(Tricuspid_TS,levels=grade))
  

  




