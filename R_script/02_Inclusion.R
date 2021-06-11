num_logical<-function(vec_x){
  
  if(any(!(vec_x %in% c(0,1,NA)))) return(vec_x)
  if(!is.numeric(vec_x)) vec_x<-as.numeric(vec_x)
  
  return(as.logical(vec_x))
  
}



inc_data<-raw_data%>%
  filter(Info_Inclusion_list=='TRUE')%>%
  mutate_at(vars(starts_with('Symptom_')),num_logical)%>%
  mutate_at(vars(starts_with('PMH_')),num_logical)%>%
  mutate_at(vars(starts_with('Mitral_')),num_logical)%>%
  mutate_at(vars(starts_with('Concomitant_')),num_logical)%>%
  mutate(Info_Sex=factor(`Info_성별`,levels=c('M','F')))%>%
  select(-Info_성별)%>%
  rename(Info_Age=`Info_나이(yr)`)

  




