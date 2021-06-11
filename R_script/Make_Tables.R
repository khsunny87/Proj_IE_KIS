library(moonBook)
library(forcats)

anal_data%>%
  mutate(Mitral_MVR=fct_relevel(if_else(Mitral_MVR,'Replacement','Repair'),'Repair'))%>%
  select(Mitral_MVR,Info_성별,`Info_나이(yr)`,starts_with('Lab'))%>%
    rename(Info_Age=`Info_나이(yr)`)%>%
  mytable(Mitral_MVR~.,data=.,show.total=T,catMethod=0)%>%
  compress(add.label=F)

  

inc_data%>%
  mytable(Mitral_MVR~PMH_HTN,data=.,show.total=T,catMethod=0)%>%
  compress(add.label=F)
