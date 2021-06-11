library(tidyr)
library(sjlabelled)
library(readr)

raw_preop_lab<-read_csv('Input/IE_KIS_preop_lab.csv')


anal_data<-raw_preop_lab%>%
  rename_at(c(-1),function(x) paste0('Lab_',x))%>%
  left_join(x=inc_data,y=.,by=c('Info_Case_No.'='Case No.'))


View(anal_data)




label_df<-raw_lab%>%
  group_by(`검사코드`,`검사명`)%>%
  summarise()%>%
  filter(!(`검사코드` %in% c('BL312002','BL312011')))

labels<-names(inc_preop_lab)
names(labels)<-labels
labels[label_df$`검사코드`]<-label_df$`검사명`
inc_preop_lab<-set_label(inc_preop_lab,labels)

inc_preop_lab<-inc_preop_lab%>%
  select(c(1:14)) #뒤에 제거



  