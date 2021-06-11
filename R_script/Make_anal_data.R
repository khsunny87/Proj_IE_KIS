library(tidyr)
library(sjlabelled)


inc_preop_lab<-raw_lab%>%
  #select(`Case No.`,수술시작시간,검사코드,접수일시)%>%
  filter(`접수일시`<`수술시작시간`)%>%
  group_by(`Case No.`,`수술시작시간`,`검사코드`)%>%
  arrange(desc(`접수일시`))%>%
  slice(1)%>%
  select(`검사결과수치값`)%>%
  ungroup(`검사코드`)%>%
  #group_by(`접수일시`)%>%
  pivot_wider(names_from=검사코드,values_from=검사결과수치값)%>%
  filter(`Case No.` %in% inc_data$Info_Case_No.)%>%
  select(-BL312002,-BL312011)
#BL312002	Estimated GFR	단위가 3개 이상이라 제거함
#BL312011	Cystatin-C, based GFR	같이 제거함


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



anal_data<-inc_preop_lab%>%
  ungroup(`수술시작시간`)%>%
  select(-`수술시작시간`)%>%
  rename_at(c(-1),function(x) paste0('Lab_',x))%>%
  left_join(x=inc_data,y=.,by=c('Info_Case_No.'='Case No.'))

  