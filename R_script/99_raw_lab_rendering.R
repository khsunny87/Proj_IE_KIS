library(readr)
library(dplyr)

raw_lab<-read_csv('Input/raw_data/IE_KIS_lab_raw.txt',col_types=cols(`접수일시`=col_datetime(format="%Y/%m/%d %H:%M:%S")))

raw_preop_lab<-raw_lab%>%
  #select(`Case No.`,수술시작시간,검사코드,접수일시)%>%
  filter(`접수일시`<`수술시작시간`)%>%
  group_by(`Case No.`,`수술시작시간`,`검사코드`)%>%
  arrange(desc(`접수일시`))%>%
  slice(1)%>%
  select(`검사결과수치값`)%>%
  ungroup(`검사코드`)%>%
  #group_by(`접수일시`)%>%
  pivot_wider(names_from=검사코드,values_from=검사결과수치값)%>%
  ungroup(`수술시작시간`)%>%
  select(-`수술시작시간`)%>%
  select(-BL312002,-BL312011)%>% #BL312002	Estimated GFR	단위가 3개 이상이라 제거함
#BL312011	Cystatin-C, based GFR	같이 제거함
  select(c(1:13))

raw_lab2<-read_csv('Input/raw_data/IE_KIS_lab_raw2.txt',col_types=cols(`접수일시`=col_datetime(format="%Y/%m/%d %H:%M:%S")))

raw_preop_lab2<-raw_lab2%>%
  filter(`접수일시`<`수술시작시간`)%>%
  group_by(`Case No.`,`수술시작시간`,`검사코드`)%>%
  arrange(desc(`접수일시`))%>%
  slice(1)%>%
  select(`검사결과수치값`)%>%
  ungroup(`검사코드`)%>%
  pivot_wider(names_from=검사코드,values_from=검사결과수치값)%>%
  ungroup(`수술시작시간`)%>%
  select(-`수술시작시간`)
  
  

  

output<-raw_preop_lab%>%
  left_join(.,raw_preop_lab2)

output$BL3119[output$`Case No.`==487]<-9.6
output$BL3120[output$`Case No.`==487]<-1.02


output%>%
  write_excel_csv('Input/IE_KIS_preop_lab.csv')

raw_lab%>%
  group_by(`검사코드`,`검사명`)%>%
  summarise()%>%View()


raw_PFT<-read_csv('Input/raw_data/IE_KIS_PFT.txt',col_types=cols(`시행일시`=col_datetime(format="%Y/%m/%d %H:%M:%S")))
raw_PFT%>%
  #select(`Case No.`,수술시작시간,검사코드,접수일시)%>%
  filter(`시행일시`<`수술시작시간`)%>%
  group_by(`Case No.`,`수술시작시간`,`검사코드`)%>%
  arrange(desc(`시행일시`))%>%
  slice(1)%>%
  select(`검사결과내용`)%>%
  ungroup(`검사코드`)%>%
  #group_by(`접수일시`)%>%
  pivot_wider(names_from=검사코드,values_from=검사결과내용)%>%
  filter(`Case No.` %in% inc_data$Info_Case_No.)%>%
  View()




