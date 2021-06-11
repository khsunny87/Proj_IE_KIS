library(readr)
library(dplyr)

raw_lab<-read_csv('Input/raw_data/IE_KIS_lab_raw.txt',col_types=cols(`시행일시`=col_datetime(format="%Y/%m/%d %H:%M:%S")))





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




