library(readr)

raw_lab<-read_csv('Input/raw_data/IE_KIS_lab_raw.txt',col_types=cols(`시행일시`=col_datetime(format="%Y/%m/%d %H:%M:%S")))


raw_lab%>%
  #select(`Case No.`,수술시작시간,검사코드,접수일시)%>%
  filter(`시행일시`<`수술시작시간`)%>%
  group_by(`Case No.`,`수술시작시간`,`검사코드`)%>%
  arrange(desc(`시행일시`))%>%
  slice(1)%>%
  View()
  

  skimr::skim()

  
raw_lab%>%
  filter(`Case No.`==4,`검사코드`=='BL2011')%>%
  arrange(desc(시행일시))%>%
  View()
