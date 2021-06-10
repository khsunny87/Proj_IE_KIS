library(readr)
library(dplyr)
library(tidyr)

raw_lab<-read_csv('Input/raw_data/IE_KIS_lab_raw.txt',col_types=cols(`시행일시`=col_datetime(format="%Y/%m/%d %H:%M:%S")))


raw_lab%>%
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
  View()

raw_lab%>%
  filter(`Case No.` %in% inc_data$Info_Case_No.,`검사코드`=='BL312002')%>%
  select(`검사결과수치값`,`검사결과단위명`)%>%
  mutate(unit=factor(`검사결과단위명`))%>%
  ggplot()+
    geom_density(aes(fill=unit,x=`검사결과수치값`),alpha=.4)+
    xlim(c(0,200))
  mytable(unit~`검사결과수치값`,data=.)

  ?geom_density
  

raw_lab%>%
  group_by(`검사코드`,`검사명`,`검사결과단위명`)%>%summarise()%>%View()
  
raw_lab%>%
  group_by(`검사코드`,`검사명`)%>% #,`검사결과단위명`)%>%summarise()%>%View()
  summarise(`단위`= tail(`검사결과단위명`,n=1))%>%
  View()


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




