
library(readr)


anal_data%>%
  select(-`Info_수술시작시간`,-`Info_환자번호`,-`Info_이름`,-Info_Inclusion_list,-`Info_Exlusion_비고`,-`Info_생년월일`,-`Info_나이(mon)`,-`Info_집도의`,-`IE_비고`,-`Info_비고`)%>%
  select(`Info_Case_No.`,`Info_Male`,`Info_Age`,everything())%>%View()
  write_excel_csv('통계지원.csv')


anal_data%>%
  select(AR_gr_mod,TR_gr_mod)%>%
  write_excel_csv('ARTR.csv')
View(anal_data)
