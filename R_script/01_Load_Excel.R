library(readxl)
library(dplyr)

fname<-'Input/IE_KIS_data.xlsx'

#변수설정
read_excel(fname,n_max=2,col_names=F)%>%t()%>%as.data.frame()->tmp
var_name<-paste0(tmp[[1]],'_',gsub(" ","_",tmp[[2]]))


tmp_data<-read_excel(fname,skip=2,col_names=F)
names(tmp_data)<-var_name

eff_var<-var_name[!is.na(tmp[[1]])]

raw_data<-tmp_data%>%
  select(eff_var)%>%
  mutate(Info_Inclusion_list=factor(Info_Inclusion_list))

rm(tmp_data)


var_sheet<-read_excel(fname,sheet='변수정리')
Labels<-var_sheet%>%
  select(변수,변수설명)%>%
  filter(!is.na(변수설명))

raw_Cox_var<-var_sheet%>%
  filter(UV_Cox=='Y')%>%.$변수
