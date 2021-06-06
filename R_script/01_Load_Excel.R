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
  select(eff_var)

rm(tmp_data)

