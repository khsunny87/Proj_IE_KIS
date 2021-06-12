library(tidyr)
library(sjlabelled)
library(readr)

raw_preop_lab<-read_csv('Input/IE_KIS_preop_lab.csv')


anal_data<-raw_preop_lab%>%
  rename_at(c(-1),function(x) paste0('Lab_',x))%>%
  left_join(x=inc_data,y=.,by=c('Info_Case_No.'='Case No.'))





  