library(readxl)
library(dplyr)
library(stringr)
library(readr)
library(xlsx)

fname<-'Input/CRF/CRF_210326.xlsx'


#변수설정
read_excel(fname,n_max=2,col_names=F)%>%t()%>%as.data.frame()->tmp
var_name<-paste0(tmp[[1]],'_',gsub(" ","_",tmp[[2]]))


tmp_data<-as.data.frame(read_excel(fname,skip=1))
names(tmp_data)<-var_name

eff_var<-var_name[!is.na(tmp[[1]])]


raw_data<-tmp_data%>%
  select(eff_var)%>%
  mutate(Info_환자번호=str_pad(Info_환자번호,8,'left','0'))

inc_list<-read_excel('Input/CRF/inclusion_list.xlsx')%>%
  filter(Exclusion==0)%>%.$환자번호%>%as.character()%>%str_pad(.,8,'left','0')%>%unique()

output<-raw_data%>%
  mutate(inclusion=Info_환자번호%in%inc_list)%>%
  select(Info_No.,Info_환자번호,Info_이름,inclusion)
  


output%>%
#  write.xlsx('Chart_review.xlsx')
write_excel_csv('Chart_review_inclusion.csv')

nrow(output%>%filter(inclusion))

#read_csv('Chart_review.csv')%>%View()
