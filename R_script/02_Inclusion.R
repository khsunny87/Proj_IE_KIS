inc_data<-raw_data%>%
  slice(1:321)%>%
  filter(Info_Inclusion_list=='TRUE')
