res_skim<-skim(anal_data)


res_skim%>%as.data.frame()%>%
  select(skim_variable,skim_type,n_missing)%>%
  write_excel_csv('skim_var.csv')
