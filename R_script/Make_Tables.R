library(moonBook)



inc_data%>%
  mytable(Mitral_MVR~PMH_HTN,data=.,show.total=T,catMethod=0)%>%
  compress(add.label=F)
