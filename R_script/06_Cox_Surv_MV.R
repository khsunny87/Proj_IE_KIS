
MV_candi
MV_trim<-UV_cox_data%>%
  select(S_TS,all_of(MV_candi))%>%
  na.omit()%>%
  as.data.frame()

MV_COX_res<-coxph(S_TS~.,data=MV_trim)%>%
  step(.,direction='both',trace=T) # MV 

#extractHR(MV_COX_res)
#ggforest(MV_COX_res,data=MV_trim)

fig_surv<-ggforest(MV_COX_res,data=MV_trim)

model_var<-MV_COX_res$terms%>%
  attr('term.labels')
ren_MV_trim<-MV_trim%>%
  select(S_TS,all_of(model_var))
names(ren_MV_trim)[-1]<-map_chr(names(ren_MV_trim)[-1],~Labels$변수설명[Labels$`변수`==.x])

ren_MV_COX_res<-coxph(S_TS~.,data=ren_MV_trim)

#ggforest(ren_MV_COX_res,data=ren_MV_trim)
fig_surv_ren<-ggforest(ren_MV_COX_res,data=ren_MV_trim)
