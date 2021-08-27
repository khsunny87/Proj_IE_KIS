library(survival)
library(survminer)
library(lubridate)
library(dplyr)
library(moonBook)


Get_UV_Cox<-function(TS_name,var_name,data,Dx_data=T){
  
  #3개 이상 factor 제한
  
  tmp_df=data.frame(TS=data[TS_name],var=data[var_name])
  names(tmp_df)<-c(TS_name,var_name)
  
  suppressWarnings(cox_model<-coxph(formula(paste(TS_name,'~',var_name)),data=tmp_df))
  res_cox<-summary(cox_model)
  if(any(is.infinite(res_cox$conf.int))) {
    cat(var_name,' was excluded : infinite\n')
    return(list())
  }
  
  if(Dx_data){
    return(
      list(cox_df=data.frame(Var=var_name,HR=res_cox['conf.int'][[1]][1],lcl=res_cox['conf.int'][[1]][3],ucl=res_cox['conf.int'][[1]][4],pval=res_cox['coefficients'][[1]][5]),
           cox_diag=cox.zph(cox_model)%>%ggcoxzph(.)
      )
    )}
  
  return(
    list(cox_df=data.frame(Var=var_name,HR=res_cox['conf.int'][[1]][1],lcl=res_cox['conf.int'][[1]][3],ucl=res_cox['conf.int'][[1]][4],pval=res_cox['coefficients'][[1]][5]))
  )
  
}



#UV Cox Survival
Cox_var<-c(raw_Cox_var,'PMH_prev.OHS')
cox_data<-anal_data

UV_cox_data<-cox_data%>%
  select(S_TS,all_of(Cox_var))
  #select(-PMH_prev.OHS,-Mitral_MR,-Mitral_MS,-Aortic_AR,-Aortic_AS,-Mitral_prev_MV_Op,-Aortic_Prev_AV_Op)%>%
  #select(-Tricuspid_TS,-Tricuspid_TR,-Tricuspid_Prev_TV_Op,-Tricuspid_IE_lesion)
  
UV_COX_res=mycph(S_TS~.,data=UV_cox_data)

UV_COX_res

UV_COX_list<-map(names(UV_cox_data)[-1],~Get_UV_Cox('S_TS',.x,data=UV_cox_data))
UV_COX_df<-map_df(UV_COX_list,~.$cox_df)


#cox_p<-cox_data%>%
#  select(-TS_Death)%>%
#  map_dbl(.,~Get_Cox_P(cox_data$TS_Death,.x))

#UV_COX_sig<-names(cox_p[cox_p<0.2])

UV_COX_sig<-UV_COX_df%>%
  filter(pval<0.05)%>%.$Var

MV_candi<-UV_COX_df%>%
  filter(pval<0.2)%>%.$Var



MV_trim<-UV_cox_data%>%
  select(S_TS,all_of(MV_candi))%>%
  na.omit()%>%
  as.data.frame()

MV_COX_res<-coxph(S_TS~.,data=MV_trim)%>%
  step(.,direction='both',trace=T) # MV 

extractHR(MV_COX_res)
ggforest(MV_COX_res,data=MV_trim)

#fig7<-ggforest(MV_COX_res,data=MV_trim)

#model_var<-MV_COX_res$terms%>%
#  attr('term.labels')
#ren_MV_trim<-MV_trim%>%
#  select(TS_Death,all_of(model_var))



############################################

#Cox Composite


C_UV_cox_data<-cox_data%>%
  select(C_TS,all_of(Cox_var))
#select(-PMH_prev.OHS,-Mitral_MR,-Mitral_MS,-Aortic_AR,-Aortic_AS,-Mitral_prev_MV_Op,-Aortic_Prev_AV_Op)%>%
#select(-Tricuspid_TS,-Tricuspid_TR,-Tricuspid_Prev_TV_Op,-Tricuspid_IE_lesion)

C_UV_COX_res=mycph(C_TS~.,data=C_UV_cox_data)

C_UV_COX_res

C_UV_COX_list<-map(names(C_UV_cox_data)[-1],~Get_UV_Cox('C_TS',.x,data=C_UV_cox_data))
C_UV_COX_df<-map_df(C_UV_COX_list,~.$cox_df)


#cox_p<-cox_data%>%
#  select(-TS_Death)%>%
#  map_dbl(.,~Get_Cox_P(cox_data$TS_Death,.x))

#UV_COX_sig<-names(cox_p[cox_p<0.2])

C_UV_COX_sig<-C_UV_COX_df%>%
  filter(pval<0.05)%>%.$Var

C_MV_candi<-C_UV_COX_df%>%
  filter(pval<0.2)%>%.$Var

C_MV_candi

C_MV_trim<-C_UV_cox_data%>%
  select(C_TS,all_of(C_MV_candi))%>%
  na.omit()%>%
  as.data.frame()

C_MV_COX_res<-coxph(C_TS~.,data=C_MV_trim)%>%
  step(.,direction='both',trace=T) # MV 

extractHR(C_MV_COX_res)
ggforest(C_MV_COX_res,data=C_MV_trim)

#fig7<-ggforest(MV_COX_res,data=MV_trim)

#model_var<-MV_COX_res$terms%>%
#  attr('term.labels')
#ren_MV_trim<-MV_trim%>%
#  select(TS_Death,all_of(model_var))


