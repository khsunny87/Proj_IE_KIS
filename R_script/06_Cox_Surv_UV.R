# Proj_IE_KIS

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
Cox_var<-raw_Cox_var

UV_cox_data<-cox_data%>%
  select(S_TS,all_of(Cox_var))

UV_COX_res=mycph(S_TS~.,data=UV_cox_data)

#UV_COX_res

UV_COX_list<-map(names(UV_cox_data)[-1],~Get_UV_Cox('S_TS',.x,data=UV_cox_data))
UV_COX_df<-map_df(UV_COX_list,~.$cox_df)


#cox_p<-cox_data%>%
#  select(-TS_Death)%>%
#  map_dbl(.,~Get_Cox_P(cox_data$TS_Death,.x))

#UV_COX_sig<-names(cox_p[cox_p<0.2])

UV_COX_sig<-UV_COX_df%>%
  filter(pval<0.05)%>%.$Var

MV_candi<-UV_COX_df%>%
  filter(pval<MV_cut)%>%.$Var

