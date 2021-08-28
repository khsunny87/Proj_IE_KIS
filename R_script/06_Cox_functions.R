# Proj_IE_KIS

library(survival)
library(survminer)
library(lubridate)
library(dplyr)
library(moonBook)
library(purrr)


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

#surv_uv<-anal_data%>%
#  select(S_TS,all_of(raw_Cox_var))%>%
#  UV_Cox('S_TS')

UV_Cox<-function(data,TS_name,Dx_data=T){

  UV_COX_res=mycph(formula(paste(TS_name,'~ .')),data=data)
  UV_COX_list<-map(names(data)[-1],~Get_UV_Cox(TS_name,.x,data=data,Dx_data=Dx_data))
  UV_COX_df<-map_df(UV_COX_list,~.$cox_df)
  
  return(list(result=UV_COX_res,df=UV_COX_df,dx=UV_COX_list))
}


MV_Cox<-function(data,TS_name,trace=T,rename=F,keep_variable='1'){

  if(keep_variable!='1' & !(keep_variable %in%names(data))) {
    cat(keep_variable,'NOT in the MV_candidates')
    keep_variable='1'
  }

  MV_trim<-data%>%
    mutate_if(is.logical,~if_else(.x,1,0))%>%
    na.omit()%>%
    as.data.frame()

  MV_COX_res<-coxph(formula(paste(TS_name,'~ .')),data=MV_trim)%>%
    step(., scope = list(lower=  formula(paste('~',keep_variable))), direction = "both")
    
ret_list<-list(MV_data=MV_trim,MV_model=MV_COX_res,MV_forest=ggforest(MV_COX_res,data=MV_trim))

if(!rename) return(ret_list)

model_var<-MV_COX_res$terms%>%
  attr('term.labels')
ren_MV_trim<-MV_trim%>%
  select(MV_COX_res$terms[[2]],all_of(model_var))
names(ren_MV_trim)[-1]<-map_chr(names(ren_MV_trim)[-1],~if_else(.x%in%Labels$`변수`,Labels$`변수설명`[Labels$`변수`==.x],.x))
ren_MV_COX_res<-coxph(formula(paste(TS_name,'~ .')),data=ren_MV_trim)
return(append(ret_list,list(ren_MV_model=ren_MV_COX_res,ren_MV_forest=ggforest(ren_MV_COX_res,data=ren_MV_trim))))
}

my_ggadjKM<-function(MV_model,data,variable,method='conditional',ylab='Survival',leg_lab){
  
  
  KM_plot<-surv_adjustedcurves(MV_model,data=data,variable=variable,method=method)%>%
    ggplot(aes(x=time,y=surv,color=variable))+
    geom_step(size=1)+
    coord_cartesian(xlim = c(0, 180), ylim = c(0,1))+
    scale_x_continuous(breaks=seq(0, 180, 60))+
    scale_y_continuous(labels = scales::percent)+
    labs(x = "Months after operation",y=ylab)+
    scale_color_discrete(name="",labels=leg_lab)+
    theme_classic()+
    theme(text=element_text(size=12),axis.text=element_text(size=12,face='bold'),
          axis.title=element_text(size=12,face='bold'),
          #axis.text=element_text(size=12,face='bold'),
          axis.ticks=element_line(size=1),
          legend.position = "top",legend.direction = 'horizontal')
  
  return(KM_plot)
}


