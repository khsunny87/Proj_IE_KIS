mat_data

raw_Cox_var
Cox_var<-setdiff(raw_Cox_var,c('PMH_CVA','Aortic_IE_lesion'))

Surv_UV<-mat_data%>%
  select(S_TS,all_of(Cox_var))%>%
  UV_Cox('S_TS',Dx_data=F)

Surv_UV$df%>%
  filter(pval<0.05)
  mutate_if(is.numeric,~round(.x,digits=3))%>%
  knitr::kable(format = 'pipe')%>%print()

MV_candi<-Surv_UV$df%>%
    filter(pval<0.05)%>%.$Var
MV_candi

Surv_MV<-mat_data%>%
  select(S_TS,all_of(MV_candi),'Mitral_Repair')%>%
  MV_Cox('S_TS',trace=T,rename=F,keep_variable='Mitral_Repair')
Surv_MV$MV_forest



Composite_UV<-mat_data%>%
  select(C_TS,all_of(Cox_var))%>%
  UV_Cox('C_TS',Dx_data=F)

Composite_UV$df%>%
  filter(pval<0.05)

MV_candi<-Composite_UV$df%>%
  filter(pval<0.05)%>%.$Var

Composite_MV<-mat_data%>%
  select(C_TS,all_of(MV_candi),Mitral_Repair)%>%
  MV_Cox('C_TS',trace=T,rename=F,keep_variable = 'Mitral_Repair')
Composite_MV$MV_forest




Surv_UV$result%>%knitr::kable(format = 'pipe')%>%print()

survfit(S_TS~Mitral_repair,data=anal_data)
coxph(S_TS~Lab_BL2013,data=anal_data)
coxph(C_TS~Lab_BL2013,data=anal_data)
coxph(S_TS~Lab_BL2013+Mitral_repair+Info_Age,data=anal_data)
coxph(C_TS~Lab_BL2013+Mitral_repair+Info_Age,data=anal_data)

coxph
mutate(S_TS=Surv(S_fum,O_Survival_Death))%>%
  #mutate(O_Composite=(O_Survival_Death|O_Recurrence_Recur|O_Valve_ReOp))%>%
  #  mutate(C_date=pmin(Outcomes_last_FU_date,O_Survival_Death_date,O_Recurrence_Recur_Dx._date,O_Valve_ReOp_date,na.rm=T))%>%
  mutate(O_Composite=(O_Survival_Death|O_Recurrence_Recur|O_Valve_ReOp|O_Valve_B_major|O_Valve_E_major))%>%
  mutate(C_date=pmin(Outcomes_last_FU_date,O_Survival_Death_date,O_Recurrence_Recur_Dx._date,O_Valve_ReOp_date,O_Valve_B_mj_date,O_Valve_E_mj_date,na.rm=T))%>%
  #mutate(O_Composite=(O_Survival_Death|O_Recurrence_Recur|O_Valve_ReOp|O_Valve_B_major|O_Valve_E_major|O_Valve_B_minor|O_Valve_E_minor))%>%
  #  mutate(C_date=pmin(Outcomes_last_FU_date,O_Survival_Death_date,O_Recurrence_Recur_Dx._date,O_Valve_ReOp_date,O_Valve_B_mj_date,O_Valve_E_mj_date,O_Valve_B_mn_date,O_Valve_E_mn_date,na.rm=T))%>%
  
  mutate(C_fum=(C_date-Info_수술일)/dmonths(1))%>%
  mutate(C_TS=Surv(C_fum,O_Composite))

library(moonBook)
library(dplyr)
library(purrr)

View(acs)
mytable(sex~.,data=acs,show.total=T,catMethod=0)

Calc_SMD(sex~.,data=acs)
Calc_SMD<-function(f,data){

myt<-terms(f,data=data)
group<-as.character(myt[[2]])

####일단 이항 그룹인지 확인


if(length(unique(data[[group]]))!=2)
  cat(group,"is NOT binomial factor")

myvar<-attr(myt,'term.labels')

return(map_df(myvar,Calc_each_SMD,group=group,data=data))

}

Calc_each_SMD<-function(item,group,data,digits=4){
  cate=unique(data[[group]])
  G1<-data%>%filter(.[[group]]==cate[1])%>%.[[item]]
  G2<-data%>%filter(.[[group]]==cate[2])%>%.[[item]]
  
  if(is.numeric(data[[item]]) & length(unique(data[[item]]))>2){
    cat(item,'is a continuous variable\n')
    smd=(mean(G1,na.rm=T)-mean(G2,na.rm=T))/sqrt(((sd(G1,na.rm=T)^2+sd(G2,na.rm=T)^2))/2)
  }
  else if(length(unique(data[[item]]))==2){
    cat(item,'is a binomial variable\n')
    X=unique(G1)[1]
    p1<-sum(G1==X,na.rm=T)/sum(!is.na(G1),na.rm=T)
    p2<-sum(G2==X,na.rm=T)/sum(!is.na(G2),na.rm=T)
    smd=(p1-p2)/sqrt((p1*(1-p1)+p2*(1-p2))/2)
  }
  else{
  cat(item,'is NOT proper variable\n')
  smd=NA
  }
  return(data.frame(variable=item,smd=round(smd,digits=digits)))
}

  
View(res)
View(res$res)

tmp<-raw_data%>%
  mutate(`Info_환자번호`=str_pad(as.character(`Info_환자번호`),8,'left','0'))



reop_id<-with(tmp,{unique(Info_환자번호[duplicated(Info_환자번호)])})



NVE<-tmp%>%
  filter(`Info_환자번호` %in% reop_id)%>%
  filter(`Info_Inclusion_list`=='TRUE')%>%.$`Info_환자번호`
  

tmp%>%
  #filter(`Info_환자번호` %in% reop_id)%>%
  filter(`Info_환자번호` %in% NVE)%>%
  select(`Info_환자번호`,`Info_이름`,`Info_수술일`,`Info_Inclusion_list`,`Info_Exlusion_비고`)%>%
  arrange(`Info_환자번호`,`Info_수술일`)%>%
  View()


unique(raw_data$Info_환자번호)

anyDuplicated(
  
tmp<-c(3,1,1,1)
unique(tmp)
