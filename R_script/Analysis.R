library(survival)
library(survminer)
library(lubridate)

anal_data$fum<-(anal_data$Outcomes_last_FU_date-anal_data$Info_수술일)/dmonths(1)
anal_data$TS<-Surv(anal_data$fum,anal_data$O_Survival_Death)
OS<-survfit(TS~Mitral_MVR,data=anal_data)
ggsurvplot(OS,data=anal_data,risk.table=T,pval=T,break.time.by = 60,xlim = c(0, 240))

print(OS)
summary(OS,times=60)
summary(OS,times=120)
#skim(anal_data$fum)

ggsurvplot(OS,data=anal_data,risk.table=F,pval=T)

fit1<-survfit(TS~Cate_Gr,data=anal_data)
print(fit1)
summary(fit1)
ggsurvplot(fit1,data=anal_data,risk.table=F,pval=F,break.time.by = 30,xlim = c(0, 120),legend.labs = c("CM", "SCM", "NoMP"))


surv_DC<-anal_data%>%
  filter(`Outcome_IHM`==0)

surv_DC%>%
  mytable(~Cate_Gr,data=.)
DC_OS<-survfit(TS~1,data=surv_DC)
print(DC_OS)
summary(DC_OS,times=60)
summary(DC_OS,times=120)


DC_fit<-survfit(TS~Cate_Gr,data=surv_DC)
print(DC_fit)
summary(DC_fit,times=60)
summary(DC_fit,times=120)
ggsurvplot(DC_fit,data=surv_DC,risk.table=F,pval=T,break.time.by = 30,xlim = c(0, 120),legend.labs = c("CM", "SCM", "NoMP"))



anal_data%>%
  filter(Cate_Gr %in% c(1,3))%>%
  survdiff(TS~Cate_Gr,data=.)
anal_data%>%
  filter(Cate_Gr %in% c(2,3))%>%
  survdiff(TS~Cate_Gr,data=.)

cox_trim<-anal_data%>%
  mutate(Cate_F=as.factor(-Cate_Gr),Op_factor=as.factor(Op_SurType),PMH_smk=PMH_preSmok>0)%>%
  select(TS,Info_Male,Info_Age,starts_with('Cate_'),starts_with('PMH_'),starts_with('Lab_'),starts_with('Op_'),starts_with('Comb_'),starts_with('CPB_'),DO_interval)%>%
  select(-PMH_preSmok,-Cate_Gr,-Cate_preopCT유무,-Cate_내용,-Cate_sxFU,-Lab_TnT,-Op_SurType,-Op_Asc,-Op_Hemi,-Op_Partial,-Op_Desc,-Op_TA,-Op_Abdao,-Op_Iliac,-Comb_PVDbypssname)

uv_out=mycph(TS~.,data=cox_trim)
uv_out%>%ztable
HRplot(uv_out,type=2,show.CI=TRUE)

candi2<-c("Info_Male","Info_Age","Cate_F","PMH_preHTN","PMH_smk","PMH_Marfan","Op_Root","Op_Total","Comb_CABG","CPB_CPB","CPB_TCA","DO_interval10")
candi_label<-c('Male','Age','MP','HTN','Smoking','Marfan','Root procedure','Total Arch','Combined CABG','CPB time','TCA time','Diagnosis to surgery')

cox_trim%>%
  mutate(DO_interval10=DO_interval/10)%>%
  #mutate(DO_interval10=DO_interval)%>%
  select(TS,contains(candi2))->uv_cox
names(uv_cox)<-c('TS',candi_label)
mycph(TS~.,data=uv_cox)->uv2

uv2%>%ztable()
HRplot(uv2,type=2,show.CI=TRUE)



cox_trim%>%
  replace_na(list(CPB_time=0))%>%
  mutate(DO_interval10=DO_interval/10)%>%
  #mutate(DO_interval10=DO_interval)%>%
  select(TS,contains(candi2))%>%na.omit()->
  mv_trim
names(mv_trim)<-c('TS',candi_label)

mv_out<-coxph(TS~.,data = mv_trim)
res<-step(mv_out,direction='backward')
res%>%ztable()
HRplot(res,type=2,show.CI=TRUE)




?HRplot
uv_out
uv_out%>%
  filter(p<0.2)

candi<-uv_out%>%
  filter(p<0.2)%>%row.names()
candi

mv_out<-cox_trim%>%
  #mutate(MPS=(Cate_F==-1))%>%
  select(TS,contains(candi))%>%
  select(-Cate_CMPS,-Cate_SCMPS)%>%
  #select(-starts_with("Lab_"))%>%
  coxph(TS~.,data=.)
mv_out

p_v<-summary(mv_out)$coefficients%>%.[,5]
p_v[p_v<0.05]




cox_trim%>%na.omit()%>%
  select(-Cate_CMPS,-Cate_SCMPS)%>%
  select(TS,contains(candi))->mv_trim

mv_out<-coxph(TS~.,data = mv_trim)
step(mv_out,direction='backward')



#EM 분석

cutoff<-1
fu<-ifelse(anal_data$Outcome_fum_mo<cutoff,anal_data$Outcome_fum_mo,cutoff)
death<-ifelse(anal_data$Outcome_fum_mo<cutoff&anal_data$Outcome_Death,TRUE,FALSE)

anal_data$EM_TS<-Surv(fu,death)

EM_fit<-survfit(EM_TS~Cate_Gr,data=anal_data)
print(EM_fit)
summary(EM_fit)
ggsurvplot(EM_fit,data=anal_data,risk.table=T,pval=T)


anal_data%>%
  filter(Cate_Gr %in% c(1,3))%>%
  survdiff(EM_TS~Cate_Gr,data=.)

anal_data%>%
  filter(Cate_Gr %in% c(2,3))%>%
  survdiff(EM_TS~Cate_Gr,data=.)


cox_trim<-anal_data%>%
  mutate(Cate_F=as.factor(-Cate_Gr),Op_factor=as.factor(Op_SurType))%>%
  select(EM_TS,Info_Male,Info_Age,starts_with('Cate_'),starts_with('PMH_'),starts_with('Lab_'),starts_with('Op_'),starts_with('Comb_'),starts_with('CPB_'))%>%
  select(-Cate_Gr,-Cate_preopCT유무,-Cate_내용,-Cate_sxFU,-Lab_TnT,-Op_SurType,-Op_Asc,-Op_Hemi,-Op_Partial,-Op_Total,-Op_Desc,-Op_TA,-Op_Abdao,-Op_Iliac,-Comb_PVDbypssname)



out=mycph(EM_TS~.,data=cox_trim)
out
out%>%
  filter(p<0.2)

candi<-out%>%
  filter(p<0.2)%>%row.names()


mv_out<-cox_trim%>%
  #mutate(MPS=(Cate_F==-1))%>%
  select(EM_TS,contains(candi))%>%
  select(-Cate_CMPS,-Cate_SCMPS)%>%
  #select(-starts_with("Lab_"))%>%
  coxph(EM_TS~.,data=.)
mv_out

p_v<-summary(mv_out)$coefficients%>%.[,5]
p_v[p_v<0.05]


cox_trim%>%na.omit()%>%
  select(-Cate_CMPS,-Cate_SCMPS)%>%
  select(EM_TS,contains(candi))->mv_trim

mv_out<-coxph(EM_TS~.,data = mv_trim)
step(mv_out,direction='backward')

