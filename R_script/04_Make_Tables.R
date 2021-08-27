library(moonBook)
library(ztable)
library(forcats)
library(kableExtra)
library(purrr)

myt_output<-'viewer' #default

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

print_mytable<-function(myt){
  
  if(myt_output=='viewer'){
    options(ztable.type='viewer')
    myt%>%ztable()%>%print()
    
  } else if(myt_output=='HTML'){
    options(ztable.type='HTML')
    myt%>%ztable()%>%print()
  } else if(myt_output=='PPT'){
    myt%>%mytable2df()%>%knitr::kable(format='pipe',row.names=F)%>%print()
  }
  
}


baseline_tbl<-tbl_data%>%
  select(Mitral_Repair,Info_Male,Info_Age,starts_with('Baseline_'),starts_with('Symptom_'),IE_embolism,starts_with('PMH_'),
         IE_culture,starts_with('Lab_'),starts_with('Op_Ix_'),starts_with('Duration_'),
         Mitral_prev_MV_Op,Mitral_MS,Mitral_MR,MR_gr_mod,Mitral_annular_recon,
         Aortic_Prev_AV_Op,Aortic_IE_lesion,Aortic_AS,Aortic_AR,AR_gr_mod,Aortic_Concomittant_AV_Op,
         Tricuspid_Prev_TV_Op,Tricuspid_IE_lesion,Tricuspid_TS,Tricuspid_TR,TR_gr_mod,Tricuspid_Concomittant_TV_Op,
         starts_with('Concomitant_'),starts_with('CPB_'),
         O_Survival_Death,O_Survival_EM,O_Survival_IHM,O_Recurrence_Recur,O_Recurrence_ReOp,O_Valve_ReOp,
         O_Valve_Bleeding,O_Valve_B_major,O_Valve_B_minor,O_Valve_Embolism,O_Valve_E_major,O_Valve_E_minor)%>%
  select(-CPB_TCA_time)%>%
  mutate(Mitral_Repair=fct_relevel(if_else(Mitral_Repair==1,'Repair','Replacement'),'Repair'))%>%
  mytable(Mitral_Repair~.,data=.,show.total=T,catMethod=0)%>%
  compress(add.label=F)

smd_table<-tbl_data%>%
  select(Mitral_Repair,Info_Male,Info_Age,starts_with('Baseline_'),starts_with('Symptom_'),IE_embolism,starts_with('PMH_'),
         IE_culture,starts_with('Lab_'),starts_with('Op_Ix_'),starts_with('Duration_'),
         Mitral_prev_MV_Op,Mitral_MS,Mitral_MR,MR_gr_mod,Mitral_annular_recon,
         Aortic_Prev_AV_Op,Aortic_IE_lesion,Aortic_AS,Aortic_AR,AR_gr_mod,Aortic_Concomittant_AV_Op,
         Tricuspid_Prev_TV_Op,Tricuspid_IE_lesion,Tricuspid_TS,Tricuspid_TR,TR_gr_mod,Tricuspid_Concomittant_TV_Op,
         starts_with('Concomitant_'),starts_with('CPB_'),
         O_Survival_Death,O_Survival_EM,O_Survival_IHM,O_Recurrence_Recur,O_Recurrence_ReOp,O_Valve_ReOp,
         O_Valve_Bleeding,O_Valve_B_major,O_Valve_B_minor,O_Valve_Embolism,O_Valve_E_major,O_Valve_E_minor)%>%
  select(-CPB_TCA_time)%>%
  mutate(Mitral_Repair=fct_relevel(if_else(Mitral_Repair==1,'Repair','Replacement'),'Repair'))%>%
  Calc_SMD(Mitral_Repair~.,data=.)
  

raw_org_tbl<-table(tbl_data$IE_균주)%>%as.data.frame()%>%
  rename(Organisms=Var1)

MVR_tbl<-tbl_data%>%
  filter(!Mitral_Repair)%>%
  select(Mitral_Prosthesis,Info_Male,Info_Age,
         #starts_with('Baseline_'),starts_with('Symptom_'),IE_embolism,starts_with('PMH_'),
         #IE_culture,starts_with('Lab_'),starts_with('Op_Ix_'),starts_with('Duration_'),
         #Mitral_prev_MV_Op,Mitral_MS,Mitral_MR,MR_gr_mod,Mitral_annular_recon,
         #Aortic_Prev_AV_Op,Aortic_IE_lesion,Aortic_AS,Aortic_AR,AR_gr_mod,Aortic_Concomittant_AV_Op,
         #Tricuspid_Prev_TV_Op,Tricuspid_IE_lesion,Tricuspid_TS,Tricuspid_TR,TR_gr_mod,Tricuspid_Concomittant_TV_Op,
         #starts_with('Concomitant_'),starts_with('CPB_'),
         O_Survival_Death,O_Survival_EM,O_Survival_IHM,O_Recurrence_Recur,O_Recurrence_ReOp,O_Valve_ReOp,
         O_Valve_Bleeding,O_Valve_B_major,O_Valve_B_minor,O_Valve_Embolism,O_Valve_E_major,O_Valve_E_minor)%>%
  #select(-CPB_TCA_time)%>%
  mutate(Mitral_Prosthesis=fct_relevel(if_else(Mitral_Prosthesis=="T",'Tissue','Mechanical'),'Tissue'))%>%
  mytable(Mitral_Prosthesis~.,data=.,show.total=F,catMethod=0)%>%
  compress(add.label=F)
  
  


sum(raw_org_tbl$Freq)

tmp1<-raw_org_tbl%>%
  filter(Organisms!='(NG)',Organisms!='Others')%>%
  arrange(desc(Freq))

tmp2<-raw_org_tbl%>%
  filter(Organisms=='Others')

tmp3<-raw_org_tbl%>%
  filter(Organisms=='(NG)')

tmp_tbl<-rbind(tmp1,tmp2,tmp3)
levels(tmp_tbl$Organisms)[levels(tmp_tbl$Organisms)=='(NG)']<-'Culture-negative'

colnames(tmp_tbl)[2]<-paste0('Total (N=',sum(tmp_tbl$Freq),')')


org_tbl<-tmp_tbl%>%kable()%>%
  
          kable_styling()


#anal_data%>%
#  mutate(Mitral_MVR=fct_relevel(if_else(Mitral_MVR,'Replacement','Repair'),'Repair'))%>%
#  select(Mitral_MVR,Info_Sex,Info_Age,starts_with('Baseline_'),starts_with('Symptom_'),starts_with('PMH_'),starts_with('Lab_'),starts_with('Duration_'),starts_with('CPB_'),starts_with('Concomitant_'),Mitral_MS,Mitral_MR)%>%
#  select(-CPB_TCA_time)%>%names()%>%data.frame()%>%
#  write_excel_csv('Input/label.csv')
  

