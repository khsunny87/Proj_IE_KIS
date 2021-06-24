library(MatchIt)
library(cobalt)
library(kableExtra)
library(stringr)
library(ggplot2)



mat_cov<-c('Info_Age','Info_Male','Baseline_Ht','Baseline_Wt',#'Baseline_BMI',
      #     'Symptom_dyspnea','Symptom_fever','Symptom_chills',
      #     'Symptom_myalgia','Symptom_fatigue','Symptom_GW',
           'PMH_HTN','PMH_DM','PMH_CAD','PMH_CVA','PMH_CKD','PMH_dialysis','PMH_Smoking','PMH_Malignancy','PMH_Obesity',
           'IE_embolism','IE_culture','Op_Ix_HF','Op_Ix_uncontrolled_infection','Op_Ix_Embolism_vegetation',
      #     'Duration_Onset2Op',  #부정확
      #     'Duration_Anti2Op',
      #    'Lab_BL2011','Lab_BL2013','Lab_BL2016','Lab_BL3119','Lab_BL3120','Lab_BL201806',
      #     'Mitral_MS','Mitral_MR',
           'MR_gr_mod',
           'Aortic_IE_lesion','Aortic_Concomittant_AV_Op','AR_gr_mod',#'Aortic_AR', #'Aortic_AS' 없어서 제거
           'Tricuspid_IE_lesion','Tricuspid_Concomittant_TV_Op','TR_gr_mod',#'Tricuspid_TR', #'Tricuspid_TS' 없어서 제거
      #     'Concomitant_other_procedure',
           'Concomitant_CABG','Concomitant_maze','Concomitant_aorta',
           'Mitral_annular_recon')


pre_match<-anal_data%>%
  drop_na(any_of(mat_cov))

matched<-pre_match%>%
  #matchit(as.formula(paste0('Mitral_Repair~',paste0(mat_cov,collapse='+'))),data=.,method="optimal",ratio=1)
  matchit(as.formula(paste0('Mitral_Repair~',paste0(mat_cov,collapse='+'))),data=.,method="nearest",caliper=0.2,ratio=1)
  #matchit(Mitral_Repair~Info_Age+Baseline_Ht+Baseline_Wt+Baseline_BMI+Symptom_dyspnea+Symptom_fever+CPB_CPB_time+CPB_ACC_time,data=.,method="nearest",caliper=0.05,ratio=1)
  
  #matchit(Mitral_Repair~Info_Age+Baseline_Ht+Baseline_Wt+Baseline_BMI+Symptom_dyspnea+Symptom_fever+CPB_CPB_time+CPB_ACC_time,data=.,method="optimal",ratio=1)
res<-summary(matched)


#print(bal.tab(matched,continuous="std",s.d.denom = "pooled",m.threshold=0.2,v.threshold=2))

mat_tbl1<-res$nn%>%kable()%>% 
          kable_styling()
mat_tbl2<-cbind(res$sum.all[,1:3],res$sum.matched[,1:3])%>%
          kable()%>%
          add_header_above(c(" " = 1, "Raw data" = 3, "Matched data" = 3))

mat_fig1<-bal.plot(matched, var.name = "distance", which = "both",type = "histogram", mirror = T)

mat_bal<-bal.tab(matched,continuous="std",s.d.denom = "pooled",m.threshold=0.2,v.threshold=2)
mat_fig2<-love.plot(matched,s.d.denom = "pooled",stars="std",stat="mean.diffs",drop.distance=F,threshold=0.2,sample.names=c('Unmatched','Matched'))+coord_cartesian(xlim=c(-0.5,0.5))

mat_data<-match.data(matched)


#surv_data<-mat_data
#S_fit<-survfit(S_TS~Mitral_Repair,cluster=subclass,data=surv_data)
#surv_fig1<-ggsurvplot(S_fit,data=surv_data,risk.table=T,pval=T,break.time.by = 60,xlim = c(0, 180),legend.title="",surv.scale = "percent",legend.labs=c('Replacement','Repair'),xlab='Months after operation')
#surv_fig1
#ggsurvplot(S_fit,data=surv_data)
#S_fit
#,legend.title="",surv.scale = "percent",legend.labs=c('Replacement','Repair'),xlab='Months after operation')
#surv_fig1$plot <- surv_fig1$plot + theme(
#  axis.title.x = element_text(face='bold'),
#  axis.title.y = element_text(face='bold')
#)

#summary(S_fit)
