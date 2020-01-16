library(foreign)
library(dplyr)
library(reshape2)
library(ggplot2)
library(lme4)
library(usdm)

sr<- read.csv("C:/Users/Om/OneDrive - Queen's University/Fall 2018/ECON 435/sr.csv")




sr$Class<-factor(sr$Class,levels = c("H","UM","LM","L"))
col<-c(80:101)

sr[col]<-lapply(sr[col],factor)
sr$b_communityres_2011<-as.factor(sr$b_communityres_2011)
sr$b_hosp_2011 <-as.factor(sr$b_hosp_2011)
sr$b_outpatientfac_2011<-as.factor(sr$b_outpatientfac_2011)
sr$b_daytreat_2011 <-as.factor(sr$b_daytreat_2011)
sr$mh_legis <-as.factor(sr$mh_legis)
sr$mh_plan <-as.factor(sr$mh_plan)
sr$mh_policy <-as.factor(sr$mh_policy)
sr$child_abuse_pgm_12.14 <-as.factor(sr$child_abuse_pgm_12.14)
sr$dom_abuse_12.14 <-as.factor(sr$dom_abuse_12.14)
sr$sen_abuse_12.14 <-as.factor(sr$sen_abuse_12.14)

sr$Dev<-ifelse(sr$Class=="H"|sr$Class=="UM",1,0)
sr$Dev<-as.factor(sr$Dev)

summary(sr$awr_binge_drinking)


p<-ggplot(data=sr,aes(x=Dev, y=std_sr))+geom_boxplot()
p

p<-ggplot(data=sr,aes(x=Dev, y=SR_2015))+geom_boxplot()
p

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

sr %>%
  group_by(Dev) %>%
  mutate(outlier = ifelse(is_outlier(SR_2015), Country, as.numeric(NA))) %>%
  ggplot(., aes(x = factor(Dev), y = SR_2015)) +
  geom_boxplot() +
  geom_text(aes(label = sr$Country[outlier]), na.rm = TRUE, hjust = -0.3)


ggplot(sr, aes(x = factor(Dev), y = SR_CHG_1015)) +geom_point(aes(label = sr$Country[outlier]), na.rm = TRUE, hjust = -0.3)


r<-ggplot(data=sr,aes(x=Region, y=sr$SR_CHG_1015))+geom_boxplot()+theme(axis.text.x = element_text(angle=45))
r

s<-ggplot(data=sr,aes(x=Region, y=sr$SR_p_CHG))+geom_boxplot()+theme(axis.text.x = element_text(angle=45))
s

q<-ggplot(data=sr,aes(x=SR_2010, y=`SR_2015`,color=sr$Class))+geom_point()
q

j<-ggplot(data=sr,aes(x=SR_2010, y=SR_CHG_1015,color=sr$Class))+geom_point()
j

j<-ggplot(data=sr,aes(x=log10(SR_2010), y=log10(SR_2015),color=sr$Class))+geom_point()
j

cor(sr$SR_2010,sr$SR_CHG_1015)
cor(sr$SR_2010,sr$std_sr)

lookat2005<-lm(sr$SR_~ sr$SR_2005 + sr$SR_2010, data=sr)
summary(lookat2005)


vif(lookat2005)


sr%>%
  group_by(.dots=c("Dev")) %>%
  summarize(obs=n(),mean=mean(`SR_2016`,na.rm=TRUE),median=median(`SR_2016`,na.rm=TRUE), standard_dev = sd(`SR_2016`,na.rm=TRUE))

sr%>%
  group_by(.dots=c("Region")) %>%
  summarize(obs=n(),mean=mean(`SR_2016`,na.rm=TRUE),median=median(`SR_2016`,na.rm=TRUE), standard_dev = sd(`SR_2016`,na.rm=TRUE))


ggplot(sr,aes(x=sr$SR_2010,y=sr$SR_2015,color=sr$Class))+geom_point()+theme(axis.text.x = element_blank())

ggplot(sr,aes(x=log10(sr$SR_2010),y=log10(sr$SR_2015),color=sr$Dev))+geom_point()+theme(axis.text.x = element_blank())


sr_het<-lm(sr$SR_2015~sr$SR_2010,data=sr)
summary(sr_het)
lmtest::bptest(sr_het)
sr_alc<- lm(sr$`SR_2016` ~ sr$Alc_2016 + sr$Heavy_ttl_2016 + sr$risk_impov_hc_2015 + sr$unemp_adv_2015, data=sr, na.rm=FALSE)
summary(sr_alc)


summary(sr$Class)
sr_alc<- lm(sr$SR_2015~sr$Region + sr$SR_2010 + sr$Alc_Chg_1015:sr$Class + sr$awr_dd:sr$Class + sr$awr_youthdrink:sr$Class + sr$conf_drug_disorder_2014:sr$Class + sr$awr_binge_drinking:sr$Class + sr$awr_illalc:sr$Class  + sr$treat_sub_add:sr$Class + sr$treat_sub_alt:sr$Class + sr$alc_policy_before:sr$Class  + sr$alc_policy_rev.add_during:sr$Class,data=sr)
summary(sr_alc)
vif(sr_alc)
sink("sr_2015 class alc.txt")
print(summary(sr_alc))
sink()

sr_alc_c<- lm(sr$SR_CHG_1015~sr$Region + sr$Alc_Chg_1015:sr$Class + sr$awr_dd:sr$Class + sr$awr_youthdrink:sr$Class + sr$conf_alc_disorder_2014:sr$Class + sr$conf_drug_disorder_2014:sr$Class + sr$awr_binge_drinking:sr$Class + sr$awr_illalc:sr$Class +sr$treat_alc_add:sr$Class + sr$treat_alc_alt:sr$Class + sr$treat_sub_add:sr$Class + sr$treat_sub_alt:sr$Class + sr$alc_policy_before:sr$Class + sr$alc_policy_during:sr$Class + sr$alc_policy_rev.add_during:sr$Class,data=sr)
summary(sr_alc_c)
sink("sr change alc class.txt")
print(summary(sr_alc_c))
sink()
#dev
summary(sr$Class)
sr_alc_dev<- lm(log10(sr$SR_2015)~log10(sr$SR_2010) + sr$Alc_Chg_1015:sr$Dev + sr$awr_binge_drinking:sr$Dev + sr$awr_dd:sr$Dev + sr$awr_youthdrink:sr$Dev + sr$conf_drug_disorder_2014:sr$Dev + sr$awr_illalc:sr$Dev + sr$treat_sub_add:sr$Dev + sr$treat_sub_alt:sr$Dev + sr$alc_policy_before:sr$Dev + sr$alc_policy_rev.add_during:sr$Dev,data=sr)
summary(sr_alc_dev)
vif(sr_alc_dev)
sink("sr_2015 alc dev.txt")
print(summary(sr_alc_dev))
sink()

sr_alc_c_dev<- lm(sr$SR_CHG_1015~sr$Region + sr$Alc_Chg_1015:sr$Dev + sr$awr_binge_drinking:sr$Dev + sr$awr_dd:sr$Dev + sr$awr_youthdrink:sr$Dev + sr$conf_alc_disorder_2014:sr$Dev + sr$conf_drug_disorder_2014:sr$Dev + sr$awr_binge_drinking:sr$Dev + sr$awr_illalc:sr$Dev +sr$treat_alc_add:sr$Dev + sr$treat_alc_alt:sr$Dev + sr$treat_sub_add:sr$Dev + sr$treat_sub_alt:sr$Dev + sr$alc_policy_before:sr$Dev + sr$alc_policy_during:sr$Dev + sr$alc_policy_rev.add_during:sr$Dev,data=sr)
summary(sr_alc_c_dev)
sink("sr change alc dev.txt")
print(summary(sr_alc_c_dev))
sink()



##mental shit
sr_hs<-lm(sr$SR_CHG_1015~ sr$SR_2010 + sr$Region + sr$govt_exp_mental.health_2011:sr$Class + sr$govt_exp_hosp.health_2011:sr$Class + sr$govt_exp_mh:sr$Class + sr$govt_exp_mental.health_2011:sr$Class +sr$che_chg:sr$Class,  data=sr)
summary(sr_hs)
sink("sr hs class.txt")
print(summary(sr_hs))
sink()

sr_hs_d<-lm(log10(sr$SR_2015)~log10(sr$SR_2010)+ sr$Region + sr$govt_exp_mental.health_2011 sr$govt_exp_mh:sr$Dev + sr$govt_exp_hosp:sr$Dev ,  data=sr)
summary(sr_hs_d)
vif(sr_hs_d)
sink("sr hs dev.txt")
print(summary(sr_hs_d))
sink()

sr_hs_chg<-lm(sr$SR_CHG_1015~ sr$Region + sr$govt_exp_mental.health_2011:sr$Class + sr$govt_exp_hosp.health_2011:sr$Class + sr$govt_exp_mh:sr$Class + sr$govt_exp_mental.health_2011:sr$Class +sr$che_chg:sr$Class,  data=sr)
summary(sr_hs_chg)
sink("sr hs class chg.txt")
print(summary(sr_hs_chg))
sink()

sr_hs_d_chg<-lm(sr$SR_CHG_1015~  sr$Region + sr$govt_exp_mental.health_2011:sr$Dev + sr$govt_exp_hosp.health_2011:sr$Dev + sr$govt_exp_mh:sr$Dev + sr$govt_exp_mental.health_2011:sr$Dev +sr$che_chg:sr$Class,  data=sr)
summary(sr_hs_d_chg)
sink("sr hs dev chg.txt")
print(summary(sr_hs_d_chg))
sink()

sr_pol_c<- lm(sr$SR_CHG_1015~ sr$SR_2010 + sr$Region + sr$mh_legis:sr$Class + sr$mh_plan:sr$Class + sr$mh_policy:sr$Class , data=sr)
summary(sr_pol_c)
sink("sr policy class 2015.txt")
print(summary(sr_pol_c))
sink()

sr_pol_c_chg<- lm(sr$SR_CHG_1015 ~  sr$Region + sr$mh_legis:sr$Class + sr$mh_plan:sr$Class + sr$mh_policy:sr$Class , data=sr)
summary(sr_pol_c_chg)
sink("sr policy class chg.txt")
print(summary(sr_pol_c_chg))
sink()

sr_pol_d<- lm(log10(sr$SR_2015)~log10(sr$SR_2010) + sr$Region + sr$mh_legis:sr$Dev , data=sr)
summary(sr_pol_d)
vif(sr_pol_d)
sink("sr policy Dev 2015.txt")
print(summary(sr_pol_d))
sink()

sr_pol_d_chg<- lm(sr$SR_CHG_1015 ~  sr$Region + sr$mh_legis:sr$Dev + sr$mh_plan:sr$Dev + sr$mh_policy:sr$Dev , data=sr)
summary(sr_pol_d_chg)
sink("sr policy Dev chg.txt")
print(summary(sr_pol_d_chg))
sink()

sr_res_Class_2015<- lm(sr$SR_CHG_1015~ sr$SR_2010 + sr$Region + sr$psychologist_chg:sr$Class + sr$beds_gen_hosp_chg:sr$Class + sr$beds_mh_chg:sr$Class, data=sr)
summary(sr_res_Class_2015)
sink("sr res Class 2015.txt")
print(summary(sr_res_Class_2015))
sink()

sr_res_Class_Chg<- lm(sr$SR_CHG_1015~  sr$Region + sr$psychologist_chg:sr$Class + sr$beds_gen_hosp_chg:sr$Class + sr$beds_mh_chg:sr$Class, data=sr)
summary(sr_res_Class_Chg)
sink("sr res Class Chg.txt")
print(summary(sr_res_Class_Chg))
sink()

sr_ppl<- lm(log10(sr$SR_2015)~log10(sr$SR_2010) + sr$Region + sr$psychiatrist_chg:sr$Dev + sr$nurse_chg:sr$Dev  + sr$psychologist_chg:sr$Dev, data =sr)
summary(sr_ppl)
vif(sr_ppl)
sr_res_Dev_2015<- lm(log10(sr$SR_2015)~log10(sr$SR_2010) + sr$Region + sr$psychiatrist_chg:sr$Dev + sr$beds_gen_hosp_chg:sr$Dev + sr$beds_mh_chg:sr$Dev, data=sr)
summary(sr_res_Dev_2015)
vif(sr_res_Dev_2015)
sink("sr res Dev 2015.txt")
print(summary(sr_res_Dev_2015))
sink()

sr_res_Dev_Chg<- lm(sr$SR_CHG_1015~  sr$Region + sr$psychologist_chg:sr$Dev + sr$beds_gen_hosp_chg:sr$Dev + sr$beds_mh_chg:sr$Dev, data=sr)
summary(sr_res_Dev_Chg)
sink("sr res Dev Chg.txt")
print(summary(sr_res_Dev_Chg))
sink()


sr_awr_Class_2015<- lm(sr$SR_CHG_1015~ sr$SR_2010 + sr$Region + sr$awr_dom_viol:sr$Class + sr$awr_eld:sr$Class + sr$awr_indig:sr$Class + sr$awr_harmother:sr$Class +sr$child_abuse_pgm_12.14:sr$Class + sr$youth_viol_pvn_12.14:sr$Class + sr$dom_abuse_12.14:sr$Class + sr$sen_abuse_12.14:sr$Class, data=sr)
summary(sr_awr_Class_2015)
sink("sr awr Class 2015.txt")
print(summary(sr_awr_Class_2015))
sink()

sr_awr_Class_Chg<- lm(sr$SR_CHG_1015~  sr$Region + sr$awr_dom_viol:sr$Class + sr$awr_eld:sr$Class + sr$awr_indig:sr$Class + sr$awr_harmother:sr$Class +sr$child_abuse_pgm_12.14:sr$Class + sr$youth_viol_pvn_12.14:sr$Class + sr$dom_abuse_12.14:sr$Class + sr$sen_abuse_12.14:sr$Class, data=sr)
summary(sr_awr_Class_Chg)
sink("sr awr Class Chg.txt")
print(summary(sr_awr_Class_Chg))
sink()

sr_awr_Dev_2015<- lm(log10(sr$SR_2015)~log10(sr$SR_2010) + sr$Region + sr$awr_indig:sr$Dev + sr$awr_harmother:sr$Dev +sr$child_abuse_pgm_12.14:sr$Dev + sr$dom_abuse_12.14:sr$Dev + sr$sen_abuse_12.14:sr$Dev, data=sr)
summary(sr_awr_Dev_2015)
vif(sr_awr_Dev_2015)
sink("sr awr Dev 2015.txt")
print(summary(sr_awr_Dev_2015))
sink()

sr_awr_Dev_Chg<- lm(sr$SR_CHG_1015~  sr$Region + sr$awr_dom_viol:sr$Dev + sr$awr_eld:sr$Dev + sr$awr_indig:sr$Dev + sr$awr_harmother:sr$Dev +sr$child_abuse_pgm_12.14:sr$Dev + sr$youth_viol_pvn_12.14:sr$Dev + sr$dom_abuse_12.14:sr$Dev + sr$sen_abuse_12.14:sr$Dev, data=sr)
summary(sr_awr_Dev_Chg)
sink("sr awr Dev Chg.txt")
print(summary(sr_awr_Dev_Chg))
sink()




sr_final_Dev_2015<-lm(log10(sr$SR_2015) ~ log10(sr$SR_2010)+ sr$Alc_Chg_1015:sr$Dev  + sr$treat_sub_alt:sr$Dev +sr$awr_binge_drinking:sr$Dev + sr$awr_illalc:sr$Dev + sr$psychiatrist_chg:sr$Dev + sr$beds_mh_chg:sr$Dev + sr$dom_abuse_12.14:sr$Dev +sr$child_abuse_pgm_12.14:sr$Dev, data=sr)
summary(sr_final_Dev_2015)
vif(sr_final_Dev_2015)
sink("sr final dev.txt")
print(summary(sr_final_Dev_2015))
sink()

lmtest::bptest(sr_final_Dev_2015)

sr_final_Dev_Chg<-lm(sr$SR_p_CHG ~ log10(sr$SR_2010)+ sr$Alc_Chg_1015:sr$Dev  + sr$treat_alc_alt:sr$Dev +sr$awr_binge_drinking:sr$Dev +  + sr$psychiatrist_chg:sr$Dev + sr$beds_mh_chg:sr$Dev + sr$dom_abuse_12.14:sr$Dev +sr$child_abuse_pgm_12.14:sr$Dev+sr$youth_viol_pvn_12.14:sr$Dev, data=sr)
summary(sr_final_Dev_Chg)


