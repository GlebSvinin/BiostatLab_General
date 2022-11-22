#This function adds SCORE and SCORE2 to the dataframe. SCORE is a prediction of a risk of experiencing 
#a cardiovascular death in 10 years; SCORE2 is a prediction of a risk of experiencing a fatal or 
#nonfatal cardiovascular event in 10 years. They were suggested by European Society of Cardiology 
#in 2003 and 2021 respectively.
#
#Input: 
#1) dat is a dataframe;
#2) nameAge, nameSex, nameSBP, nameHDL, nameCH, nameSmn are names of variables for Age, Sex, SBP, HDL, CH, and Smoking 
#status in "dat". It is assumed that SMN==3 denotes "Currently smoking". By default the names are "AGE","SEX","SBP","HDL", "CH", "SMN".
#3)scorenum == 1 adds SCORE to dataframe, scorenum == 2 adds SCORE2 to dataframe, scorenum = 'b' adds both. Default - 'b'.
#
#Created 22.11.22
#Revisited 22.11.22
#By Gleb


library(dplyr)

add_SCOREs <- function(dat, nameAge = 'AGE', nameSex = 'SEX', nameSBP = 'SBP',
                       nameHDL = 'HDL', nameCH = 'CH', nameSmn = 'SMN', scorenum = 'b'){
  dat_tmp = dat
  
  names(dat_tmp)[names(dat_tmp) == nameAge] = 'AGE'
  names(dat_tmp)[names(dat_tmp) == nameSex] = 'SEX'
  names(dat_tmp)[names(dat_tmp) == nameSBP] = 'SBP'
  names(dat_tmp)[names(dat_tmp) == nameHDL] = 'HDL'
  names(dat_tmp)[names(dat_tmp) == nameCH] = 'CH'
  names(dat_tmp)[names(dat_tmp) == nameSmn] = 'SMN'
  
  if(scorenum == 1 | scorenum == 'b'){
    A = ifelse(dat_tmp$SEX == 1, -21, -28.7)
    P = ifelse(dat_tmp$SEX == 1, 4.62, 6.23)
    
    Anon = ifelse(dat_tmp$SEX == 1, -25.7, -30)
    Pnon = ifelse(dat_tmp$SEX == 1, 5.47, 6.42)
    
    #1
    S_0 = exp( -exp(A) * (dat_tmp$AGE-20)^P )
    S_0_10 = exp( -exp(A) * (dat_tmp$AGE-10)^P )
    
    S_0_non = exp( -exp(Anon) * (dat_tmp$AGE-20)^Pnon )
    S_0_10_non = exp( -exp(Anon) * (dat_tmp$AGE-10)^Pnon )
    
    #2
    beta_smoke = 0.71
    beta_chol = 0.24
    beta_sbp = 0.018
    
    beta_smoke_non = 0.63
    beta_chol_non = 0.02
    beta_sbp_non = 0.022
    
    w = beta_chol*(dat_tmp$CH - 6) + beta_sbp*(dat_tmp$SBP - 120) + beta_smoke* as.numeric(dat_tmp$SMN==3)
    w_non = beta_chol_non*(dat_tmp$CH - 6) + beta_sbp_non*(dat_tmp$SBP - 120) + beta_smoke_non*as.numeric(dat_tmp$SMN==3)
    
    #3
    S = (S_0)^(exp(w))
    S_10 = (S_0_10)^(exp(w))
    
    S_non = (S_0_non)^(exp(w_non))
    S_10_non  = (S_0_10_non)^(exp(w_non))
    
    #4
    S_ten = S_10/S
    S_ten_non = S_10_non/S_non
    
    #5
    risk_10 = 1 - S_ten 
    risk_10_non = 1 - S_ten_non
    
    #6
    res = risk_10_non + risk_10
    
    
    dat_tmp$VK_SCORE = res * 100
    dat$VK_SCORE = dat_tmp$VK_SCORE
  }
  
  if(scorenum == 2 | scorenum == 'b'){
    dat_tmp$age_c = (dat_tmp$AGE - 60)/5
    dat_tmp$sbp_c = (dat_tmp$SBP - 120)/20
    dat_tmp$ch_c = dat_tmp$CH - 6
    dat_tmp$hdl_c = (dat_tmp$HDL - 1.3)/0.5
    dat_tmp$smk = 0
    dat_tmp$smk[dat_tmp$SMN == 3] = 1
    
    {log_c_age_m = 0.3742
      log_c_sm_m = 0.6012
      log_c_sbp_m = 0.2777
      log_c_tot_ch_m = 0.1458
      log_c_hdl_m = -0.2698
      log_c_smxage_m = -0.0755
      log_c_sbpxage_m = -0.0255
      log_c_tot_chxage_m = -0.0281
      log_c_hdlxage_m = 0.0426
      log_base_m = 0.9605}
    
    {log_c_age_f = 0.4648
      log_c_sm_f = 0.7744
      log_c_sbp_f = 0.3131
      log_c_tot_ch_f = 0.1002
      log_c_hdl_f = -0.2606
      log_c_smxage_f = -0.1088
      log_c_sbpxage_f = -0.0277
      log_c_tot_chxage_f = -0.0226
      log_c_hdlxage_f = 0.0613
      log_base_f = 0.9776}
    
    {scale1_m = 0.5836
      scale2_m = 0.8294
      scale1_f = 0.9412
      scale2_f = 0.8329}
    
    
    dat_tmp$lin = (2-dat_tmp$SEX)*(log_c_age_m*dat_tmp$age_c + log_c_sm_m * dat_tmp$smk + log_c_sbp_m * dat_tmp$sbp_c +
                                     log_c_tot_ch_m *dat_tmp$ch_c + log_c_hdl_m * dat_tmp$hdl_c+
                                     log_c_smxage_m*dat_tmp$age_c*dat_tmp$smk + log_c_sbpxage_m*dat_tmp$age_c*dat_tmp$sbp_c+
                                     log_c_tot_chxage_m*dat_tmp$age_c*dat_tmp$ch_c+
                                     log_c_hdlxage_m*dat_tmp$age_c*dat_tmp$hdl_c) - 
      (1 - dat_tmp$SEX)*(log_c_age_f*dat_tmp$age_c + log_c_sm_f * dat_tmp$smk + log_c_sbp_f * dat_tmp$sbp_c +
                           log_c_tot_ch_f *dat_tmp$ch_c + log_c_hdl_f * dat_tmp$hdl_c+
                           log_c_smxage_f*dat_tmp$age_c*dat_tmp$smk + log_c_sbpxage_f*dat_tmp$age_c*dat_tmp$sbp_c+
                           log_c_tot_chxage_f*dat_tmp$age_c*dat_tmp$ch_c+
                           log_c_hdlxage_f*dat_tmp$age_c*dat_tmp$hdl_c)
    
    dat_tmp$uncalib = (2 - dat_tmp$SEX)*(1 - log_base_m^(exp(dat_tmp$lin))) - (1 - dat_tmp$SEX)*(1 - log_base_f^(exp(dat_tmp$lin)))
    
    dat_tmp$SCORE2 = (2-dat_tmp$SEX)*(1 - exp(-exp(scale1_m + scale2_m * log(-log(1 - dat_tmp$uncalib))))) - 
      (1-dat_tmp$SEX)*(1 - exp(-exp(scale1_f + scale2_f * log(-log(1 - dat_tmp$uncalib)))))
    
    dat_tmp$SCORE2 = dat_tmp$SCORE2%>%round(3)
    dat_tmp$SCORE2 = dat_tmp$SCORE2*100
    
    dat$SCORE2 = dat_tmp$SCORE2
  }
  
  return(dat)
} 



