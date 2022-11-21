#This function adds SCORE to the dataframe. SCORE is a prediction of a risk of experiencing 
#cardiovascular death in 10 years, suggested by European Society of Cardiology.
#
#Input: 
#1) dat is a dataframe;
#2) nameAge, nameSex, nameSBP, nameHDL, nameCH, nameSmn are names of variables for Age, Sex, SBP, HDL, CH, and Smoking 
#status. It is assumed that SMN==3 denotes "Currently smoking". By default names are "AGE","SEX","SBP","HDL", "CH", "SMN".
#
#Created 21.11.22
#Revisited 21.11.22
#By Gleb

library(dplyr)

add_SCORE <- function(dat, nameAge = 'AGE', nameSex = 'SEX', nameSBP = 'SBP',
                       nameHDL = 'HDL', nameCH = 'CH', nameSmn = 'SMN'){
  
  dat_tmp = dat
  
  names(dat_tmp)[names(dat_tmp) == nameAge] = 'AGE'
  names(dat_tmp)[names(dat_tmp) == nameSex] = 'SEX'
  names(dat_tmp)[names(dat_tmp) == nameSBP] = 'SBP'
  names(dat_tmp)[names(dat_tmp) == nameHDL] = 'HDL'
  names(dat_tmp)[names(dat_tmp) == nameCH] = 'CH'
  names(dat_tmp)[names(dat_tmp) == nameSmn] = 'SMN'
  
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
  
  return(dat)
}
  
