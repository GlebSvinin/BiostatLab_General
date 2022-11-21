#This is a genereal descriptive function. 
#Input: 
#1) dat is a dataframe;
#2) groupingvar is a variable for dividing table into two groups regerding to some property of an object. 
#Default - no groupingvar
#3) vecnames is a vector of column names to perform descriptive analysis; 
#4) vecfuns is a vector of descriptive functions to be applied to columns from vecnames. It has the same length with vecnames. 
#'Me' - median [Q25, Q75], 'Mean' - mean ±sd, 'Perc' - n (m %), 'Num' - sum(!is.na(vec))
#5) tablecolnames is a vector of column names for output table
#6) tablerownames is a vector of row names for output table
#7)pval = TRUE adds Mann-Whitney test p-val between column values form colnames of groups defined by groupingvar. Default 
#pval = FALSE. 
#
#Example 1:
#gen_desc(dat,groupingvar = 'SEX', vecnames = c('CH', 'HDL', 'CITY_01'), vecfuns = c('Me', 'Mean','Perc'), pval = TRUE)
#                    CH        HDL       CITY_01
#1     5.58 (4.89; 6.31) 1.35 ±0.41 3907 (26.5 %)
#2     5.55 (4.82; 6.33) 1.47 ±0.37 2287 (37.2 %)
#pvals             0.136      0.000         MW NA
#
#Example 2:
#gen_desc(dat,groupingvar = 'SEX', vecnames = c('CH', 'HDL', 'CITY_01'), vecfuns = c('Me', 'Mean','Perc'),
#                      tablecolnames = c('ОХС', "ЛВП", "Город"), tablerownames = c('М',"Ж"), pval = TRUE)
#                    ОХС        ЛВП         Город
#М     5.58 (4.89; 6.31) 1.35 ±0.41 3907 (26.5 %)
#Ж     5.55 (4.82; 6.33) 1.47 ±0.37 2287 (37.2 %)
#pvals             0.136      0.000         MW NA
#
#MW NA indicates that Mann-Whitney test is not very appropriate for binary random variables

#Created 01.10.22
#Revisited 21.11.22
#By Gleb

library(dplyr)

round_ed <-function(x,r) { 
  return(format(round(x,r), nsmall = r) ) 
}

mean_sd <- function(vec){
  m = mean(vec, na.rm = T)
  std = sd(vec, na.rm = T)
  n = sum(!is.na(vec))
  paste0(round(m,2), " ±", round(std,2))
}

quant <- function(vec){
  m = median(vec, na.rm = T)
  m1 = quantile(vec, probs = 0.25, na.rm = T)
  m2 = quantile(vec, probs = 0.75, na.rm =T)
  n = sum(!is.na(vec))
  paste0(round(m,2), ' (', round(m1,2),'; ',round(m2,2), ')')
}

perc <- function(vec){
  m = mean(vec, na.rm = TRUE)*100
  paste0(sum(vec, na.rm = TRUE), " (", round(m, 1), " %)")
}

number<-function(vec){
  n = sum(!is.na(vec))
  return(n)
}

func_switch <- function(vec, type){
  if(type == 'Me'){
    return(quant(vec))
  } else if(type == 'Mean'){
    return(mean_sd(vec))
  } else if(type == 'Perc'){
    return(perc(vec))
  } else if(type == 'Num'){
    return(number(vec))
  }
}


gen_desc <- function(dat, groupingvar = 0, vecnames, vecfuns, tablecolnames = c(), tablerownames = c(), pval = FALSE){
  
  l = length(vecnames)
  if(sum(vecnames %in% names(dat)) != length(vecnames)){
    print('One or more elements from vecnames is not present in names(dat)')
    return(-1)
  }
  
  if(groupingvar != 0){
    dat$groupvar = dat[[groupingvar]]
  } else{
    dat$groupvar = 0
  }
  
  dat$addit_column_1 = dat[[vecnames[1]]]

  res = dat %>% group_by(groupvar) %>% summarise(n1 = func_switch(addit_column_1, vecfuns[1]))
  
  if(l>1){
    for(i in c(2:l)){
      nametmp = vecnames[i]
      dat$tmpcol = dat[[nametmp]]
      tmp = dat %>% group_by(groupvar) %>% summarise(col = func_switch(tmpcol, vecfuns[i]))
      
      res = cbind(res, tmp[,-1])
      
    }
  }  
  rownames(res) = res$groupvar
  names(res) = c('group',vecnames)
  lables = unique(dat$groupvar)
  
  if(pval == TRUE){
    w_tmp = wilcox.test(dat$addit_column_1[dat$groupvar == lables[1]] %>% as.numeric(), dat$addit_column_1[dat$groupvar == lables[2]] %>% as.numeric())
    ps = w_tmp$p.value %>% round_ed(3)
    if(l>1){
      for(i in c(2:l)){
        nametmp = vecnames[i]
        dat$tmpcol = dat[[nametmp]]
        w_tmp = wilcox.test(dat$tmpcol[dat$groupvar == lables[1]]  %>% as.numeric(), dat$tmpcol[dat$groupvar == lables[2]] %>% as.numeric())
        p_tmp = w_tmp$p.value %>% round_ed(3)
        ps = c(ps, p_tmp)
      }
    }
    
    res = res[,-1]
    
    if(length(tablerownames)!=0){
      if(length(tablerownames) != length(unique(dat$groupvar))){
        print('Number of rows is different from number of tablerownames')
        return(-1)
      }
      rownames(res) = tablerownames
    }
    
    res = rbind(res, c(ps))
    
    rownames(res)[length(rownames(res))] = 'pvals'
    
    res[3,(vecfuns=='Perc'|vecfuns=='Num')] = 'MW NA'
    } 
  else if(pval == FALSE){
    res = res[,-1]
  }
  
  if(length(tablecolnames)!= 0){
    if(length(tablecolnames) != length(vecnames)){
      print('Length of tablecolnames is not equal to length of vecnames')
      return(-1)
    }
    colnames(res) = tablecolnames
  }
  
  return(res)
}





