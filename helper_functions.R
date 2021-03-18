recover_or_test_r <- function(df,parms,symptoms){
  
  df %>%
    subset(Inf.days==Inf.pd) %>%
    mutate(Rec.days = 0) %>%
    dplyr::select(ID,VL,arrival,departure,VL_waning,Rec.days,
                  V_doses,V_refused,V1_t,V2_t,VE_i,VE_s,VE_p) -> recovered
  
  df %>%
    subset(!(ID %in% recovered$ID)) -> df
  
  df %>%
    subset(removal.pd == ID.days) -> tested1
  
  df %>% 
    subset(!(ID %in% tested1$ID) & !(ID %in% recovered$ID)) -> df
  
  if(symptoms=="A"){    ## asymptomatics who are identified through testing
    df %>%
      subset(ID.pd==ID.days & VL>=parms[["VL.threshold.r"]] & is.na(removal.pd)) %>%
      mutate(removal.pd = ID.pd + parms[["test_delay_r"]]
             #,ID.days = ID.days + 1
             ) -> df1
    
  }else{
    
    df %>%
      subset(ID.days == parms[["id.I"]] | (ID.days == ID.pd & VL>=parms[["VL.threshold.r"]] & is.na(removal.pd))) %>%
      mutate(removal.pd = case_when(ID.days == parms[["id.I"]] ~ ID.days, 
                                   (ID.days == ID.pd & VL>=parms[["VL.threshold.r"]]) ~ ID.pd + parms[["test_delay_r"]]) #ID.days != parms[["id.I"]] &
             # ,ID.days = ID.days + 1
             ) -> df1   ##  ## symptomatics who are identified through testing OR symptoms
  }
  
  
  df %>%
    subset(ID.pd==ID.days & VL<parms[["VL.threshold.r"]] & is.na(removal.pd)  & !(ID %in% df1$ID)) %>%  # !(ID %in% df1$ID) excludes people identified through symptoms
    mutate(Inf.days=Inf.days + 1,
           ID.pd = ID.pd + parms[["test_freq_r"]],
           ID.days = ID.days + 1,
           VL = case_when(Inf.days <=VL_rise ~ VL * (Inf.days+1)/(Inf.days),
                          Inf.days > VL_rise  ~ VL - VL_waning),
           VL = case_when(VL < 0 ~ 0, TRUE ~ VL),
           Infectiousness = case_when(VL<4 ~ 0, 
                                      VL>=4 & VL<7 ~ 0.5, 
                                      VL>=7 ~ 1)) -> df2   ## anyone who is tested but NOT identified 
  
  df %>%
    subset((ID.days < ID.pd | removal.pd > ID.days | is.na(ID.days)) & !(ID %in% df1$ID)) %>%   ## if just tested pos, won't show up here becasuse df1 has not been merged w/ df
                                                                               ## including | is.na(ID.days) |  for I.rC (who are not tested, but need VL)
    mutate(Inf.days = Inf.days + 1,
           ID.days = ID.days + 1,
           VL = case_when(Inf.days <=VL_rise ~ VL * (Inf.days+1)/(Inf.days),
                          Inf.days > VL_rise ~ VL - VL_waning),
           VL = case_when(VL < 0 ~ 0, TRUE ~ VL),
           Infectiousness = case_when(VL<4 ~ 0, 
                                      VL>=4 & VL<7 ~ 0.5, 
                                      VL>=7 ~ 1)) -> df3   # those not yet tested
                                                           # or those who have tested not gotten positive results back
  
  df1 %>% 
    subset(ID.days == removal.pd) %>%
    mutate(ID.days = NA) -> tested2
  
  tested1 %>%
    bind_rows(tested2) -> tested
  
  df1 %>%
    subset(ID.days != removal.pd) %>%
    mutate(ID.days = ID.days + 1,
           Inf.days = Inf.days + 1,
           VL = case_when(Inf.days <=VL_rise ~ VL * (Inf.days+1)/(Inf.days),
                          Inf.days > VL_rise  ~ VL - VL_waning)) %>%
    bind_rows(df2) %>%
    bind_rows(df3) -> df
  
  
  list("recovered"=recovered,
       "tested"=tested,
       "df"=df)
}

recover_I.rC <- function(df,parms){
  
  df %>%
    subset(Inf.days==Inf.pd) %>%
    mutate(Rec.days = 0) %>%
    dplyr::select(ID,VL,arrival,departure,VL_waning,Rec.days,
                  V_doses,V_refused,V1_t,V2_t,VE_i,VE_s,VE_p) -> recovered
  
  df %>%
    subset(!(ID %in% recovered$ID)) %>%
    mutate(Inf.days = Inf.days + 1,
           VL = case_when(Inf.days <=VL_rise ~ VL * (Inf.days+1)/(Inf.days),
                          Inf.days > VL_rise ~ VL - VL_waning),
           VL = case_when(VL < 0 ~ 0, TRUE ~ VL)) -> df  
  
  list("recovered"=recovered,
       "df"=df)
}
  

recover_or_test_hcw <- function(df,parms, total, symptoms, ratio){
  
  df %>%
    subset(Inf.days==Inf.pd) %>%
    mutate(Rec.days = 0) %>%
    dplyr::select(ID,VL,VL_waning,Rec.days,
                  V_doses,V_refused,V1_t,V2_t,VE_i,VE_s,VE_p) -> recovered
  
  df %>%
    subset(!(ID %in% recovered$ID)) -> df
  
  df %>%
    subset(removal.pd == ID.days) -> tested1
  
  df %>% 
    subset(!(ID %in% tested1$ID) & !(ID %in% recovered$ID)) -> df
  
  
  if(symptoms=="A"){    ## asymptomatics who are identified through testing
    df %>%
      subset(ID.pd==ID.days & VL>=parms[["VL.threshold.hcw"]] & is.na(removal.pd)) %>%
      mutate(removal.pd = ID.pd + parms[["test_delay_hcw"]])  -> df1
        
             #Home.pd=parms[["gamma"]],
             #Home.days=0
              #%>% dplyr::select(-ID.pd,-ID.days)
    
  }else{
    
    df %>%
      subset(ID.days == parms[["id.I"]] | (ID.days == ID.pd & VL>=parms[["VL.threshold.hcw"]] & is.na(removal.pd))) %>%
      mutate(removal.pd = case_when(ID.days == parms[["id.I"]] ~ ID.days, 
                                    (ID.days == ID.pd & VL>=parms[["VL.threshold.hcw"]]) ~ ID.pd + parms[["test_delay_hcw"]])
             ) -> df1  ##  ## symptomatics who are identified through testing OR symptoms
  }
  
  
  df %>%
    subset(ID.pd==ID.days & VL<parms[["VL.threshold.hcw"]] & is.na(removal.pd) & !(ID %in% df1$ID)) %>%
    mutate(Inf.days=Inf.days + 1,
           ID.days=ID.days + 1,
           ID.pd = ID.pd +  parms[["test_freq_hcw"]],
           VL = case_when(Inf.days <=VL_rise ~ VL * (Inf.days+1)/(Inf.days),
                          Inf.days > VL_rise  ~ VL - VL_waning),
           VL = case_when(VL < 0 ~ 0, TRUE ~ VL),
           Infectiousness = case_when(VL<4 ~ 0, 
                                      VL>=4 & VL<7 ~ 0.5, 
                                      VL>=7 ~ 1)) -> df2   ## anyone who is tested but NOT identified 
  
  df %>%
    subset((ID.days < ID.pd | removal.pd > ID.days) & !(ID %in% df1$ID)) %>%
    mutate(Inf.days = Inf.days + 1,
           ID.days = ID.days + 1,
           VL = case_when(Inf.days <=VL_rise ~ VL * (Inf.days+1)/(Inf.days),
                          Inf.days > VL_rise  ~ VL - VL_waning),
           VL = case_when(VL < 0 ~ 0, TRUE ~ VL),
           Infectiousness = case_when(VL<4 ~ 0, 
                                      VL>=4 & VL<7 ~ 0.5, 
                                      VL>=7 ~ 1)) -> df3  ## anyone who is not tested, does not have symptoms
  
  df1 %>% 
    subset(ID.days == removal.pd) %>%
    mutate(ID.days = NA) -> tested2
  
  
  tested1 %>%
    bind_rows(tested2) %>%
    mutate(Home.pd=parms[["gamma"]],
           Home.days=0) %>% 
    dplyr::select(-ID.pd,-ID.days,-removal.pd) -> tested
  
  
  df1 %>%
    subset(ID.days != removal.pd) %>%
    mutate(ID.days = ID.days + 1,
           Inf.days = Inf.days + 1,
           VL = case_when(Inf.days <=VL_rise ~ VL * (Inf.days+1)/(Inf.days),
                          Inf.days > VL_rise  ~ VL - VL_waning)) %>% 
    bind_rows(df2) %>%
    bind_rows(df3) -> df
  
  
  if (nrow(tested)>0){
    
    if (isFALSE(parms[["shortage"]]) | ratio < parms[["shortage_threshold"]]){
      new_E <- rbinom(1,nrow(tested),parms[["I.C"]])
      new_R <- rbinom(1,(nrow(tested)-new_E),parms[["prop_rhcwR"]])
      new_S <- nrow(tested) - new_E - new_R
      if (new_S >0){
        new.hcwS <- as.data.frame(cbind("ID"=10000+c((total + 1):(total + new_S)),"VL"=NA,
                                        "V_doses" = 0, "V_refused" = NA,  
                                        "V1_t"=NA, "V2_t"=NA,                          
                                        "VE_i"=0, "VE_s"=0, "VE_p"=0))
      } else{
        new.hcwS <-  as.data.frame(cbind("ID"=as.character(),"VL"=as.numeric(),
                                         "V_doses" = as.numeric(), "V_refused" = as.numeric(),  
                                         "V1_t"=as.numeric(), "V2_t"=as.numeric(),                        
                                         "VE_i"=as.numeric(), "VE_s"=as.numeric(), "VE_p"=as.numeric()))
      }
      if (new_E >0){
        new.hcwE <- as.data.frame(cbind("ID"=10000+c((total + new_S + 1):(total+new_S + new_E)),"VL"=0,
                                        "V_doses" = 0, "V_refused" = NA,  
                                        "V1_t"=NA, "V2_t"=NA,                          
                                        "VE_i"=0, "VE_s"=0, "VE_p"=0,
                                      Inc.pd=round(runif(length(new_E), parms[["sigma1"]], parms[["sigma2"]])),Days=0))
      } else{
        new.hcwE <- as.data.frame(cbind("ID"=as.character(),"VL"=as.numeric()),
                                  "V_doses" = as.numeric(), "V_refused" = as.numeric(),  
                                  "V1_t"=as.numeric(), "V2_t"=as.numeric(),                        
                                  "VE_i"=as.numeric(), "VE_s"=as.numeric(), "VE_p"=as.numeric(),
                                  "Inc"=as.numeric(),"Days"=as.numeric())
      }
      if (new_R >0){
        new.hcwR <- as.data.frame(cbind("ID"=10000+c((total + new_S + new_E + 1):(total+new_S + new_E + new_R)),"VL"=0, 
                                        "V_doses" = 0, "V_refused" = NA,  
                                        "V1_t"=NA, "V2_t"=NA,                          
                                        "VE_i"=0, "VE_s"=0, "VE_p"=0,
                                        "VL_waning"=0, "Rec.days"=0)) # assume 0 VL
      } else{
        new.hcwR <-  as.data.frame(cbind("ID"=as.character(),"VL"=as.numeric(),
                                         "V_doses" = as.numeric(), "V_refused" = as.numeric(),  
                                         "V1_t"=as.numeric(), "V2_t"=as.numeric(),                        
                                         "VE_i"=as.numeric(), "VE_s"=as.numeric(), "VE_p"=as.numeric(),
                                         "VL_waning"=as.numeric(),"Rec.days"=as.numeric()))
      }
    } else{
      new.hcwS <-  as.data.frame(cbind("ID"=as.character(),"VL"=as.numeric(),
                                       "V_doses" = as.numeric(), "V_refused" = as.numeric(),  
                                       "V1_t"=as.numeric(), "V2_t"=as.numeric(),                        
                                       "VE_i"=as.numeric(), "VE_s"=as.numeric(), "VE_p"=as.numeric()))
      new.hcwE <- as.data.frame(cbind("ID"=as.character(),"VL"=as.numeric(),
                                      "V_doses" = as.numeric(), "V_refused" = as.numeric(),  
                                      "V1_t"=as.numeric(), "V2_t"=as.numeric(),                        
                                      "VE_i"=as.numeric(), "VE_s"=as.numeric(), "VE_p"=as.numeric(),
                                      "Inc"=as.numeric(),"Days"=as.numeric()))
      new.hcwR <-  as.data.frame(cbind("ID"=as.character(),"VL"=as.numeric(),
                                       "V_doses" = as.numeric(), "V_refused" = as.numeric(),  
                                       "V1_t"=as.numeric(), "V2_t"=as.numeric(),                        
                                       "VE_i"=as.numeric(), "VE_s"=as.numeric(), "VE_p"=as.numeric(),
                                       "VL_waning"=as.numeric(),"Rec.days"=as.numeric()))
    }
  } else{
    new.hcwS <-  as.data.frame(cbind("ID"=as.character(),"VL"=as.numeric(),
                                     "V_doses" = as.numeric(), "V_refused" = as.numeric(),  
                                     "V1_t"=as.numeric(), "V2_t"=as.numeric(),                        
                                     "VE_i"=as.numeric(), "VE_s"=as.numeric(), "VE_p"=as.numeric()))
    new.hcwE <- as.data.frame(cbind("ID"=as.character(),"VL"=as.numeric(),
                                    "V_doses" = as.numeric(), "V_refused" = as.numeric(),  
                                    "V1_t"=as.numeric(), "V2_t"=as.numeric(),                        
                                    "VE_i"=as.numeric(), "VE_s"=as.numeric(), "VE_p"=as.numeric(),
                                    "Inc"=as.numeric(),"Days"=as.numeric()))
    new.hcwR <-  as.data.frame(cbind("ID"=as.character(),"VL"=as.numeric(),
                                     "V_doses" = as.numeric(), "V_refused" = as.numeric(),  
                                     "V1_t"=as.numeric(), "V2_t"=as.numeric(),                        
                                     "VE_i"=as.numeric(), "VE_s"=as.numeric(), "VE_p"=as.numeric(),
                                     "VL_waning"=as.numeric(),"Rec.days"=as.numeric()))
  }
  
  tested %>%
   subset(!(ID>10000)) -> tested
  
  list("recovered"=recovered,
       "tested"=tested,
       "df"=df,
       "new.hcwE"=new.hcwE,
       "new.hcwS"=new.hcwS,
       "new.hcwR"=new.hcwR)
  
}

recover_home_hcw <- function(I.hcwH){    
                                         
  I.hcwH %>%
    subset(Home.pd==Home.days) %>%
    mutate(Rec.days = 0) %>%
    dplyr::select(ID,VL,VL_waning,Rec.days,
                  V_doses,V_refused,V1_t,V2_t,VE_i,VE_s,VE_p) -> recovered
  
  I.hcwH %>%
    subset(!(ID %in% recovered$ID)) %>%
    mutate(Inf.days = Inf.days + 1,
           Home.days = Home.days + 1,
           VL = case_when(Inf.days <=VL_rise ~ VL * (Inf.days+1)/(Inf.days),
                          Inf.days > VL_rise  ~ VL - VL_waning),
           VL = case_when(VL < 0 ~ 0, TRUE ~ VL),
           Infectiousness = case_when(VL<4 ~ 0, 
                                      VL>=4 & VL<7 ~ 0.5, 
                                      VL>=7 ~ 1)) -> I.hcwH
  
  list(recovered,I.hcwH)
  
}


E_to_I_r <- function(df,parms){
  
  df %>%
    subset(Days==Inc.pd) %>%
    dplyr::select(ID,VL,arrival,departure,V_doses,V_refused,V1_t,V2_t,VE_i,VE_s,VE_p) %>%
    mutate(asympt = rbinom(length(ID),1,(1-(1-parms[["alpha.r"]])*(1-VE_p))),
           Inf.pd=parms[["gamma"]],
           Inf.days=0,
           ID.pd=case_when(
             asympt==1 ~ ifelse(parms[["test_freq_r"]]==1000, 1000, round(runif(length(ID), 1, parms[["test_freq_r"]]))),
             asympt==0 ~ pmin(ifelse(parms[["test_freq_r"]]==1000, 1000,round(runif(length(ID), 1, parms[["test_freq_r"]]))), parms[["id.I"]])),
           ID.days = 0, 
           removal.pd = NA) -> infected
  
  infected %>%
    subset(asympt==1) %>%
    mutate(VL_rise = round(runif(sum(asympt==1), 1, 4)),
           VL = abs(rnorm(sum(asympt==1), 8, 1))/(VL_rise+1), 
           VL_waning = VL*(VL_rise+1)/runif(sum(asympt==1), 15, 21), 
           Infectiousness = case_when(VL<4 ~ 0, 
                                      VL>=4 & VL<7 ~ 0.5, 
                                      VL>=7 ~ 1))-> infected_asympt
  
  infected %>%
    subset(asympt==0) %>%
    mutate(VL_rise = round(runif(sum(asympt==0), 1, 4)),
           VL = abs(rnorm(sum(asympt==0), 8, 1))/(VL_rise+1), 
           VL_waning = VL*(VL_rise+1)/runif(sum(asympt==0), 15, 21),
           Infectiousness = case_when(VL<4 ~ 0, 
                                      VL>=4 & VL<7 ~ 0.5, 
                                      VL>=7 ~ 1)) -> infected_sympt
  
  df %>%
    subset(Days<Inc.pd) %>%
    mutate(Days=Days + 1) -> df
  
  list("infected_asympt"=infected_asympt,
       "infected_sympt"=infected_sympt,
       "df"=df)
}

E_to_I_hcw <- function(df,parms){
  
  df %>%
    subset(Days==Inc.pd) %>%
    dplyr::select(ID,VL,V_doses,V_refused,V1_t,V2_t,VE_i,VE_s,VE_p) %>%
    mutate(asympt = rbinom(length(ID),1,(1-(1-parms[["alpha.hcw"]])*(1-VE_p))),
           Inf.pd=parms[["gamma"]],
           Inf.days=0,
           ID.pd=case_when(
             asympt==1 ~ ifelse(parms[["test_freq_hcw"]]==1000, 1000, round(runif(length(ID), 1, parms[["test_freq_hcw"]]))) # + parms[["test_delay_hcw"]]
             ,
             asympt==0 ~ pmin(ifelse(parms[["test_freq_hcw"]]==1000, 1000, round(runif(length(ID), 1, parms[["test_freq_hcw"]]))) # + parms[["test_delay_hcw"]]
                              , parms[["id.I"]])
           ),
           ID.days = 0, 
           removal.pd = NA) -> infected
  
  infected %>%
    subset(asympt==1) %>%
    mutate(VL_rise = round(runif(sum(asympt==1), 1, 4)),
           VL = abs(rnorm(sum(asympt==1), 8, 1))/(VL_rise+1), 
           VL_waning = VL*(VL_rise+1)/runif(sum(asympt==1), 15, 21), 
           Infectiousness = case_when(VL<4 ~ 0, 
                                      VL>=4 & VL<7 ~ 0.5, 
                                      VL>=7 ~ 1)) %>%
    dplyr::select(-asympt) -> infected_asympt
  
  infected %>%
    subset(asympt==0) %>%
    mutate(VL_rise = round(runif(sum(asympt==0), 1, 4)),
           VL = abs(rnorm(sum(asympt==0), 8, 1))/(VL_rise+1), 
           VL_waning = VL*(VL_rise+1)/runif(sum(asympt==0), 15, 21), 
           Infectiousness = case_when(VL<4 ~ 0, 
                                      VL>=4 & VL<7 ~ 0.5, 
                                      VL>=7 ~ 1)) %>%
    dplyr::select(-asympt) -> infected_sympt
  
  df %>%
    subset(Days<Inc.pd)%>%
    mutate(Days=Days + 1) -> df
  
  list("infected_asympt"=infected_asympt,
       "infected_sympt"=infected_sympt,
       "df"=df)
}


infect <- function(df,parms,num.exposed){
  
  df %>%
    sample_n(num.exposed,replace=FALSE) %>%
    mutate(Inc.pd= round(runif(num.exposed, parms[["sigma1"]], parms[["sigma2"]])),
           VL= 0,
           Days=0)-> new_exposed
  
  df %>%
    subset(!(ID %in% new_exposed$ID)) -> df
  
  list("new_exposed"=new_exposed,
       "df"=df)
  
}


recover_VL <- function(df){
  df %>%
    mutate(Rec.days = Rec.days + 1,
           VL = VL - VL_waning) -> df
  
  df %>%
    mutate(VL = case_when(VL < 0 ~ 0, TRUE ~ VL)) -> df
  
  return(df)
}


move_hcw <- function(S.hcwNC, 
                     E.hcwNC, 
                     A.hcwNC,
                     I.hcwNC, 
                     R.hcwNC,
                     S.hcwC, 
                     E.hcwC, 
                     A.hcwC,
                     I.hcwC, 
                     R.hcwC,
                     prop.rNC, prop.hcwNC){
  
  SEAI.hcwNC <- c(S.hcwNC$ID,E.hcwNC$ID,A.hcwNC$ID,I.hcwNC$ID)
  SEAI.hcwC <- c(S.hcwC$ID,E.hcwC$ID,A.hcwC$ID,I.hcwC$ID)
  
  if(prop.rNC > prop.hcwNC){ # need to move hcw from C to NC 
    
    move <- round((prop.rNC - prop.hcwNC)*(length(SEAI.hcwC) + length(SEAI.hcwNC) + nrow(R.hcwNC) + nrow(R.hcwC)))
    left <- length(SEAI.hcwC) + nrow(R.hcwC) - move
    if (left==0){
      move <- move-1
    }
    
    if (move > length(SEAI.hcwC)){
      R.move <- move - length(SEAI.hcwC)
      SEAI.move <- length(SEAI.hcwC)
    } else{
      R.move <- 0
      SEAI.move <- move
    }
    
    SEAI.hcwC %>%
      as.data.frame() %>%
      setNames("ID") %>%
      sample_n(SEAI.move,replace=FALSE) -> ID_move
    
    S.hcwC %>%
      subset(ID %in% ID_move$ID) %>%
      bind_rows(S.hcwNC) -> S.hcwNC
    
    S.hcwC %>%
      subset(!(ID %in% ID_move$ID)) -> S.hcwC
    
    E.hcwC %>%
      subset(ID %in% ID_move$ID) %>%
      bind_rows(E.hcwNC) -> E.hcwNC
    
    E.hcwC %>%
      subset(!(ID %in% ID_move$ID)) -> E.hcwC
    
    A.hcwC %>%
      subset(ID %in% ID_move$ID) %>%
      bind_rows(A.hcwNC) -> A.hcwNC
    
    A.hcwC %>%
      subset(!(ID %in% ID_move$ID)) -> A.hcwC
    
    I.hcwC %>%
      subset(ID %in% ID_move$ID) %>%
      bind_rows(I.hcwNC) -> I.hcwNC
    
    I.hcwC %>%
      subset(!(ID %in% ID_move$ID)) -> I.hcwC
    
    if (R.move > 0){
      R.hcwC %>%
        sample_n(R.move,replace=FALSE) -> ID_move.R
      
      R.hcwC %>%
        subset(ID %in% ID_move.R$ID) %>%
        bind_rows(R.hcwNC) -> R.hcwNC
      
      R.hcwC %>%
        subset(!(ID %in% ID_move.R$ID)) -> R.hcwC
    }
    
    
  } else if (prop.rNC < prop.hcwNC){ # need to move hcw from NC to C
    
    move <- round((prop.hcwNC-prop.rNC)*(length(SEAI.hcwC) + length(SEAI.hcwNC) + nrow(R.hcwNC) + nrow(R.hcwC)))
    left <- length(SEAI.hcwNC) + nrow(R.hcwNC) - move
    if (left==0){
      move <- move-1
    }
    
    if (move > length(SEAI.hcwNC)){
      R.move <- move - length(SEAI.hcwNC)
      SEAI.move <- length(SEAI.hcwNC)
    } else{
      R.move <- 0
      SEAI.move <- move
    }
    
    SEAI.hcwNC %>%
      as.data.frame() %>%
      setNames("ID") %>%
      sample_n(SEAI.move,replace=FALSE) -> ID_move
    
    S.hcwNC %>%
      subset(ID %in% ID_move$ID) %>%
      bind_rows(S.hcwC) -> S.hcwC
    
    S.hcwNC %>%
      subset(!(ID %in% ID_move$ID)) -> S.hcwNC
    
    E.hcwNC %>%
      subset(ID %in% ID_move$ID) %>%
      bind_rows(E.hcwC) -> E.hcwC
    
    E.hcwNC %>%
      subset(!(ID %in% ID_move$ID)) -> E.hcwNC
    
    A.hcwNC %>%
      subset(ID %in% ID_move$ID) %>%
      bind_rows(A.hcwC) -> A.hcwC
    
    A.hcwNC %>%
      subset(!(ID %in% ID_move$ID)) -> A.hcwNC
    
    I.hcwNC %>%
      subset(ID %in% ID_move$ID) %>%
      bind_rows(I.hcwC) -> I.hcwC
    
    I.hcwNC %>%
      subset(!(ID %in% ID_move$ID)) -> I.hcwNC
    
    if (R.move > 0){
      R.hcwNC %>%
        sample_n(R.move,replace=FALSE) -> ID_move.R
      
      R.hcwNC %>%
        subset(ID %in% ID_move.R$ID) %>%
        bind_rows(R.hcwC) -> R.hcwC
      
      R.hcwNC %>%
        subset(!(ID %in% ID_move.R$ID)) -> R.hcwNC
    }
  }
  
  list(S.hcwNC, 
       E.hcwNC, 
       A.hcwNC,
       I.hcwNC, 
       R.hcwNC,
       S.hcwC, 
       E.hcwC, 
       A.hcwC,
       I.hcwC, 
       R.hcwC)
  
}

death <- function(df,mu,parms,total,t,dt){ # departure and death function
  
  #num_die <- rbinom(1,nrow(df),mu)
  
  VE_s2 = parms[["VE_s2_r"]]
  VE_i2 = parms[["VE_i2_r"]]
  VE_p2 = parms[["VE_p2_r"]]
  
  max_LOS <- max(dt) + 10
  
  df %>%
    subset(t==departure) -> discharges
  
  df %>%
    subset(!(ID %in% discharges$ID)) -> df
  
  new_dead <- data.frame()
  if (nrow(df)>0){
    for (i in 1:nrow(df)){ # loop through each person to see if they die
      death_stat <- rbinom(1,1,mu) 
      if (death_stat == 1){
        new_dead <- bind_rows(new_dead,df %>% subset(ID==i))
        
      }
    }
  }
  
  df %>%
    subset(!(ID %in% new_dead$ID)) -> df
  
  new_dead %>%
    bind_rows(discharges) -> leaving
  
  leaving %>%
    mutate(VL = NA,arrival=t,departure= t + sample(x=LOS$LOS,size=nrow(discharges)+nrow(new_dead),prob=LOS$prob,replace=TRUE),  # alternate is + max_LOS
           V_doses = sample(c(2,0),size=nrow(discharges)+nrow(new_dead),prob=c(parms[["comm.vax"]],1-parms[["comm.vax"]]),replace=TRUE),
           V_refused = NA,
           V1_t = NA,
           V2_t = NA,   
           VE_i = case_when(V_doses==2 ~ VE_i2, 
                           TRUE ~ 0),
           VE_s = case_when(V_doses==2 ~ VE_s2, 
                           TRUE ~ 0),
           VE_p = case_when(V_doses==2 ~ VE_p2, 
                           TRUE ~ 0)) %>%
    dplyr::select(ID,VL,arrival,departure,V_doses,V_refused,V1_t,V2_t,VE_i,VE_s,VE_p) -> new_entry
  
  if (nrow(new_entry)>0){
    new_entry$ID = c((total + 1):(total+nrow(new_dead) + nrow(discharges))) # had trouble doing this in the above line
  }
  
  list(df,nrow(new_dead),new_entry,leaving,nrow(discharges))
  
}

covid_death <- function(df,mu,parms,total,t,dt){ # departure and death function
  
  VE_s2 = parms[["VE_s2_r"]]
  VE_i2 = parms[["VE_i2_r"]]
  VE_p2 = parms[["VE_p2_r"]]
  
  max_LOS <- max(dt) + 10
  
  df %>%
    subset(t==departure) -> discharges
  
  df %>%
    subset(!(ID %in% discharges$ID)) -> df
  
  new_dead <- data.frame()
  if (nrow(df)>0){
    for (i in 1:nrow(df)){ # loop through each person to see if they die
      if(df$asympt[i]==1){
        mu = parms[["mu.NC"]]
      }
      death_stat <- rbinom(1,1,mu) 
      if (death_stat == 1){
        new_dead <- bind_rows(new_dead,df %>% subset(ID==i))
      }
      
    }
  }
  
  df %>%
    subset(!(ID %in% new_dead$ID)) -> df
  
  new_dead %>%
    bind_rows(discharges) -> leaving
  
  leaving %>%
    mutate(VL = NA,arrival=t,departure= t + sample(x=LOS$LOS,size=nrow(discharges)+nrow(new_dead),prob=LOS$prob,replace=TRUE), # alternate is + max_LOS
           V_doses = sample(c(2,0),size=nrow(discharges)+nrow(new_dead),prob=c(parms[["comm.vax"]],1-parms[["comm.vax"]]),replace=TRUE),
           V_refused = NA,
           V1_t = NA,
           V2_t = NA,   
           VE_i = case_when(V_doses==2 ~ VE_i2, 
                            TRUE ~ 0),
           VE_s = case_when(V_doses==2 ~ VE_s2, 
                            TRUE ~ 0),
           VE_p = case_when(V_doses==2 ~ VE_p2, 
                            TRUE ~ 0)) %>%
    dplyr::select(ID,VL,arrival,departure,V_doses,V_refused,V1_t,V2_t,VE_i,VE_s,VE_p) -> new_entry
  
  if (nrow(new_entry)>0){
    new_entry$ID = c((total + 1):(total+nrow(new_dead) + nrow(discharges))) # had trouble doing this in the above line
  }
  
  list(df,nrow(new_dead),new_entry,leaving,nrow(discharges))
  
}



get_VL <- function(list, day){
  
  VL <- NULL
  
  for(j in 1:length(list)){
    if("ID" %in% names(list[[j]])){
      
      as.data.frame(list[[j]]) %>%
        dplyr::select(ID,VL) %>%
        mutate(Sim.day=day, State=names(list)[j]) -> new_VL
      
      VL <- rbind(VL, new_VL)}
  }
  
  return(VL)
  
}

assign_rooms <- function(rooms,new_entry,R.room,int.room,t,parms){
  
  rooms %>%
    subset(!is.na(Res1) & !is.na(Res2)) -> full
  
  rooms %>%
    subset(is.na(Res1) & is.na(Res2)) -> empty
  
  rooms %>%
    subset(xor(is.na(Res1), is.na(Res2))) -> single.occupant

  if (length(new_entry) + length(R.room)>0){
  

    if (int.room == 1 & t> parms[["int_time"]]){ # put in empty rooms first, then single occupancy rooms
      
        x= 0
        y= 0
        
        check.empty <- min(length(new_entry), length(R.room), nrow(empty))
        
        if (check.empty>0){
        
          for (i in 1:min(length(new_entry), length(R.room), nrow(empty))){
            x=x+1
            y=y+1
            empty[i,2] <- new_entry[x]
            empty[i,3] <- R.room[y]
          }
          
        }
        
        full_list <- c(new_entry[x+1:length(new_entry)], R.room[y+1:length(R.room)])
        full_list <- full_list[!is.na(full_list)]
        
        z=0
        
        check.single <- min(nrow(single.occupant), length(full_list))
        
        if (check.single>0){
        
          for (i in 1:min(nrow(single.occupant), length(full_list))){
            if(is.na(single.occupant[i,2])){
              z=z+1
              single.occupant[i,2] <- full_list[i]  
            }else if(is.na(single.occupant[i,3])){
              z=z+1
              single.occupant[i,3] <- full_list[i]
            }
          }
          
          remaining <- c(full_list[z+1:length(full_list)])
          remaining <- remaining[!is.na(remaining)]
          
          if (length(remaining)>0){
          
            for (i in 1:length(remaining)){
              empty[i,3] <- remaining[i]
            }
          
          }
        
      }
        
    } else{ # fill 1 person rooms first and then empty rooms, random list of people 
      
      if (length(c(new_entry,R.room))>1){
        full_list <- sample(c(new_entry,R.room),length(c(new_entry,R.room)), replace=FALSE)
      } else{
        full_list <- c(new_entry,R.room)
      }
      
      x = 0
      check.single <- min(nrow(single.occupant), length(full_list))
      
      if (check.single>0){
      
        for(i in 1:min(length(full_list), nrow(single.occupant))){
          if(is.na(single.occupant[i,2])){
            x=x+1
            single.occupant[i,2] <- full_list[i]
          }else if(is.na(single.occupant[i,3])){
            x=x+1
            single.occupant[i,3] <- full_list[i]
          }
        }
      }
      
      remaining <- c(full_list[x+1:length(full_list)])
      remaining <- remaining[!is.na(remaining)]
      
      if (length(remaining)>0){
       
        if (length(remaining)>=2){
          remaining1 <- sample(remaining,ceiling(length(remaining)/2),replace=FALSE)
          remaining2 <- setdiff(remaining,remaining1)
          
          empty[1:length(remaining1),2] <- remaining1 
          empty[1:length(remaining2),3] <- remaining2
          
        } else{
          remaining1 <- remaining
          remaining2 <- NULL
          
          empty[1:length(remaining1),2] <- remaining1 
          
        }
      }
    }
    
    rooms <- rbind(full, empty, single.occupant)
    
    missing <- new_entry[!(new_entry %in% c(rooms$Res1,rooms$Res2))]
    
    if (length(missing)>0){
      rooms %>%
        subset(is.na(Res1) | is.na(Res2)) -> open
      
      for(i in 1:length(missing)){
        if(is.na(open[i,2])){
          open[i,2] <- missing[i]
        }else if(is.na(open[i,3])){
          open[i,3] <- missing[i]
        }
      }
      
      rooms %>%
        subset(!is.na(Res1) & !is.na(Res2)) -> full
      
      rooms <- rbind(full, open)
      
    }
  }
  
  return(rooms)
  
}

expose_roommate <- function(rooms,ID,A.rNC,I.rNC){
  
  if (nrow(A.rNC) + nrow(I.rNC) > 0){
    rooms %>%
      as.data.frame() %>%
      subset(Res1==ID | Res2==ID) -> room
    
    exp <- ifelse(room$Res1 %in% c(A.rNC$ID,I.rNC$ID),1,0)
  } else{
    exp <- 0
  }
  
  return(exp)

}



vaccinate <- function(df, parms, type, vax_count1, vax_count2, t){
  
  ## assign vaccination times 

  coverage = if_else(type=="staff", parms[["vax_coverage_hcw"]], parms[["vax_coverage_r"]])
  refusal = if_else(type=="staff", parms[["refusal_hcw"]], parms[["refusal_r"]])
  VE_s1 = if_else(type=="staff", parms[["VE_s1_hcw"]], parms[["VE_s1_r"]])
  VE_s2 = if_else(type=="staff", parms[["VE_s2_hcw"]], parms[["VE_s2_r"]])
  VE_i1 = if_else(type=="staff", parms[["VE_i1_hcw"]], parms[["VE_i1_r"]])
  VE_i2 = if_else(type=="staff", parms[["VE_i2_hcw"]], parms[["VE_i2_r"]])
  VE_p1 = if_else(type=="staff", parms[["VE_p1_hcw"]], parms[["VE_p1_r"]])
  VE_p2 = if_else(type=="staff", parms[["VE_p2_hcw"]], parms[["VE_p2_r"]])
  
  if (t %in% parms[["t_first_dose"]]){
    # choose number to vax based on coverage and availabilty (only track supply of vax_count1 against vax_available)
    n_new_vax <- floor(min(parms[["vax_available"]] - vax_count1, coverage/length(parms[["t_first_dose"]])*nrow(df %>% subset(V_doses==0))))
    
    # mark a certain percent as refusing vaccines (new entries start with NA for this)
    df %>% 
      mutate(V_refused = case_when(is.na(V_refused) ~ sample(c(1,0), nrow(df), replace=TRUE, 
                                                             prob=c(refusal, 1-refusal)),
                                   TRUE ~ V_refused)) -> df
    
    # choose which people to vaccinate
    df %>% 
      subset(V_doses==0 & V_refused==0 & is.na(V1_t)) %>% # prevent people from being assigned dates twice
      slice_sample(n=n_new_vax) -> df_vax
    
    # set their vaccination times
    df %>% 
      mutate(V1_t = case_when(ID %in% df_vax$ID ~ t,  # & t>= parms[["t_first_dose"]] removed because only doing if t %in% t_first_dose
                              TRUE ~ V1_t),
             V2_t = case_when(ID %in% df_vax$ID ~  V1_t + parms[["second_dose_delay"]], 
                              TRUE ~ V2_t)) -> df
  } else{
    df_vax <- data.frame()
  }
  
  second_dose_1 <- nrow(df %>% subset(V_doses==2))
  
  ## vaccinate based on time t, update VE incorporating vaccination delay
  df %>% 
    mutate(V_doses = case_when(t >= V1_t & t<V2_t ~ 1, 
                               t >= V2_t ~ 2, 
                               TRUE ~ V_doses),
           VE_i = case_when(V_doses==1 & t>= V1_t+parms[["immune_delay"]] ~ VE_i1,
                            V_doses==2 & t>= V2_t+parms[["immune_delay"]] ~ VE_i2, 
                            TRUE ~ VE_i),
           VE_s = case_when(V_doses==1 & t>= V1_t+parms[["immune_delay"]] ~ VE_s1,
                            V_doses==2 & t>= V2_t+parms[["immune_delay"]] ~ VE_s2, 
                            TRUE ~ VE_s),
           VE_p = case_when(V_doses==1 & t>= V1_t+parms[["immune_delay"]] ~ VE_p1,
                            V_doses==2 & t>= V2_t+parms[["immune_delay"]] ~ VE_p2, 
                            TRUE ~ VE_p)) -> df
  
  second_dose_2 <- nrow(df %>% subset(V_doses==2))
  
  ## Update vax_counts
  vax_count1 <- vax_count1 + nrow(df_vax)
  vax_count2 <- vax_count2 + (second_dose_2 - second_dose_1) # get number of new second doses
  
  list(df, vax_count1, vax_count2)
}

