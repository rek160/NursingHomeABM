recover_or_test_r <- function(df,parms, symptoms){
  
  df %>%
    subset(Inf.days==Inf.pd) %>%
    mutate(Rec.days = 0) %>%
    dplyr::select(ID,VL,VL_waning,Rec.days) -> recovered
  
  df %>%
    subset(removal.pd == ID.days) -> tested1
  
  df %>% 
    subset(!(ID %in% tested1$ID) & !(ID %in% recovered$ID)) -> df
  
  if(symptoms=="A"){    ## asymptomatics who are identified through testing
    df %>%
      subset(ID.pd==ID.days & VL>=parms["VL.threshold.r"] & is.na(removal.pd)) %>%
      mutate(removal.pd = ID.pd + parms["test_delay_r"]
             #,ID.days = ID.days + 1
             ) -> df1
    
  }else{
    
    df %>%
      subset(ID.days == parms["id.I"] | (ID.days == ID.pd & VL>=parms["VL.threshold.r"] & is.na(removal.pd))) %>%
      mutate(removal.pd = case_when(ID.days == parms["id.I"] ~ ID.days, 
                                   (ID.days == ID.pd & VL>=parms["VL.threshold.r"]) ~ ID.pd + parms["test_delay_r"]) #ID.days != parms["id.I"] &
             # ,ID.days = ID.days + 1
             ) -> df1   ##  ## symptomatics who are identified through testing OR symptoms
  }
  
  
  df %>%
    subset(ID.pd==ID.days & VL<parms["VL.threshold.r"] & is.na(removal.pd)  & !(ID %in% df1$ID)) %>%  # !(ID %in% df1$ID) excludes people identified through symptoms
    mutate(Inf.days=Inf.days + 1,
           ID.pd = ID.pd + parms["test_freq_r"],
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
    mutate(ID.days = ID.days + 1) %>%
    bind_rows(df2) %>%
    bind_rows(df3) -> df
  
  
  list("recovered"=recovered,
       "tested"=tested,
       "df"=df)
}


recover_or_test_hcw <- function(df,parms, total, symptoms){
  
  df %>%
    subset(Inf.days==Inf.pd) %>%
    mutate(Rec.days = 0) %>%
    dplyr::select(ID,VL,VL_waning,Rec.days) -> recovered
  
  df %>%
    subset(removal.pd == ID.days) -> tested1
  
  df %>% 
    subset(!(ID %in% tested1$ID) & !(ID %in% recovered$ID)) -> df
  
  
  if(symptoms=="A"){    ## asymptomatics who are identified through testing
    df %>%
      subset(ID.pd==ID.days & VL>=parms["VL.threshold.hcw"] & is.na(removal.pd)) %>%
      mutate(removal.pd = ID.pd + parms["test_delay_hcw"])  -> df1
        
             #Home.pd=parms["gamma"],
             #Home.days=0
              #%>% dplyr::select(-ID.pd,-ID.days)
    
  }else{
    
    df %>%
      subset(ID.days == parms["id.I"] | (ID.days == ID.pd & VL>=parms["VL.threshold.hcw"] & is.na(removal.pd))) %>%
      mutate(removal.pd = case_when(ID.days == parms["id.I"] ~ ID.days, 
                                    (ID.days == ID.pd & VL>=parms["VL.threshold.hcw"]) ~ ID.pd + parms["test_delay_hcw"])
             ) -> df1  ##  ## symptomatics who are identified through testing OR symptoms
  }
  
  
  df %>%
    subset(ID.pd==ID.days & VL<parms["VL.threshold.hcw"] & is.na(removal.pd) & !(ID %in% df1$ID)) %>%
    mutate(Inf.days=Inf.days + 1,
           ID.days=ID.days + 1,
           ID.pd = ID.pd +  parms["test_freq_hcw"],
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
    mutate(Home.pd=parms["gamma"],
           Home.days=0) %>% 
    dplyr::select(-ID.pd,-ID.days,-removal.pd) -> tested
  
  
  df1 %>%
    subset(ID.days != removal.pd) %>%
    mutate(ID.days = ID.days + 1) %>%
    bind_rows(df2) %>%
    bind_rows(df3) -> df
  
  
  if (nrow(tested)>0){
    new_E <- rbinom(1,nrow(tested),parms["I.C"])
    new_R <- rbinom(1,(nrow(tested)-new_E),parms["prop_rhcwR"])
    new_S <- nrow(tested) - new_E - new_R
    if (new_S >0){
      new.hcwS <- as.data.frame(cbind("ID"=10000+c((total + 1):(total + new_S)),"VL"=NA))
    } else{
      new.hcwS <-  as.data.frame(cbind("ID"=as.character(),"VL"=as.numeric()))
    }
    if (new_E >0){
      new.hcwE <- as.data.frame(cbind("ID"=10000+c((total + new_S + 1):(total+new_S + new_E)),"VL"=0,
                                    Inc.pd=round(runif(length(new_E), parms["sigma1"], parms["sigma2"])),Days=0))
    } else{
      new.hcwE <- as.data.frame(cbind("ID"=as.character(),"VL"=as.numeric()),"Inc"=as.numeric(),"Days"=as.numeric())
    }
    if (new_R >0){
      new.hcwR <- as.data.frame(cbind("ID"=10000+c((total + new_S + new_E + 1):(total+new_S + new_E + new_R)),"VL"=0, "VL_waning"=0, "Rec.days"=0)) # assume 0 VL
    } else{
      new.hcwR <-  as.data.frame(cbind("ID"=as.character(),"VL"=as.numeric(), "VL_waning"=as.numeric(),"Rec.days"=as.numeric()))
    }
  } else{
    new.hcwS <-  as.data.frame(cbind("ID"=as.character(),"VL"=as.numeric()))
    new.hcwE <- as.data.frame(cbind("ID"=as.character(),"VL"=as.numeric(),"Inc"=as.numeric(),"Days"=as.numeric()))
    new.hcwR <-  as.data.frame(cbind("ID"=as.character(),"VL"=as.numeric(),"VL_waning"=as.numeric(),"Rec.days"=as.numeric()))
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
    dplyr::select(ID,VL,VL_waning,Rec.days) -> recovered
  
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
    dplyr::select(ID,VL) %>%
    mutate(asympt = rbinom(length(ID),1,parms["alpha.r"]),
           Inf.pd=parms["gamma"],
           Inf.days=0,
           ID.pd=case_when(
             asympt==1 ~ round(runif(length(ID), 1, parms["test_freq_r"])),
             asympt==0 ~ pmin(round(runif(length(ID), 1, parms["test_freq_r"])), parms["id.I"])),
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
    subset(Days<Inc.pd) %>%
    mutate(Days=Days + 1) -> df
  
  list("infected_asympt"=infected_asympt,
       "infected_sympt"=infected_sympt,
       "df"=df)
}

E_to_I_hcw <- function(df,parms){
  
  df %>%
    subset(Days==Inc.pd) %>%
    dplyr::select(ID,VL) %>%
    mutate(asympt = rbinom(length(ID),1,parms["alpha.hcw"]),
           Inf.pd=parms["gamma"],
           Inf.days=0,
           ID.pd=case_when(
             asympt==1 ~ round(runif(length(ID), 1, parms["test_freq_hcw"])) # + parms["test_delay_hcw"]
             ,
             asympt==0 ~ pmin(round(runif(length(ID), 1, parms["test_freq_hcw"])) # + parms["test_delay_hcw"]
                              , parms["id.I"])
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
    mutate(Inc.pd= round(runif(num.exposed, parms["sigma1"], parms["sigma2"])),
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
                     prop.rNC,prop.hcwNC){
  
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
      subset(ID %in% ID_move) %>%
      bind_rows(S.hcwNC) -> S.hcwNC
    
    S.hcwC %>%
      subset(!(ID %in% ID_move)) -> S.hcwC
    
    E.hcwC %>%
      subset(ID %in% ID_move) %>%
      bind_rows(E.hcwNC) -> E.hcwNC
    
    E.hcwC %>%
      subset(!(ID %in% ID_move)) -> E.hcwC
    
    A.hcwC %>%
      subset(ID %in% ID_move) %>%
      bind_rows(A.hcwNC) -> A.hcwNC
    
    A.hcwC %>%
      subset(!(ID %in% ID_move)) -> A.hcwC
    
    I.hcwC %>%
      subset(ID %in% ID_move) %>%
      bind_rows(I.hcwNC) -> I.hcwNC
    
    I.hcwC %>%
      subset(!(ID %in% ID_move)) -> I.hcwC
    
    if (R.move > 0){
      R.hcwC %>%
        sample_n(R.move,replace=FALSE) -> ID_move.R
      
        R.hcwC %>%
          subset(ID %in% ID_move.R) %>%
          bind_rows(R.hcwNC) -> R.hcwNC
        
        R.hcwC %>%
          subset(!(ID %in% ID_move.R)) -> R.hcwC
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
      subset(ID %in% ID_move) %>%
      bind_rows(S.hcwC) -> S.hcwC
    
    S.hcwNC %>%
      subset(!(ID %in% ID_move)) -> S.hcwNC
    
    E.hcwNC %>%
      subset(ID %in% ID_move) %>%
      bind_rows(E.hcwC) -> E.hcwC
    
    E.hcwNC %>%
      subset(!(ID %in% ID_move)) -> E.hcwNC
    
    A.hcwNC %>%
      subset(ID %in% ID_move) %>%
      bind_rows(A.hcwC) -> A.hcwC
    
    A.hcwNC %>%
      subset(!(ID %in% ID_move)) -> A.hcwNC
    
    I.hcwNC %>%
      subset(ID %in% ID_move) %>%
      bind_rows(I.hcwC) -> I.hcwC
    
    I.hcwNC %>%
      subset(!(ID %in% ID_move)) -> I.hcwNC
    
    if (R.move > 0){
      R.hcwNC %>%
        sample_n(R.move,replace=FALSE) -> ID_move.R
      
      R.hcwNC %>%
        subset(ID %in% ID_move.R) %>%
        bind_rows(R.hcwC) -> R.hcwC
      
      R.hcwNC %>%
        subset(!(ID %in% ID_move.R)) -> R.hcwNC
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


death <- function(df,mu,parms,total){
  
  num_die <- rbinom(1,nrow(df),mu)
  
  df %>%
    sample_n(num_die,replace=FALSE) -> new_dead
  
  df %>%
    subset(!(ID %in% new_dead$ID)) -> df
  
  new_dead %>%
    mutate(VL = NA) %>%
    dplyr::select(ID,VL) -> new_entry
  
  if (nrow(new_entry)>0){
    new_entry$ID = c((total + 1):(total+nrow(new_dead))) # had trouble doing this in the above line
  }
  
  list(df,nrow(new_dead),new_entry,new_dead)
  
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
  

    if (int.room == 1 & t> parms["int_time"]){ # put in empty rooms first, then single occupancy rooms
      
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



