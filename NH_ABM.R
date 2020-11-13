library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyverse)

setwd('/n/home00/rkahn/NH')

source("./initialize_ABM.R")
source("./helper_functions.R")



stochastic_NH <- function(parms, Ns, delta_t, t){
  
  beta = parms["beta"]
  beta.s = parms["beta.s"]
  beta.rm = parms["beta.rm"]
  sigma1 = parms["sigma1"]
  sigma2 = parms["sigma2"]
  gamma = parms["gamma"]
  I.C = parms["I.C"]
  k.HH = parms["k.HH"]
  k.HR=parms["k.HR"]
  k.RH=parms["k.RH"]
  k.RR=parms["k.RR"]
  alpha.r= parms["alpha.r"] 
  alpha.hcw= parms["alpha.hcw"] 
  ID= parms["id.I"]        
  int.r = parms["int.r"]
  int.hcw = parms["int.hcw"]
  int.rooms = parms["int.rooms"]
  mu.C = parms["mu.C"]
  mu.NC = parms["mu.NC"]
  ppe = parms["ppe"]
  prop_rhcwR = parms["prop_rhcwR"]
  VL.threshold.r = parms["VL.threshold.r"]
  VL.threshold.hcw = parms["VL.threshold.hcw"]
  int_time = parms["int_time"]
  
  
  S.rNC=Ns[["S.rNC"]]
  E.rNC=Ns[["E.rNC"]]
  A.rNC=Ns[["A.rNC"]]
  I.rNC=Ns[["I.rNC"]]
  R.rNC=Ns[["R.rNC"]]
  I.rC=Ns[["I.rC"]]
  R.rC=Ns[["R.rC"]]
  S.hcwNC=Ns[["S.hcwNC"]]
  E.hcwNC=Ns[["E.hcwNC"]]
  A.hcwNC=Ns[["A.hcwNC"]]
  I.hcwNC=Ns[["I.hcwNC"]]
  R.hcwNC=Ns[["R.hcwNC"]]
  S.hcwC=Ns[["S.hcwC"]]
  E.hcwC=Ns[["E.hcwC"]]
  A.hcwC=Ns[["A.hcwC"]]
  I.hcwC=Ns[["I.hcwC"]]
  R.hcwC=Ns[["R.hcwC"]]
  I.hcwH=Ns[["I.hcwH"]]
  cum_inc_r=Ns[["cum_inc_r"]]
  cum_inc_hcw=Ns[["cum_inc_hcw"]]
  cum_inc_community=Ns[["cum_inc_community"]]
  total=Ns[["total"]]
  rooms=Ns[["rooms"]]
  
  N.rNC <- nrow(S.rNC) + nrow(E.rNC) + nrow(A.rNC) + nrow(I.rNC) + nrow(R.rNC)
  N.rC <- ifelse(nrow(I.rC) + nrow(R.rC) > 0, nrow(I.rC) + nrow(R.rC), 1)  
  N.rNC + N.rC
  
  N.hcwNC <-  nrow(S.hcwNC) + nrow(E.hcwNC) + nrow(A.hcwNC) + nrow(I.hcwNC) + nrow(R.hcwNC)
  N.hcwC <- nrow(S.hcwC) + nrow(E.hcwC) + nrow(A.hcwC) + nrow(I.hcwC) + nrow(R.hcwC) 
  N.hcwNC + N.hcwC
  
  
  # move residents from A/I to R and NC to C 
  recover.A.rNC <- recover_or_test_r(A.rNC,parms, "A")
  R.rNC <- rbind(R.rNC,recover.A.rNC[[1]])    # move A.rNC to R.rNC if recovered
  I.rC <- rbind(I.rC,recover.A.rNC[[2]])      # move A.rNC to I.rC if identified as positive
  A.rNC <- recover.A.rNC[[3]]                 # keep rest of A.rNC in A.rNC
  
  rooms %>%
    mutate(Res1 = case_when(Res1 %in% recover.A.rNC[[2]]$ID ~ NA_real_, TRUE~Res1),
           Res2 = case_when(Res2 %in% recover.A.rNC[[2]]$ID ~ NA_real_, TRUE~Res2)) -> rooms

  
  recover.I.rNC <- recover_or_test_r(I.rNC,parms, "I")
  R.rNC <- rbind(R.rNC,recover.I.rNC[[1]])     # move I.rNC to R.rNC if recovered
  I.rC <- rbind(I.rC,recover.I.rNC[[2]])      # move I.rNC to I.rC if identified as posiitive
  I.rNC <- recover.I.rNC[[3]]                 # keep rest of I.rNC in I.rNC
  
  rooms %>%
    mutate(Res1 = case_when(Res1 %in% recover.I.rNC[[2]]$ID ~ NA_real_, TRUE~Res1),
           Res2 = case_when(Res2 %in% recover.I.rNC[[2]]$ID ~ NA_real_, TRUE~Res2)) -> rooms
  
  recover.I.rC <- recover_I.rC(I.rC, parms)
  
  if (int.r==1 & t>int_time){
    R.rNC <- rbind(R.rNC,recover.I.rC[[1]]) # recover I.rC
    R.room <- recover.I.rC[[1]]$ID          # need room assignment
  } else {
    R.rC <- rbind(R.rC,recover.I.rC[[1]])   # recover I.rC
    R.room <- NULL
  }
  I.rC <- recover.I.rC[[2]] # keep rest in I.rC
  
  #cat("recover",(nrow(S.rNC) + nrow(E.rNC) + nrow(A.rNC) + nrow(I.rNC) + nrow(R.rNC) + nrow(I.rC) + nrow(R.rC)),t,"\n")
  
  # move hcw from A/I to R and NC or C to home
  recover.A.hcwNC <- recover_or_test_hcw(A.hcwNC,parms,total, "A")
  R.hcwNC <- rbind(R.hcwNC,recover.A.hcwNC[[1]]) # move A.hcwNC to R.hcwNC if recover
  I.hcwH <- rbind(I.hcwH,recover.A.hcwNC[[2]]) # move A.hcwNC to I.hcwH if test positive
  A.hcwNC <- recover.A.hcwNC[[3]] # keep rest of A.hcwNC in A.hcwNC
  E.hcwNC <- rbind(E.hcwNC,recover.A.hcwNC[[4]])
  S.hcwNC <- rbind(S.hcwNC,recover.A.hcwNC[[5]])
  R.hcwNC <- rbind(R.hcwNC,recover.A.hcwNC[[6]])
  total <- total + nrow(recover.A.hcwNC[[4]]) + nrow(recover.A.hcwNC[[5]]) + nrow(recover.A.hcwNC[[6]])
  
  recover.I.hcwNC <- recover_or_test_hcw(I.hcwNC,parms,total, "I")
  R.hcwNC <- rbind(R.hcwNC,recover.I.hcwNC[[1]]) # move I.hcwNC to R.hcwNC if recover
  I.hcwH <- rbind(I.hcwH,recover.I.hcwNC[[2]]) # move I.hcwNC to I.hcwH if test positive
  I.hcwNC <- recover.I.hcwNC[[3]] # keep rest of I.hcwNC in I.hcwNC
  E.hcwNC <- rbind(E.hcwNC,recover.I.hcwNC[[4]])
  S.hcwNC <- rbind(S.hcwNC,recover.I.hcwNC[[5]])
  R.hcwNC <- rbind(R.hcwNC,recover.I.hcwNC[[6]])
  total <- total + nrow(recover.I.hcwNC[[4]]) + nrow(recover.I.hcwNC[[5]]) + nrow(recover.I.hcwNC[[6]])
  
  recover.A.hcwC <- recover_or_test_hcw(A.hcwC,parms,total, "A")
  R.hcwC <- rbind(R.hcwC,recover.A.hcwC[[1]]) # move A.hcwC to R.hcwC if recover
  I.hcwH <- rbind(I.hcwH,recover.A.hcwC[[2]]) # move A.hcwC to I.hcwH if test positive
  A.hcwC <- recover.A.hcwC[[3]] # keep rest of A.hcwC in A.hcwC
  E.hcwC <- rbind(E.hcwC,recover.A.hcwC[[4]])
  S.hcwC <- rbind(S.hcwC,recover.A.hcwC[[5]])
  R.hcwC <- rbind(R.hcwC,recover.A.hcwC[[6]])
  total <- total + nrow(recover.A.hcwC[[4]]) + nrow(recover.A.hcwC[[5]]) + nrow(recover.A.hcwC[[6]]) 
  
  recover.I.hcwC <- recover_or_test_hcw(I.hcwC,parms,total, "I")
  R.hcwC <- rbind(R.hcwC,recover.I.hcwC[[1]]) # move I.hcwC to R.hcwC if recover
  I.hcwH <- rbind(I.hcwH,recover.I.hcwC[[2]]) # move I.hcwC to I.hcwH if test positive
  I.hcwC <- recover.I.hcwC[[3]] # keep rest of I.hcwC in I.hcwC
  E.hcwC <- rbind(E.hcwC,recover.I.hcwC[[4]])
  S.hcwC <- rbind(S.hcwC,recover.I.hcwC[[5]])
  R.hcwC <- rbind(R.hcwC,recover.I.hcwC[[6]])
  total <- total + nrow(recover.I.hcwC[[4]]) + nrow(recover.I.hcwC[[5]]) + nrow(recover.I.hcwC[[6]])
  
  # move infected hcw that are home back to one of Rs depending on intervention
  recover.home <- recover_home_hcw(I.hcwH)
  if (int.hcw==1 & t>int_time){
    R.hcwNC <- rbind(R.hcwNC,recover.home[[1]])
    # remove temporary hcw
    if (nrow(recover.home[[1]])>0){
      num_remove <- nrow(recover.home[[1]])
      temp <- unlist(c(S.hcwNC %>% subset(ID>10000) %>% dplyr::select(ID),
                       S.hcwC %>% subset(ID>10000) %>% dplyr::select(ID),
                       E.hcwNC %>% subset(ID>10000) %>% dplyr::select(ID),
                       E.hcwC %>% subset(ID>10000) %>% dplyr::select(ID),
                       A.hcwNC %>% subset(ID>10000) %>% dplyr::select(ID),
                       A.hcwC %>% subset(ID>10000) %>% dplyr::select(ID),
                       I.hcwNC %>% subset(ID>10000) %>% dplyr::select(ID),
                       I.hcwC %>% subset(ID>10000) %>% dplyr::select(ID),
                       R.hcwNC %>% subset(ID>10000) %>% dplyr::select(ID),
                       R.hcwC %>% subset(ID>10000) %>% dplyr::select(ID)), use.names=FALSE)
      if (length(temp)>0){
        if (length(temp)>1){
          temp_remove <- cbind("ID"=sample(temp,num_remove,replace=FALSE))
        } else{
          temp_remove <- temp
        }        
        S.hcwNC %>% 
          subset(!(ID %in% temp_remove)) -> S.hcwNC
        S.hcwC %>% 
          subset(!(ID %in% temp_remove)) -> S.hcwC
        E.hcwNC %>% 
          subset(!(ID %in% temp_remove)) -> E.hcwNC
        E.hcwC %>% 
          subset(!(ID %in% temp_remove)) -> E.hcwC
        A.hcwNC %>% 
          subset(!(ID %in% temp_remove)) -> A.hcwNC
        A.hcwC %>% 
          subset(!(ID %in% temp_remove)) -> A.hcwC
        I.hcwNC %>% 
          subset(!(ID %in% temp_remove)) -> I.hcwNC
        I.hcwC %>% 
          subset(!(ID %in% temp_remove)) -> I.hcwC
        R.hcwNC %>% 
          subset(!(ID %in% temp_remove)) -> R.hcwNC
        R.hcwC %>% 
          subset(!(ID %in% temp_remove)) -> R.hcwC
      }
    }
  } else{
    R.hcwC <- rbind(R.hcwC,recover.home[[1]])
    # remove temporary hcw
    if (nrow(recover.home[[1]])>0){
      num_remove <- nrow(recover.home[[1]]) 
      temp <- unlist(c(S.hcwNC %>% subset(ID>10000) %>% dplyr::select(ID),
                       S.hcwC %>% subset(ID>10000) %>% dplyr::select(ID),
                       E.hcwNC %>% subset(ID>10000) %>% dplyr::select(ID),
                       E.hcwC %>% subset(ID>10000) %>% dplyr::select(ID),
                       A.hcwNC %>% subset(ID>10000) %>% dplyr::select(ID),
                       A.hcwC %>% subset(ID>10000) %>% dplyr::select(ID),
                       I.hcwNC %>% subset(ID>10000) %>% dplyr::select(ID),
                       I.hcwC %>% subset(ID>10000) %>% dplyr::select(ID),
                       R.hcwNC %>% subset(ID>10000) %>% dplyr::select(ID),
                       R.hcwC %>% subset(ID>10000) %>% dplyr::select(ID)), use.names=FALSE)
      if (length(temp)>0){
        if (length(temp)>1){
          temp_remove <- cbind("ID"=sample(temp,num_remove,replace=FALSE))
        } else{
          temp_remove <- temp
        }        
        S.hcwNC %>% 
          subset(!(ID %in% temp_remove)) -> S.hcwNC
        S.hcwC %>% 
          subset(!(ID %in% temp_remove)) -> S.hcwC
        E.hcwNC %>% 
          subset(!(ID %in% temp_remove)) -> E.hcwNC
        E.hcwC %>% 
          subset(!(ID %in% temp_remove)) -> E.hcwC
        A.hcwNC %>% 
          subset(!(ID %in% temp_remove)) -> A.hcwNC
        A.hcwC %>% 
          subset(!(ID %in% temp_remove)) -> A.hcwC
        I.hcwNC %>% 
          subset(!(ID %in% temp_remove)) -> I.hcwNC
        I.hcwC %>% 
          subset(!(ID %in% temp_remove)) -> I.hcwC
        R.hcwNC %>% 
          subset(!(ID %in% temp_remove)) -> R.hcwNC
        R.hcwC %>% 
          subset(!(ID %in% temp_remove)) -> R.hcwC
      }
    }
  }
  
  #cat(nrow(recover.home[[1]]),temp,nrow(S.hcwNC),nrow(E.hcwNC),nrow(A.hcwNC),nrow(I.hcwNC),nrow(R.hcwNC),"\n")
  I.hcwH <- recover.home[[2]]
  
  # moving hcw
  prop.rNC <- (nrow(S.rNC) + nrow(E.rNC) + nrow(A.rNC) + nrow(I.rNC) + nrow(R.rNC))/
    (nrow(S.rNC) + nrow(E.rNC) + nrow(A.rNC) + nrow(I.rNC) + nrow(R.rNC) + nrow(I.rC) + nrow(R.rC))
  prop.hcwNC <- (nrow(S.hcwNC) + nrow(E.hcwNC) + nrow(A.hcwNC) +  nrow(I.hcwNC) + nrow(R.hcwNC))/
    (nrow(S.hcwNC) + nrow(E.hcwNC) + nrow(A.hcwNC) +  nrow(I.hcwNC) + nrow(R.hcwNC) + 
       nrow(S.hcwC) + nrow(E.hcwC) + nrow(A.hcwC) + nrow(I.hcwC) + nrow(R.hcwC))
  
  moved_hcw <- move_hcw(S.hcwNC, 
                        E.hcwNC, 
                        A.hcwNC,
                        I.hcwNC, 
                        R.hcwNC,
                        S.hcwC, 
                        E.hcwC, 
                        A.hcwC,
                        I.hcwC, 
                        R.hcwC,
                        prop.rNC,
                        prop.hcwNC)
  S.hcwNC <- moved_hcw[[1]]
  E.hcwNC <- moved_hcw[[2]]
  A.hcwNC <- moved_hcw[[3]]
  I.hcwNC <- moved_hcw[[4]]
  R.hcwNC <- moved_hcw[[5]]
  S.hcwC <- moved_hcw[[6]]
  E.hcwC <- moved_hcw[[7]]
  A.hcwC <- moved_hcw[[8]]
  I.hcwC <- moved_hcw[[9]]
  R.hcwC <- moved_hcw[[10]]
  
  # E -> A/I
  infect_E.rNC <- E_to_I_r(E.rNC,parms)
  A.rNC <- rbind(A.rNC,infect_E.rNC[[1]])
  I.rNC <- rbind(I.rNC,infect_E.rNC[[2]])
  E.rNC <- infect_E.rNC[[3]]
  
  #cat("infect",(nrow(S.rNC) + nrow(E.rNC) + nrow(A.rNC) + nrow(I.rNC) + nrow(R.rNC) + nrow(I.rC) + nrow(R.rC)),t,"\n")
  
  infect_E.hcwNC <- E_to_I_hcw(E.hcwNC,parms)
  A.hcwNC <- rbind(A.hcwNC,infect_E.hcwNC[[1]])
  I.hcwNC <- rbind(I.hcwNC,infect_E.hcwNC[[2]])
  E.hcwNC <- infect_E.hcwNC[[3]]
  
  infect_E.hcwC <- E_to_I_hcw(E.hcwC,parms)
  A.hcwC <- rbind(A.hcwC,infect_E.hcwC[[1]])
  I.hcwC <- rbind(I.hcwC,infect_E.hcwC[[2]])
  E.hcwC <- infect_E.hcwC[[3]]
  
  
  # S -> E
  #cat("expose",S.rNC.exposed,nrow(S.rNC),nrow(E.rNC),(nrow(S.rNC) + nrow(E.rNC) + nrow(A.rNC) + nrow(I.rNC) + nrow(R.rNC) + nrow(I.rC) + nrow(R.rC)),t,"\n")
  
  S.rNC2 <- S.rNC
  
  if(nrow(S.rNC)>=1){
    for (i in 1:nrow(S.rNC)){
      roommate.exp <- expose_roommate(rooms,S.rNC$ID[i],A.rNC,I.rNC)
      exp.prob <- beta.rm*roommate.exp +  
        beta*(ifelse(N.rNC>0,k.RR*(sum(I.rNC$Infectiousness)+sum(A.rNC$IInfectiousness))/N.rNC,0)) +
        beta.s*(ifelse(N.hcwNC>0,k.RH*(sum(I.hcwNC$Infectiousness)+sum(A.hcwNC$Infectiousness))/N.hcwNC,0))
      if (exp.prob >= 1){
        exp.prob <- 1
      } else if (exp.prob <0){
        exp.prob <- 0
      } 
      S.rNC.exposed <- rbinom(1,1,exp.prob)
      #cat(i,S.rNC.exposed,"\n")
      if (S.rNC.exposed==1){
        E.rNC <- rbind(E.rNC,cbind(ID=S.rNC$ID[i],VL=0,Inc.pd=round(runif(1, parms["sigma1"], parms["sigma2"])), Days=0))
        S.rNC2 %>%
          subset(ID != S.rNC$ID[i]) -> S.rNC2
      }
    }
  }
  
  expose.S.rNC <- nrow(S.rNC) - nrow(S.rNC2)
  S.rNC <- S.rNC2
  
  
  #cat("expose",S.rNC.exposed,nrow(S.rNC),nrow(E.rNC),(nrow(S.rNC) + nrow(E.rNC) + nrow(A.rNC) + nrow(I.rNC) + nrow(R.rNC) + nrow(I.rC) + nrow(R.rC)),t,"\n")
  
  ## community introductions
  S.hcwNC.exposed <- rbinom(1,nrow(S.hcwNC), I.C)
  expose.S.hcwNC <- infect(S.hcwNC,parms,S.hcwNC.exposed) 
  E.hcwNC <- rbind(E.hcwNC,expose.S.hcwNC[[1]])
  S.hcwNC <- expose.S.hcwNC[[2]]
  
  S.hcwC.exposed <- rbinom(1,nrow(S.hcwC), I.C)
  expose.S.hcwC <- infect(S.hcwC,parms,S.hcwC.exposed)
  E.hcwC <- rbind(E.hcwC,expose.S.hcwC[[1]])
  S.hcwC <- expose.S.hcwC[[2]]
  
  ## tally total community introductions
  inc_community <- nrow(expose.S.hcwNC[[1]]) + nrow(expose.S.hcwC[[1]])
  
  ## "local" nursing home transmission
  exp.prob <- beta*(ifelse(N.hcwNC>0,k.HH*(sum(I.hcwNC$Infectiousness)+sum(A.hcwNC$Infectiousness))/N.hcwNC,0)) + 
    beta.s*(ifelse(N.rNC>0,k.HR*(sum(I.rNC$Infectiousness)+sum(A.rNC$Infectiousness))/N.rNC,0))
  if (exp.prob > 1){
    exp.prob <- 1
  }
  S.hcwNC.exposed <- rbinom(1,nrow(S.hcwNC),exp.prob)
  expose.S.hcwNC <- infect(S.hcwNC,parms,S.hcwNC.exposed) 
  E.hcwNC <- rbind(E.hcwNC,expose.S.hcwNC[[1]])
  S.hcwNC <- expose.S.hcwNC[[2]]
  
  exp.prob <- beta*ppe*(ifelse(N.hcwC>0,k.HH*(sum(I.hcwC$Infectiousness)+sum(A.hcwC$Infectiousness))/N.hcwC,0)) + 
    beta.s*ppe*(ifelse(N.rC>0,k.HR*sum(I.rC$Infectiousness)/N.rC,0)) + I.C
  if (exp.prob > 1){
    exp.prob <- 1
  }
  S.hcwC.exposed <- rbinom(1,nrow(S.hcwC),exp.prob)
  expose.S.hcwC <- infect(S.hcwC,parms,S.hcwC.exposed)
  E.hcwC <- rbind(E.hcwC,expose.S.hcwC[[1]])
  S.hcwC <- expose.S.hcwC[[2]]
  
  
  # death
  death.S.rNC <- death(S.rNC,mu.NC,parms,total)
  S.rNC <- death.S.rNC[[1]]
  S.rNC <- rbind(S.rNC,death.S.rNC[[3]]) # new entry
  total <- total + nrow(death.S.rNC[[3]])
  
  death.E.rNC <- death(E.rNC,mu.NC,parms,total)
  E.rNC <- death.E.rNC[[1]]
  S.rNC <- rbind(S.rNC,death.E.rNC[[3]]) # new entry
  total <- total + nrow(death.E.rNC[[3]])
  
  death.A.rNC <- death(A.rNC,mu.C,parms,total)
  A.rNC <- death.A.rNC[[1]]
  S.rNC <- rbind(S.rNC,death.A.rNC[[3]]) # new entry
  total <- total + nrow(death.A.rNC[[3]])
  
  death.I.rNC <- death(I.rNC,mu.C,parms,total)
  I.rNC <- death.I.rNC[[1]]
  S.rNC <- rbind(S.rNC,death.I.rNC[[3]]) # new entry
  total <- total + nrow(death.I.rNC[[3]])
  
  death.R.rNC <- death(R.rNC,mu.NC,parms,total)
  R.rNC <- death.R.rNC[[1]]
  S.rNC <- rbind(S.rNC,death.R.rNC[[3]]) # new entry
  total <- total + nrow(death.R.rNC[[3]])
  
  death.I.rC <- death(I.rC,mu.C,parms,total)
  I.rC <- death.I.rC[[1]]
  S.rNC <- rbind(S.rNC,death.I.rC[[3]]) # new entry
  total <- total + nrow(death.I.rC[[3]])
  
  death.R.rC <- death(R.rC,mu.NC,parms,total)
  R.rC <- death.R.rC[[1]]
  S.rNC <- rbind(S.rNC,death.R.rC[[3]]) # new entry
  total <- total + nrow(death.R.rC[[3]])
  
  # update rooms 
  new_entry <- c(death.S.rNC[[3]]$ID,death.E.rNC[[3]]$ID,death.A.rNC[[3]]$ID,death.I.rNC[[3]]$ID,death.R.rNC[[3]]$ID,
                 death.I.rC[[3]]$ID,death.R.rC[[3]]$ID)
  
  new_dead <- c(death.S.rNC[[4]]$ID,death.E.rNC[[4]]$ID,death.A.rNC[[4]]$ID,death.I.rNC[[4]]$ID,death.R.rNC[[4]]$ID,
                death.I.rC[[4]]$ID,death.R.rC[[4]]$ID)
  
  rooms %>%
    mutate(Res1 = case_when(Res1 %in% new_dead ~ NA_real_, TRUE~Res1),
           Res2 = case_when(Res2 %in% new_dead ~ NA_real_, TRUE~Res2)) -> rooms
  
  if (length(c(new_entry,R.room))>0){
    rooms <- assign_rooms(rooms,new_entry,R.room,int.rooms,t, parms)
  }
  
  #cat(new_entry,new_entry %in% rooms$Res1 | new_entry %in% rooms$Res2,"\n")
  
  #cat("death",(nrow(S.rNC) + nrow(E.rNC) + nrow(A.rNC) + nrow(I.rNC) + nrow(R.rNC) + nrow(I.rC) + nrow(R.rC)),t,"\n")
  
  # change VL for recovered
  R.rNC <- recover_VL(R.rNC)
  R.hcwNC <- recover_VL(R.hcwNC)
  R.rC <- recover_VL(R.rC)
  R.hcwC <- recover_VL(R.hcwC)
  
  
  mortality <- death.S.rNC[[2]] + death.E.rNC[[2]] + death.A.rNC[[2]] + death.I.rNC[[2]] + death.R.rNC[[2]] + 
    death.I.rC[[2]] + death.R.rC[[2]]
  
  inc_r <- expose.S.rNC
  inc_hcw <- nrow(expose.S.hcwC[[1]]) + nrow(expose.S.hcwNC[[1]])
  
  cum_inc_r = (cum_inc_r + inc_r)
  cum_inc_hcw = (cum_inc_hcw + inc_hcw)
  cum_inc_community = (cum_inc_community + inc_community)
  
  
  final <- list("S.rNC"=S.rNC,
                "E.rNC"=E.rNC,
                "A.rNC"=A.rNC,
                "I.rNC"=I.rNC,
                "R.rNC"=R.rNC,
                "I.rC"=I.rC, 
                "R.rC"=R.rC,
                "S.hcwNC"=S.hcwNC, 
                "E.hcwNC"=E.hcwNC, 
                "A.hcwNC"=A.hcwNC,
                "I.hcwNC"=I.hcwNC, 
                "R.hcwNC"=R.hcwNC,
                "S.hcwC"=S.hcwC, 
                "E.hcwC"=E.hcwC, 
                "A.hcwC"=A.hcwC,
                "I.hcwC"=I.hcwC, 
                "R.hcwC"=R.hcwC,
                "I.hcwH"=I.hcwH,
                "inc_r"=inc_r,
                "inc_hcw"=inc_hcw,
                "cum_inc_r"=cum_inc_r,
                "cum_inc_hcw"=cum_inc_hcw,
                "cum_inc_community"=cum_inc_community,
                "mortality"=mortality,
                "total"=total,
                "rooms"=rooms)
  
  return(final)
  
}

inits=c(S.rNC.init = 99,
        E.rNC.init = 1,
        A.rNC.init = 0,
        I.rNC.init = 0,
        R.rNC.init = 0,
        I.rC.init  = 0,
        R.rC.init = 0,
        S.hcwNC.init = 99,
        E.hcwNC.init = 0,
        A.hcwNC.init = 0,
        I.hcwNC.init = 0,
        R.hcwNC.init = 0,
        S.hcwC.init = 1,
        E.hcwC.init = 0,
        A.hcwC.init = 0,
        I.hcwC.init = 0,
        R.hcwC.init = 0,
        I.hcwH.init = 0,
        inc_r=0,
        inc_hcw=0,
        cum_inc_r=0,
        cum_inc_hcw=0,
        cum_inc_community=0,
        mortality=0,
        total=0)

args=(commandArgs(TRUE))

for (i in 1:length(args)) {
  eval (parse (text = args[[i]] ))
}

nsim <- 1
j <- j

### run simulations ----
parms <- c(beta=0.02,    ## beta
           beta.s=(1-(1-0.02)**3), ## beta for interactions between staff and residents
           beta.rm=(1-(1-0.02)**10),  ## beta for roommates
           sigma1=1.5,      ## incubation period shortest
           sigma2=4.49,      ## incubation period longest
           gamma=14,     ## infectious period 
           I.C=0.005,    ## probability of infection from community
           k.HH=2,       ## n contacts between staff
           k.RH=6,       ## n staff contacted by each resident, per day
           k.HR=6,       ## n residents contacted by each staff member, per day
           k.RR=0,       ## n daily contacts between residents besides roommates
           alpha.hcw = 0.4, # proportion hcw asymptomatic
           alpha.r = 0.2,   # proportion residents asymptomatic
           id.I= 2,         # duration of pre-symptomatic transmission
           ppe=0.05,        # reduction in beta with ppe
           prop_rhcwR=0.2,    # probability replacement hcw recovered
           mu.C=0.02,       # COVID mortality
           mu.NC=0.001,     # regular mortality
           int.r=1,         # 1 if go to new_recovered.rNC, 0 if go to new_recovered.rC
           int.rooms=1,     # if 1 put susceptible and recovered together in NC
           int.hcw=1,       # 1 if recovered work with NC and 0 if work with C
           VL.threshold.r=0,  # threshold for detectable VL (put arbitrary # in for now)
           VL.threshold.hcw=0,
           test_freq_hcw=7, # staff testing frequency (days) 
           test_freq_r=7,   # resident testing frequency (days) 
           test_delay_hcw=2, # staff test delay (days) 
           test_delay_r=2,   # resident test delay (days)
           int_time=21
)

t_step <- 1
dt <- seq(0,200,t_step)


## loop over testing strategies

testing_scenarios <- as.data.frame(cbind(c("PCR", "PCR", "Antigen","PCR","PCR","Antigen","Antigen","PCR","PCR","None",
                                           "PCR","Antigen","Antigen","None","None","None","None"), 
                                         c("PCR", "Antigen", "Antigen","PCR","PCR","Antigen","Antigen","PCR","PCR", "None",
                                           "PCR","Antigen", "Antigen","Antigen", "Antigen","Antigen", "Antigen")))
rownames(testing_scenarios) <- c("weekly_PCR", "daily_staff_antigen", "daily_antigen","weekly_PCR_slow","twice_weekly_PCR",
                                 "weekly_antigen","antigen_highLOD","daily_PCR", "fast_PCR","none",
                                 "2 day both PCR","2 day both","3 day both", 
                                 "1 day staff", "2 day staff","3 day staff", "7 day staff")
colnames(testing_scenarios) <- c("Res", "HCW")
testing_scenarios$LOD.r <- c(3,3,5,3,3,5,7,3,3,1000,3,5,5,1000,1000,1000,1000)
testing_scenarios$freq.r <- c(7,7,1,7,3,7,1,1,7,1000,2,2,3,1000,1000,1000,1000)
testing_scenarios$delay.r <- c(2,2,0,7,2,0,0,2,1,1000,2,0,0,1000,1000,1000,1000)
testing_scenarios$LOD.hcw <- c(3,5,5,3,3,5,7,3,3,1000,3,5,5,5,5,5,5)
testing_scenarios$freq.hcw <- c(7,1,1,7,3,7,1,1,7,1000,2,2,3,1,2,3,7)
testing_scenarios$delay.hcw <- c(2,0,0,7,2,0,0,2,1,1000,2,0,0,0,0,0,0)



# PCR <- c(0, 7, 2)
# PCR_slow <- c(0, 7, 7)
# Antigen <- c(5, 1, 0)
# 
# names(PCR) <- c("LOD", "freq", "delay")
# names(Antigen) <- names(PCR)

beta = parms["beta"]
beta.s = parms["beta.s"]
beta.rm = parms["beta.rm"]
sigma1 = parms["sigma1"]
sigma2 = parms["sigma2"]
gamma = parms["gamma"]
I.C = parms["I.C"]
k.HH = parms["k.HH"]
k.HR=parms["k.HR"]
k.RH=parms["k.RH"]
k.RR=parms["k.RR"]
alpha.r= parms["alpha.r"] 
alpha.hcw= parms["alpha.hcw"] 
id.I= parms["id.I"]        
int.r = parms["int.r"]
int.hcw = parms["int.hcw"]
int.rooms = parms["int.rooms"]
mu.C = parms["mu.C"]
mu.NC = parms["mu.NC"]
ppe = parms["ppe"]
prop_rhcwR = parms["prop_rhcwR"]
VL.threshold.r = parms["VL.threshold.r"]
VL.threshold.hcw = parms["VL.threshold.hcw"]
int_time = parms["int_time"]


res_master <- NULL
VL_master <- NULL

for(s in seq_along(1:nrow(testing_scenarios))){
  
  test_scenario <- rownames(testing_scenarios)[s]
  
  parms[["VL.threshold.r"]] <-  testing_scenarios[test_scenario,"LOD.r"]
  parms[["test_freq_r"]] <-  testing_scenarios[test_scenario,"freq.r"]
  parms[["test_delay_r"]] <-  testing_scenarios[test_scenario,"delay.r"]
  
  parms[["VL.threshold.hcw"]] <- testing_scenarios[test_scenario,"LOD.hcw"]
  parms[["test_freq_hcw"]] <- testing_scenarios[test_scenario,"freq.hcw"]
  parms[["test_delay_hcw"]] <- testing_scenarios[test_scenario,"delay.hcw"]
  
  parms[["int.r"]] <- 0
  parms[["int.rooms"]] <- 0
  parms[["int.hcw"]] <- 0
  Intervention <- "A) No intervention"
  
  for (sim in 1:nsim){
    cat(sim,s,"\n")
    Ns <- initialize(inits,parms)
    res <- as.data.frame(matrix(nrow=length(dt),ncol=length(Ns)))
    VLs <- NULL    
    bug_staff <- NULL
    bug_resident <- NULL
    bug_r <-0
    bug_s <- 0
    for(i in 1:length(dt)){
      debug <- Ns
      final <- stochastic_NH(parms,Ns,t_step,(i-1)*t_step)
      Ns <- final
      res[i,] <- c(i, nrow(Ns[["S.rNC"]]), nrow(Ns[["E.rNC"]]), nrow(Ns[["A.rNC"]]), nrow(Ns[["I.rNC"]]), nrow(Ns[["R.rNC"]]), 
                   nrow(Ns[["I.rC"]]), nrow(Ns[["R.rC"]]), 
                   nrow(Ns[["S.hcwNC"]]), nrow(Ns[["E.hcwNC"]]), nrow(Ns[["A.hcwNC"]]), nrow(Ns[["I.hcwNC"]]), nrow(Ns[["R.hcwNC"]]),  
                   nrow(Ns[["S.hcwC"]]), nrow(Ns[["E.hcwC"]]), nrow(Ns[["A.hcwC"]]), nrow(Ns[["I.hcwC"]]), nrow(Ns[["R.hcwC"]]), 
                   nrow(Ns[["I.hcwH"]]), Ns[["inc_r"]],  Ns[["inc_hcw"]], Ns[["cum_inc_r"]], Ns[["cum_inc_hcw"]], Ns[["cum_inc_community"]],
                   Ns[["mortality"]],Ns[["total"]])
      staff <- sum(res[i,c(9:18)])
      residents <- sum(res[i,2:8])
      if (staff!=100 & bug_s==0){
        bug_staff <- debug
        cat("staff",i,staff,"\n")
        bug_s <- 1
      }
      if (residents!=100 & bug_r==0){
        bug_resident <- debug
        cat("residents",i,residents,"\n")
        bug_r <- 1
      }
      VLs <- rbind(VLs, get_VL(final, i))
    }
    
    res %>%
      add_column("Sim"=j) %>%
      add_column("Intervention"=Intervention) %>%
      add_column("Testing"=test_scenario) %>%
      bind_rows(res_master) -> res_master
    
    VLs %>%
      add_column("Sim"=j) %>%
      add_column("Intervention"=Intervention) %>%
      add_column("Testing"=test_scenario) %>%
      bind_rows(VL_master) -> VL_master
    
    write.csv(res_master, paste0(Sys.Date(),j,"_",k.HH,"_",k.RH,"_",k.HR,"_",k.RR,"_",I.C,"_",ppe,"_res_master_simulations.csv"))
    #write.csv(VL_master, paste0(Sys.Date(),j,"_VL_master_simulations.csv"))
    
  }
  
  
  parms[["int.r"]] <- 1
  parms[["int.rooms"]] <- 1
  parms[["int.hcw"]] <- 0
  Intervention <- "B) Resident intervention"
  
  for (sim in 1:nsim){
    cat(sim,"\n")
    Ns <- initialize(inits,parms)
    res <- as.data.frame(matrix(nrow=length(dt),ncol=length(Ns)))
    VLs <- NULL
    for(i in 1:length(dt)){
      final <- stochastic_NH(parms,Ns,t_step,(i-1)*t_step)
      Ns <- final
      res[i,] <- c(i, nrow(Ns[["S.rNC"]]), nrow(Ns[["E.rNC"]]), nrow(Ns[["A.rNC"]]), nrow(Ns[["I.rNC"]]), nrow(Ns[["R.rNC"]]), 
                   nrow(Ns[["I.rC"]]), nrow(Ns[["R.rC"]]), 
                   nrow(Ns[["S.hcwNC"]]), nrow(Ns[["E.hcwNC"]]), nrow(Ns[["A.hcwNC"]]), nrow(Ns[["I.hcwNC"]]), nrow(Ns[["R.hcwNC"]]),  
                   nrow(Ns[["S.hcwC"]]), nrow(Ns[["E.hcwC"]]), nrow(Ns[["A.hcwC"]]), nrow(Ns[["I.hcwC"]]), nrow(Ns[["R.hcwC"]]), 
                   nrow(Ns[["I.hcwH"]]), Ns[["inc_r"]],  Ns[["inc_hcw"]], Ns[["cum_inc_r"]], Ns[["cum_inc_hcw"]], Ns[["cum_inc_community"]],
                   Ns[["mortality"]],Ns[["total"]])
      VLs <- rbind(VLs, get_VL(final, i))
    }
    
    res %>%
      add_column("Sim"=j) %>%
      add_column("Intervention"=Intervention) %>%
      add_column("Testing"=test_scenario) %>%
      bind_rows(res_master) -> res_master
    
    VLs %>%
      add_column("Sim"=j) %>%
      add_column("Intervention"=Intervention) %>%
      add_column("Testing"=test_scenario) %>%
      bind_rows(VL_master) -> VL_master
    
    write.csv(res_master, paste0(Sys.Date(),j,"_",k.HH,"_",k.RH,"_",k.HR,"_",k.RR,"_",I.C,"_",ppe,"_res_master_simulations.csv"))
    #write.csv(VL_master, paste0(Sys.Date(),j,"_VL_master_simulations.csv"))
    
  }
  
  
  parms[["int.r"]] <- 0
  parms[["int.rooms"]] <- 0
  parms[["int.hcw"]] <- 1
  Intervention <- "C) HCW intervention"
  
  for (sim in 1:nsim){
    cat(sim,"\n")
    Ns <- initialize(inits,parms)
    res <- as.data.frame(matrix(nrow=length(dt),ncol=length(Ns)))
    VLs <- NULL
    for(i in 1:length(dt)){
      final <- stochastic_NH(parms,Ns,t_step,(i-1)*t_step)
      Ns <- final
      res[i,] <- c(i, nrow(Ns[["S.rNC"]]), nrow(Ns[["E.rNC"]]), nrow(Ns[["A.rNC"]]), nrow(Ns[["I.rNC"]]), nrow(Ns[["R.rNC"]]), 
                   nrow(Ns[["I.rC"]]), nrow(Ns[["R.rC"]]), 
                   nrow(Ns[["S.hcwNC"]]), nrow(Ns[["E.hcwNC"]]), nrow(Ns[["A.hcwNC"]]), nrow(Ns[["I.hcwNC"]]), nrow(Ns[["R.hcwNC"]]),  
                   nrow(Ns[["S.hcwC"]]), nrow(Ns[["E.hcwC"]]), nrow(Ns[["A.hcwC"]]), nrow(Ns[["I.hcwC"]]), nrow(Ns[["R.hcwC"]]), 
                   nrow(Ns[["I.hcwH"]]), Ns[["inc_r"]],  Ns[["inc_hcw"]], Ns[["cum_inc_r"]], Ns[["cum_inc_hcw"]], Ns[["cum_inc_community"]],
                   Ns[["mortality"]], Ns[["total"]])
      VLs <- rbind(VLs, get_VL(final, i))
    }
    
    res %>%
      add_column("Sim"=j) %>%
      add_column("Intervention"=Intervention) %>%
      add_column("Testing"=test_scenario) %>%
      bind_rows(res_master) -> res_master
    
    VLs %>%
      add_column("Sim"=j) %>%
      add_column("Intervention"=Intervention) %>%
      add_column("Testing"=test_scenario) %>%
      bind_rows(VL_master) -> VL_master
    
    write.csv(res_master, paste0(Sys.Date(),j,"_",k.HH,"_",k.RH,"_",k.HR,"_",k.RR,"_",I.C,"_",ppe,"_res_master_simulations.csv"))
    #write.csv(VL_master, paste0(Sys.Date(),j,"_VL_master_simulations.csv"))
    
  }
  

  
  parms[["int.r"]] <- 1
  parms[["int.rooms"]] <- 1
  parms[["int.hcw"]] <- 1
  Intervention <- "D) Both interventions"
  
  for (sim in 1:nsim){
    cat(sim,"\n")
    Ns <- initialize(inits,parms)
    res <- as.data.frame(matrix(nrow=length(dt),ncol=length(Ns)))
    VLs <- NULL
    for(i in 1:length(dt)){
      final <- stochastic_NH(parms,Ns,t_step,(i-1)*t_step)
      Ns <- final
      res[i,] <- c(i, nrow(Ns[["S.rNC"]]), nrow(Ns[["E.rNC"]]), nrow(Ns[["A.rNC"]]), nrow(Ns[["I.rNC"]]), nrow(Ns[["R.rNC"]]), 
                   nrow(Ns[["I.rC"]]), nrow(Ns[["R.rC"]]), 
                   nrow(Ns[["S.hcwNC"]]), nrow(Ns[["E.hcwNC"]]), nrow(Ns[["A.hcwNC"]]), nrow(Ns[["I.hcwNC"]]), nrow(Ns[["R.hcwNC"]]),  
                   nrow(Ns[["S.hcwC"]]), nrow(Ns[["E.hcwC"]]), nrow(Ns[["A.hcwC"]]), nrow(Ns[["I.hcwC"]]), nrow(Ns[["R.hcwC"]]), 
                   nrow(Ns[["I.hcwH"]]), Ns[["inc_r"]],  Ns[["inc_hcw"]], Ns[["cum_inc_r"]], Ns[["cum_inc_hcw"]], Ns[["cum_inc_community"]],
                   Ns[["mortality"]],Ns[["total"]])
      VLs <- rbind(VLs, get_VL(final, i))
    }
    
    res %>%
      add_column("Sim"=j) %>%
      add_column("Intervention"=Intervention) %>%
      add_column("Testing"=test_scenario) %>%
      bind_rows(res_master) -> res_master
    
    VLs %>%
      add_column("Sim"=j) %>%
      add_column("Intervention"=Intervention) %>%
      add_column("Testing"=test_scenario) %>%
      bind_rows(VL_master) -> VL_master
    
    write.csv(res_master, paste0(Sys.Date(),j,"_",k.HH,"_",k.RH,"_",k.HR,"_",k.RR,"_",I.C,"_",ppe,"_res_master_simulations.csv"))
    #write.csv(VL_master, paste0(Sys.Date(),j,"_VL_master_simulations.csv"))
    
  }
}

colnames(res_master) <- c("time", "S.rNC", "E.rNC", "A.rNC", "I.rNC", "R.rNC", 
                          "I.rC", "R.rC", 
                          "S.hcwNC", "E.hcwNC", "A.hcwNC", "I.hcwNC", "R.hcwNC",  
                          "S.hcwC", "E.hcwC", "A.hcwC", "I.hcwC", "R.hcwC", "I.hcwH", 
                          "inc_r", "inc_hcw", "cum_inc_r", "cum_inc_hcw", "cum_inc_community", "mortality","total","Sim", 
                          "Intervention", "Testing")


write.csv(res_master, paste0(Sys.Date(),j,"_",k.HH,"_",k.RH,"_",k.HR,"_",k.RR,"_",I.C,"_",ppe,"_res_master_simulations.csv"))
write_rds(VL_master, paste0(Sys.Date(),j,"_VL_master_simulations.rds"))

# ### VL ----
# 
# VL_master %>%
#   ggplot()+
#   geom_line(aes(x=Sim.day, y=VL, group=interaction(ID, Sim), color=as.factor(Sim)))+
#   facet_wrap(vars(Intervention))+
#   scale_x_continuous(limits = c(0,50)) +
#   labs(color="Simulation #")
# 
# 
# VL_master %>%
#   subset(State %in% c("I.rNC", "R.rNC",
#                       "I.rC", "R.rC",
#                       "A.hcwNC", "I.hcwNC", "R.hcwNC",
#                       "A.hcwC", "I.hcwC", "R.hcwC")) %>%
#   group_by(Sim,Sim.day,Intervention) %>%
#   summarise(median=quantile(VL)[3],
#             lower=quantile(VL)[2],
#             upper=quantile(VL)[4]) -> VL_summary
# 
# ggplot() +
#   geom_point(data=VL_summary,aes(x=Sim.day, y=median))+
#   geom_errorbar(data=VL_summary,aes(x=Sim.day, ymin=lower, ymax=upper))+
#   geom_line(data=res_master,aes(x=time,y=(inc_r+inc_hcw)),color="red")+
#   facet_wrap(vars(Sim,Intervention)) +
#   labs(x="Day",y="Viral load (median and IQR)") -> VL_lines_inc
# 
# VL_lines_inc
# 
# ggsave(paste0(Sys.Date(),"VLplots.png"),VL_lines_inc, width=8, height=6, units="in")
# 
# 
# VL_master %>%
#   subset(Sim==2 & VL!=0) %>%
#   ggplot() + geom_histogram(aes(VL)) +
#   facet_wrap(vars(Sim.day,Intervention)) -> VL_bars_HCWint

#
# ggsave("VL_lines_HCWint.png",VLlines_HCWint)
# ggsave("VL_bars_HCWint.png",VL_bars_HCWint)


# 
# 
# ######## plots -----
# 
# 
# ## plot summary values (median) across simulations
# 
# res_master %>%
#   group_by(time, Intervention, Testing) %>%
#   summarise(med.r.S = median((S.rNC)),
#             med.hcw.S = median((S.hcwNC+S.hcwC)),
#             med.r.E = median(E.rNC),
#             med.hcw.E = median((E.hcwNC+E.hcwC)),
#             med.r.R = median((R.rC+R.rNC)),
#             med.hcw.R = median((R.hcwNC+R.hcwC)),
#             med.cum.inc.r = median(cum_inc_r),
#             med.cum.inc.hcw = median(cum_inc_hcw),
#             med.cum.inc.community = median(cum_inc_community)) -> summary
# 
# 
# summary %>%
#   filter(Testing =="daily") %>%
#   ggplot()+
#   geom_line(aes(x=time, y=med.r.S),col="black",lty=1)+
#   geom_line(aes(x=time, y=med.hcw.S),col="black",lty=4)+
#   geom_line(aes(x=time, y=med.r.E),col="red",lty=1)+
#   geom_line(aes(x=time, y=med.hcw.E),col="red",lty=4)+
#   geom_line(aes(x=time, y=med.r.R),col="blue",lty=1)+
#   geom_line(aes(x=time, y=med.hcw.R),col="blue",lty=4)+
#   geom_line(aes(x=time, y=med.cum.inc.community),col="green",lty=4)+
#   facet_wrap(vars(Intervention),nrow = 2) +
#   scale_y_continuous(name="people")+
#   theme_bw() +
#   #labs(caption = "Solid: residents, Dashed: healthcare workers") +
#   theme(legend.position="none")  -> plot_C
# 
# plot_C
# 
# ggsave(paste0(Sys.Date(),"daily-staff-testing.png"),plot)
# 
# summary_plots<- ggarrange(plot_A, plot_B, plot_C, nrow=1, ncol=3)
# ggsave(paste0(Sys.Date(),"plot_100.pdf"),summary_plots,width=12)
# 
# 
# ## plot individual simulations to see spread
# 
# res_master %>%
#   group_by(Sim, Intervention, time) %>%
#   ggplot()+
#   geom_line(aes(x=time, y=S.rNC, group=Sim),col="black",lty=1, lwd=0.2)+
#   geom_line(aes(x=time, y=S.hcwNC+S.hcwC, group=Sim),col="black",lty=4, lwd=0.2)+
#   geom_line(aes(x=time, y=E.rNC, group=Sim),col="red",lty=1, lwd=0.2)+
#   geom_line(aes(x=time, y=E.hcwNC+E.hcwC, group=Sim),col="red",lty=4, lwd=0.2)+
#   geom_line(aes(x=time, y=R.rC+R.rNC, group=Sim),col="blue",lty=1, lwd=0.2)+
#   geom_line(aes(x=time, y=R.hcwNC+R.hcwC, group=Sim),col="blue",lty=4, lwd=0.2)+
# 
#   geom_line(data=summary, aes(x=time, y=med.r.S),col="black",lty=1, lwd=0.8)+
#   geom_line(data=summary, aes(x=time, y=med.hcw.S),col="black",lty=4, lwd=0.8)+
#   geom_line(data=summary, aes(x=time, y=med.r.E),col="red",lty=1, lwd=0.8)+
#   geom_line(data=summary, aes(x=time, y=med.hcw.E),col="red",lty=4, lwd=0.8)+
#   geom_line(data=summary, aes(x=time, y=med.r.R),col="blue",lty=1, lwd=0.8)+
#   geom_line(data=summary, aes(x=time, y=med.hcw.R),col="blue",lty=4, lwd=0.8)+
#   geom_line(data=summary, aes(x=time, y=med.hcw.R),col="blue",lty=4, lwd=0.8)+
# 
#   scale_y_continuous(name="people")+
# 
#   facet_wrap(vars(Intervention),nrow = 2)+
#   theme_bw() +
#   theme(legend.position="none") +
#   labs(caption = "Solid: residents, Dashed: healthcare workers;  black: susceptible, red: infected, blue: recovered") -> sim_plot
# 
# sim_plot
# 
# ggsave(paste0(Sys.Date(),"dailysimplots_100.png"),sim_plot)
# 
# 
# 
# 
# final_size_plot <- res_master %>%
#   ggplot()+
#   #geom_line(aes(x=time, y=cum_inc_r, group=interaction(Sim, Intervention), color=Intervention), lty=1, lwd=0.2)+
#   #geom_line(aes(x=time, y=cum_inc_hcw, group=interaction(Sim, Intervention), color=Intervention), lty=4, lwd=0.2)+
#   geom_line(data=summary, aes(x=time, y=med.cum.inc.r, color=Intervention), lty=1, lwd=0.8)+
#   geom_line(data=summary, aes(x=time, y=med.cum.inc.hcw, color=Intervention), lty=4, lwd=0.8)+
#   geom_line(data=summary, aes(x=time, y=med.cum.inc.community, color=Intervention),lty=3, lwd=0.8)+
#   scale_y_continuous(name="people", limits=c(0,125))+
#   facet_wrap(vars(factor(Testing, levels = c("weekly", "daily_staff", "daily"))),nrow = 1) +
#   theme_bw()+
#   labs(caption = "Solid: residents, Dashed: healthcare workers, Dotted: community introductions")
# 
# final_size_plot
# ggsave(paste0(Sys.Date(),"finalsize_100.pdf"), final_size_plot)
# 
# 
# 
# final_size_bar <- summary %>%
#   filter(time==201) %>%
#   rename(Resident=med.cum.inc.r, HCW = med.cum.inc.hcw, Community = med.cum.inc.community) %>%
#   select(Intervention, Testing, Community, HCW, Resident) %>%
#   gather(Population, Final_size, Community:Resident) %>%
#   filter(Population!="Community") %>%
#   ggplot()+
#   geom_col(aes(x=factor(Testing, levels = c("weekly", "daily_staff", "daily")), y=Final_size,
#                fill=Intervention, alpha=factor(Population, levels = c("Resident", "HCW"))), position="dodge", color="grey")+
#   scale_alpha_discrete(range=c(1, 0.5), name="Population")+
#   ylab("Cumulative incidence")+
#   xlab("testing strategy")+
#   theme_bw()
# 
# final_size_bar
# ggsave(paste0(Sys.Date(),"finalsize_bar_100.pdf"), final_size_bar)
# 
# 
# #ggsave("both_intervention.png",both_intervention)
# # master <- rbind(master,cbind(res_master,"Intervention" = rep(Intervention,nrow(res_master))))
# # summary_master <- rbind(summary_master,cbind(summary,"Intervention" = rep(Intervention,nrow(summary))))
# # 
# 
# # res_intervention <- res_intervention + labs(caption = "Solid: residents, Dashed: healthcare workers",
# #                         title = "Resident intervention")
# # 
# # summary_plots <- ggarrange(no_intervention,res_intervention,hcw_intervention,both_intervention,nrow=2,ncol=2)
# # ggsave("summary_plots.png",summary_plots)
# # 
# # ggplot(res_master)+
# #   geom_line(aes(x=time, y=(inc),lty=factor(Sim)), col="purple")+
# #   geom_line(aes(x=time, y=(mortality),lty=factor(Sim)))+
# #   scale_y_continuous(name="people")+
# #   theme_bw() +
# #   theme(legend.position="none") +
# #   labs(caption = "Purple: incidence, Black: mortality")
# # 
# # 
# # #### older plots----
# # 
# # master %>%
# #   filter(Intervention=="both") %>%
# #   ggplot() +
# #   geom_line(aes(x=time, y=(S.rNC+S.hcwNC+S.hcwC),lty=factor(Sim)))+
# #   #geom_line(aes(x=time, y=(A.rNC+I.rNC+I.rC)), col="orange")+
# #   geom_line(aes(x=time, y=(E.rNC),lty=factor(Sim)), col="blue")+
# #   geom_line(aes(x=time, y=(E.hcwNC+E.hcwC),lty=factor(Sim)), col="red")+
# #   #geom_line(aes(x=time, y=(I.hcwNC+I.hcwC+A.hcwNC+A.hcwC)), col="green")+
# #   #geom_line(aes(x=time, y=(I.hcwH)), col="purple")+
# #   geom_line(aes(x=time, y=(R.hcwNC+R.hcwC+R.rC+R.rNC),lty=factor(Sim)))+
# #   scale_y_continuous(name="people")+
# #   theme_bw() +
# #   theme(legend.position="none") +
# #   labs(caption = "Red: infected HCW, Blue: infected residents") -> both
# # 
# # ggarrange(hcw,both,nrow=1)


       