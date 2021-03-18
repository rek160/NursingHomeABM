#read in length of stay data
LOS <- read.csv('NH_LOS_MDS_2016.csv')

LOS %>%
  subset(LOS >=7) %>%
  mutate(prob = Freq/sum(Freq)) -> LOS # setting a minimum of a week stay - may need to adjust


#require(splitstackshape)

# LOS %>%
#   expandRows("Freq") -> LOS_expand
# 
# summary(LOS_expand)
# 
# ggplot(LOS) + geom_line(aes(x=LOS,y=Freq)) + xlim(0,300)


initialize <- function(inits,params,dt){
  
  # max_LOS <- max(dt) + 10
  # 
  # LOS <- data.frame("LOS"=c(seq(1,30,1),max_LOS))
  # 
  # LOS %>%
  #   mutate(prob = c(rep(1/60,30),0.5)) -> LOS
  
  S.rNC.init = inits["S.rNC.init"]
  E.rNC.init = inits["E.rNC.init"]
  A.rNC.init = inits["A.rNC.init"]
  I.rNC.init = inits["I.rNC.init"]
  R.rNC.init = inits["R.rNC.init"]
  I.rC.init  = inits["I.rC.init"]
  R.rC.init = inits["R.rC.init"]
  S.hcwNC.init = inits["S.hcwNC.init"]
  E.hcwNC.init = inits["E.hcwNC.init"]
  A.hcwNC.init = inits["A.hcwNC.init"]
  I.hcwNC.init = inits["I.hcwNC.init"]
  R.hcwNC.init = inits["R.hcwNC.init"]
  S.hcwC.init = inits["S.hcwC.init"]
  E.hcwC.init = inits["E.hcwC.init"]
  A.hcwC.init = inits["A.hcwC.init"]
  I.hcwC.init = inits["I.hcwC.init"]
  R.hcwC.init = inits["R.hcwC.init"]
  I.hcwH.init = inits["I.hcwH.init"]
  
  total <- 0
  if (S.rNC.init > 0){
    S.rNC=as.data.frame(cbind("ID"=1:S.rNC.init,"VL"=rep(NA,S.rNC.init), 
                              "arrival" = rep(0,S.rNC.init), 
                              "departure"= 0 + sample(x=LOS$LOS,size=S.rNC.init,prob=LOS$prob,replace=TRUE),   
                              "V_doses" = rep(0,S.rNC.init),
                              "V_refused" = rep(NA,S.rNC.init),  
                              "V1_t"=rep(NA,S.rNC.init),         ## need to assign this in vaccine function
                              "V2_t"=rep(NA,S.rNC.init),         ## need to assign this in vaccination function                    
                              "VE_i"=rep(0,S.rNC.init),            # infectiousness
                              "VE_s"=rep(0,S.rNC.init),            # susceptibility (i.e. becoming infected)
                              "VE_p"=rep(0,S.rNC.init)            # Progression to symptoms
    )) # can adjust if want to vary this
    total <- total + S.rNC.init
  } else{
    S.rNC=as.data.frame(cbind("ID"=as.numeric(),"VL"=as.numeric(), 
                              "arrival" = as.numeric(), "departure"= as.numeric(),  
                              "V_doses" = as.numeric(), "V_refused" = as.numeric(),  
                              "V1_t"=as.numeric(), "V2_t"=as.numeric(),                        
                              "VE_i"=as.numeric(), "VE_s"=as.numeric(), "VE_p"=as.numeric())) 
  }
  
  if (E.rNC.init > 0){
    E.rNC=as.data.frame(cbind("ID"=(total+1):(total + E.rNC.init),"VL"=rep(0,E.rNC.init), 
                              "arrival" = rep(0,E.rNC.init), "departure"= 0 + sample(x=LOS$LOS,size=E.rNC.init,prob=LOS$prob,replace=TRUE),   
                              "V_doses" = rep(0,E.rNC.init), "V_refused" = rep(NA,E.rNC.init),  
                              "V1_t"=rep(NA,E.rNC.init), "V2_t"=rep(NA,E.rNC.init),                          
                              "VE_i"=rep(0,E.rNC.init), "VE_s"=rep(0,E.rNC.init), "VE_p"=rep(0,E.rNC.init),
                              "Inc.pd"=round(runif(E.rNC.init, parms[["sigma1"]], parms[["sigma2"]])),"Days"=rep(0,E.rNC.init)))
    total <- total + E.rNC.init
  } else{
    E.rNC=as.data.frame(cbind("ID"=as.numeric(),"VL"=as.numeric(),
                              "arrival" = as.numeric(), "departure"= as.numeric(),  
                              "V_doses" = as.numeric(), "V_refused" = as.numeric(),  
                              "V1_t"=as.numeric(), "V2_t"=as.numeric(),                        
                              "VE_i"=as.numeric(), "VE_s"=as.numeric(), "VE_p"=as.numeric(),
                              "Inc.pd"=as.numeric(),"Days"=as.numeric())) 
  }
  
  if (A.rNC.init > 0){
    A.rNC=as.data.frame(cbind("ID"=(total+1):(total+ A.rNC.init), "VL_rise" = round(runif(A.rNC.init, 1, 4)),
                              "removal.pd"=rep(NA, A.rNC.init),
                              "VL"=abs(rnorm(A.rNC.init, 8, 1)*0.5), "VL_waning"=rep(8/runif(1, 15, 21),A.rNC.init), 
                              "arrival" = rep(0,A.rNC.init), "departure"= 0 + sample(x=LOS$LOS,size=A.rNC.init,prob=LOS$prob,replace=TRUE),   
                              "V_doses" = rep(0,A.rNC.init), "V_refused" = rep(NA,A.rNC.init),  
                              "V1_t"=rep(NA,A.rNC.init), "V2_t"=rep(NA,A.rNC.init),                          
                              "VE_i"=rep(0,A.rNC.init), "VE_s"=rep(0,A.rNC.init), "VE_p"=rep(0,A.rNC.init), 
                              "Inf.pd"=rep(parms[["gamma"]],A.rNC.init),"Inf.days"=rep(0,A.rNC.init), # can change if we want to assume they are partway into inf period
                              "ID.pd"=round(runif(A.rNC.init, 1, parms[["test_freq_r"]])),"ID.days"=rep(0,A.rNC.init),
                              "Infectiousness"=rep(1,A.rNC.init)))
    total <- total + A.rNC.init
  } else{
    A.rNC=as.data.frame(cbind("ID"=as.numeric(),"VL_rise"=as.numeric(), 
                              "removal.pd"=as.numeric(), "VL"=as.numeric(), "VL_waning"=as.numeric(),
                              "arrival" = as.numeric(), "departure"= as.numeric(),  
                              "V_doses" = as.numeric(), "V_refused" = as.numeric(),  
                              "V1_t"=as.numeric(), "V2_t"=as.numeric(),                        
                              "VE_i"=as.numeric(), "VE_s"=as.numeric(), "VE_p"=as.numeric(),
                              "Inf.pd"=as.numeric(),"Inf.days"=as.numeric(),
                              "ID.pd"=as.numeric(),"ID.days"=as.numeric(),
                              "Infectiousness"=as.numeric())) 
  }
  
  if (I.rNC.init > 0){
    I.rNC=as.data.frame(cbind("ID"=(total+1):(total+ I.rNC.init), "VL_rise" = round(runif(I.rNC.init, 1, 4)),
                              "removal.pd"=rep(NA, I.rNC.init),
                              "VL"=abs(rnorm(I.rNC.init, 8, 1)*0.5), "VL_waning"=rep(8/runif(1, 15, 21),I.rNC.init),
                              "arrival" = rep(0,I.rNC.init), "departure"= 0 + sample(x=LOS$LOS,size=I.rNC.init,prob=LOS$prob,replace=TRUE),   
                              "V_doses" = rep(0,I.rNC.init), "V_refused" = rep(NA,I.rNC.init),  
                              "V1_t"=rep(NA,I.rNC.init), "V2_t"=rep(NA,I.rNC.init),                          
                              "VE_i"=rep(0,I.rNC.init), "VE_s"=rep(0,I.rNC.init), "VE_p"=rep(0,I.rNC.init), 
                              "Inf.pd"=rep(parms[["gamma"]],I.rNC.init),"Inf.days"=rep(0,I.rNC.init), # can change if we want to assume they are partway into inf period
                              "ID.pd"=min(round(runif(I.rNC.init, 1, parms[["test_freq_r"]])), parms[["id.I"]]),
                              "ID.days"=rep(0,I.rNC.init),
                              "Infectiousness"=rep(1,I.rNC.init)))
    total <- total + I.rNC.init
  } else{
    I.rNC=as.data.frame(cbind("ID"=as.numeric(),"VL_rise"=as.numeric(), 
                              "removal.pd"=as.numeric(), "VL"=as.numeric(), "VL_waning"=as.numeric(), 
                              "arrival" = as.numeric(), "departure"= as.numeric(),  
                              "V_doses" = as.numeric(), "V_refused" = as.numeric(),  
                              "V1_t"=as.numeric(), "V2_t"=as.numeric(),                        
                              "VE_i"=as.numeric(), "VE_s"=as.numeric(), "VE_p"=as.numeric(),
                              "Inf.pd"=as.numeric(),"Inf.days"=as.numeric(),
                              "ID.pd"=as.numeric(),"ID.days"=as.numeric(),
                              "Infectiousness"=as.numeric()))
  }
  
  if (R.rNC.init > 0){
    R.rNC=as.data.frame(cbind("ID"=(total+1):(total + R.rNC.init),"VL"=rep(NA,R.rNC.init), "VL_waning"=rep(NA,R.rNC.init),
                              "arrival" = rep(0,R.rNC.init), "departure"= 0 + sample(x=LOS$LOS,size=R.rNC.init,prob=LOS$prob,replace=TRUE),   
                              "V_doses" = rep(0,R.rNC.init), "V_refused" = rep(NA,R.rNC.init),  
                              "V1_t"=rep(NA,R.rNC.init), "V2_t"=rep(NA,R.rNC.init),                          
                              "VE_i"=rep(0,R.rNC.init), "VE_s"=rep(0,R.rNC.init), "VE_p"=rep(0,R.rNC.init),
                              "Rec.days"=rep(0,R.rNC.init)))
    total <- total + R.rNC.init
  } else{
    R.rNC=as.data.frame(cbind("ID"=as.numeric(),"VL"=as.numeric(), "VL_waning"=as.numeric(), 
                              "arrival" = as.numeric(), "departure"= as.numeric(),  
                              "V_doses" = as.numeric(), "V_refused" = as.numeric(),  
                              "V1_t"=as.numeric(), "V2_t"=as.numeric(),                        
                              "VE_i"=as.numeric(), "VE_s"=as.numeric(), "VE_p"=as.numeric(),
                              "Rec.days"=as.numeric()))
  }
  
  if (I.rC.init > 0){
    I.rC=as.data.frame(cbind("ID"=(total+1):(total+ I.rC.init),"VL_rise" = round(runif(I.rC.init, 1, 4)),
                             "removal.pd"=rep(NA, I.rC.init),
                             "VL"=abs(rnorm(I.rC.init, 8, 1)*0.5), "VL_waning"=rep(8/runif(1, 15, 21),I.rC.init),
                             "arrival" = rep(0,I.rC.init), "departure"= 0 + sample(x=LOS$LOS,size=I.rC.init,prob=LOS$prob,replace=TRUE),   
                             "V_doses" = rep(0,I.rC.init), "V_refused" = rep(NA,I.rC.init),  
                             "V1_t"=rep(NA,I.rC.init), "V2_t"=rep(NA,I.rC.init),                          
                             "VE_i"=rep(0,I.rC.init), "VE_s"=rep(0,I.rC.init), "VE_p"=rep(0,I.rC.init), 
                             "Inf.pd"=rep(parms[["gamma"]],I.rC.init),"Inf.days"=rep(0,I.rC.init),
                             "ID.pd"=rep(NA,I.rC.init),"ID.days"=rep(NA,I.rC.init), # can change if we want to assume they are partway into inf period
                             "Infectiousness"=rep(1,I.rC.init), "asympt"=rbinom(I.rC.init, 1, parms[["alpha.r"]])))
    total <- total + I.rC.init
  } else{
    I.rC=as.data.frame(cbind("ID"=as.numeric(),"VL_rise" = as.numeric(),
                             "removal.pd"=as.numeric(),"VL"=as.numeric(), "VL_waning"=as.numeric(), 
                             "arrival" = as.numeric(), "departure"= as.numeric(),  
                             "V_doses" = as.numeric(), "V_refused" = as.numeric(),  
                             "V1_t"=as.numeric(), "V2_t"=as.numeric(),                        
                             "VE_i"=as.numeric(), "VE_s"=as.numeric(), "VE_p"=as.numeric(),
                             "Inf.pd"=as.numeric(),"Inf.days"=as.numeric(),
                             "ID.pd"=as.numeric(),"ID.days"=as.numeric(),
                             "Infectiousness"=as.numeric(), "asympt"=as.numeric())) 
    
  }
  
  if (R.rC.init > 0){
    R.rC=as.data.frame(cbind("ID"=(total+1):(total + R.rC.init),"VL"=rep(0,R.rC.init), "VL_waning"=rep(0,R.rC.init), 
                             "arrival" = rep(0,R.rC.init), "departure"= 0 + sample(x=LOS$LOS,size=R.rC.init,prob=LOS$prob,replace=TRUE),   
                             "V_doses" = rep(0,R.rC.init), "V_refused" = rep(NA,R.rC.init),  
                             "V1_t"=rep(NA,R.rC.init), "V2_t"=rep(NA,R.rC.init),                          
                             "VE_i"=rep(0,R.rC.init), "VE_s"=rep(0,R.rC.init), "VE_p"=rep(0,R.rC.init), 
                             "Rec.days"=rep(0,R.rC.init)))
    total <- total + R.rC.init
  } else{
    R.rC=as.data.frame(cbind("ID"=as.numeric(),"VL"=as.numeric(),"VL_waning"=as.numeric(), 
                             "arrival" = as.numeric(), "departure"= as.numeric(),  
                             "V_doses" = as.numeric(), "V_refused" = as.numeric(),  
                             "V1_t"=as.numeric(), "V2_t"=as.numeric(),                        
                             "VE_i"=as.numeric(), "VE_s"=as.numeric(), "VE_p"=as.numeric(),"Rec.days"=as.numeric())) 
  }
  
  if (S.hcwNC.init > 0){
    S.hcwNC=as.data.frame(cbind("ID"=(total+1):(total + S.hcwNC.init),"VL"=rep(NA,S.hcwNC.init), 
                                "V_doses" = rep(0,S.hcwNC.init), "V_refused" = rep(NA,S.hcwNC.init),  
                                "V1_t"=rep(NA,S.hcwNC.init), "V2_t"=rep(NA,S.hcwNC.init),                          
                                "VE_i"=rep(0,S.hcwNC.init), "VE_s"=rep(0,S.hcwNC.init), "VE_p"=rep(0,S.hcwNC.init))) # can adjust if want to vary this
    total <- total + S.hcwNC.init
  } else{
    S.hcwNC=as.data.frame(cbind("ID"=as.numeric(),"VL"=as.numeric(),  ## this used to say infectiousness
                                "V_doses" = as.numeric(), "V_refused" = as.numeric(),  
                                "V1_t"=as.numeric(), "V2_t"=as.numeric(),                        
                                "VE_i"=as.numeric(), "VE_s"=as.numeric(), "VE_p"=as.numeric()))
  }
  
  if (E.hcwNC.init > 0){
    E.hcwNC=as.data.frame(cbind("ID"=(total+1):(total + E.hcwNC.init),"VL"=rep(0,E.hcwNC.init),
                                "V_doses" = rep(0,E.hcwNC.init), "V_refused" = rep(NA,E.hcwNC.init),  
                                "V1_t"=rep(NA,E.hcwNC.init), "V2_t"=rep(NA,E.hcwNC.init),                          
                                "VE_i"=rep(0,E.hcwNC.init), "VE_s"=rep(0,E.hcwNC.init), "VE_p"=rep(0,E.hcwNC.init),
                                "Inc.pd"=round(runif(E.hcwNC.init, parms[["sigma1"]], parms[["sigma2"]])),"Days"=rep(0,E.hcwNC.init)))
    total <- total + E.hcwNC.init
  } else{
    E.hcwNC=as.data.frame(cbind("ID"=as.numeric(),"VL"=as.numeric(),
                                "V_doses" = as.numeric(), "V_refused" = as.numeric(),  
                                "V1_t"=as.numeric(), "V2_t"=as.numeric(),                        
                                "VE_i"=as.numeric(), "VE_s"=as.numeric(), "VE_p"=as.numeric(), 
                                "Inc.pd"=as.numeric(),"Days"=as.numeric())) 
  }
  
  if (A.hcwNC.init > 0){
    A.hcwNC=as.data.frame(cbind("ID"=(total+1):(total+ A.hcwNC.init),"VL_rise" = round(runif(A.hcwNC.init, 1, 4)),
                                "removal.pd"=rep(NA, A.hcwNC.init),
                                "VL"=abs(rnorm(A.hcwNC.init, 8, 1)*0.5), "VL_waning"=rep(8/runif(1, 15, 21),A.hcwNC.init),
                                "V_doses" = rep(0,A.hcwNC.init), "V_refused" = rep(NA,A.hcwNC.init),  
                                "V1_t"=rep(NA,A.hcwNC.init), "V2_t"=rep(NA,A.hcwNC.init),                          
                                "VE_i"=rep(0,A.hcwNC.init), "VE_s"=rep(0,A.hcwNC.init), "VE_p"=rep(0,A.hcwNC.init),
                                "Inf.pd"=rep(parms[["gamma"]],A.hcwNC.init),"Inf.days"=rep(0,A.hcwNC.init), # can change if we want to assume they are partway into inf period
                                "ID.pd"=round(runif(A.hcwNC.init, 1, parms[["test_freq_hcw"]])),
                                "ID.days"=rep(0,A.hcwNC.init),
                                "Infectiousness"=rep(1,A.hcwNC.init)))
    total <- total + A.hcwNC.init
  } else{
    A.hcwNC=as.data.frame(cbind("ID"=as.numeric(),"VL_rise" = as.numeric(),
                                "removal.pd"=as.numeric(),"VL"=as.numeric(), "VL_waning"=as.numeric(),
                                "V_doses" = as.numeric(), "V_refused" = as.numeric(),  
                                "V1_t"=as.numeric(), "V2_t"=as.numeric(),                        
                                "VE_i"=as.numeric(), "VE_s"=as.numeric(), "VE_p"=as.numeric(), 
                                "Inf.pd"=as.numeric(),"Inf.days"=as.numeric(),
                                "ID.pd"=as.numeric(),"ID.days"=as.numeric(),
                                "Infectiousness"=as.numeric())) 
  }
  
  if (I.hcwNC.init > 0){
    I.hcwNC=as.data.frame(cbind("ID"=(total+1):(total+ I.hcwNC.init),"VL_rise" = round(runif(I.hcwNC.init, 1, 4)),
                                "removal.pd"=rep(NA, I.hcwNC.init),"VL"=abs(rnorm(I.hcwNC.init, 8, 1)*0.5), "VL_waning"=rep(8/runif(1, 15, 21),I.hcwNC.init),
                                "V_doses" = rep(0,I.hcwNC.init), "V_refused" = rep(NA,I.hcwNC.init),  
                                "V1_t"=rep(NA,I.hcwNC.init), "V2_t"=rep(NA,I.hcwNC.init),                          
                                "VE_i"=rep(0,I.hcwNC.init), "VE_s"=rep(0,I.hcwNC.init), "VE_p"=rep(0,I.hcwNC.init),
                                "Inf.pd"=rep(parms[["gamma"]],I.hcwNC.init),"Inf.days"=rep(0,I.hcwNC.init), # can change if we want to assume they are partway into inf period
                                "ID.pd"= min(round(runif(I.hcwNC.init, 1, parms[["test_freq_hcw"]])), parms[["id.I"]]),
                                "ID.days"=rep(0,I.hcwNC.init),
                                "Infectiousness"=rep(1,I.hcwNC.init)))
    
    total <- total + I.hcwNC.init
  } else{
    I.hcwNC=as.data.frame(cbind("ID"=as.numeric(),"VL_rise" = as.numeric(),
                                "removal.pd"=as.numeric(),"VL"=as.numeric(), "VL_waning"=as.numeric(),
                                "V_doses" = as.numeric(), "V_refused" = as.numeric(),  
                                "V1_t"=as.numeric(), "V2_t"=as.numeric(),                        
                                "VE_i"=as.numeric(), "VE_s"=as.numeric(), "VE_p"=as.numeric(),
                                "Inf.pd"=as.numeric(),"Inf.days"=as.numeric(),
                                "ID.pd"=as.numeric(),"ID.days"=as.numeric(),
                                "Infectiousness"=as.numeric())) 
  }
  
  if (R.hcwNC.init > 0){
    R.hcwNC=as.data.frame(cbind("ID"=(total+1):(total + R.hcwNC.init),"VL"=rep(0,R.hcwNC.init), "VL_waning"=rep(0,R.hcwNC.init),
                                "V_doses" = rep(0,R.hcwNC.init), "V_refused" = rep(NA,R.hcwNC.init),  
                                "V1_t"=rep(NA,R.hcwNC.init), "V2_t"=rep(NA,R.hcwNC.init),                          
                                "VE_i"=rep(0,R.hcwNC.init), "VE_s"=rep(0,R.hcwNC.init), "VE_p"=rep(0,R.hcwNC.init),
                                "Rec.days"=rep(0,R.hcwNC.init)))
    total <- total + R.hcwNC.init
  } else{
    R.hcwNC=as.data.frame(cbind("ID"=as.numeric(),"VL"=as.numeric(),"VL_waning"=as.numeric(),
                                "V_doses" = as.numeric(), "V_refused" = as.numeric(),  
                                "V1_t"=as.numeric(), "V2_t"=as.numeric(),                        
                                "VE_i"=as.numeric(), "VE_s"=as.numeric(), "VE_p"=as.numeric(), "Rec.days"=as.numeric())) 
  }
  
  if (S.hcwC.init > 0){
    S.hcwC=as.data.frame(cbind("ID"=(total+1):(total + S.hcwC.init),"VL"=rep(NA,S.hcwC.init), 
                               "V_doses" = rep(0,S.hcwC.init), "V_refused" = rep(NA,S.hcwC.init),  
                               "V1_t"=rep(NA,S.hcwC.init), "V2_t"=rep(NA,S.hcwC.init),                          
                               "VE_i"=rep(0,S.hcwC.init), "VE_s"=rep(0,S.hcwC.init), "VE_p"=rep(0,S.hcwC.init))) # can adjust if want to vary this
    total <- total + S.hcwC.init
  } else{
    S.hcwC=as.data.frame(cbind("ID"=as.numeric(),"VL"=as.numeric(), 
                               "V_doses" = as.numeric(), "V_refused" = as.numeric(),  
                               "V1_t"=as.numeric(), "V2_t"=as.numeric(),                        
                               "VE_i"=as.numeric(), "VE_s"=as.numeric(), "VE_p"=as.numeric())) 
  }
  
  if (E.hcwC.init > 0){
    E.hcwC=as.data.frame(cbind("ID"=(total+1):(total + E.hcwC.init),"VL"=rep(0,E.hcwC.init), 
                               "V_doses" = rep(0,E.hcwC.init), "V_refused" = rep(NA,E.hcwC.init),  
                               "V1_t"=rep(NA,E.hcwC.init), "V2_t"=rep(NA,E.hcwC.init),                          
                               "VE_i"=rep(0,E.hcwC.init), "VE_s"=rep(0,E.hcwC.init), "VE_p"=rep(0,E.hcwC.init),
                               "Inc.pd"=round(runif(E.hcwC.init, parms[["sigma1"]], parms[["sigma2"]])),"Days"=rep(0,E.hcwC.init)))
    total <- total + E.hcwC.init
  } else{
    E.hcwC=as.data.frame(cbind("ID"=as.numeric(),"VL"=as.numeric(),
                               "V_doses" = as.numeric(), "V_refused" = as.numeric(),  
                               "V1_t"=as.numeric(), "V2_t"=as.numeric(),                        
                               "VE_i"=as.numeric(), "VE_s"=as.numeric(), "VE_p"=as.numeric(), 
                               "Inc.pd"=as.numeric(),"Days"=as.numeric())) 
  }
  
  if (A.hcwC.init > 0){
    A.hcwC=as.data.frame(cbind("ID"=(total+1):(total+ A.hcwC.init), "VL_rise" = round(runif(A.hcwC.init, 1, 4)),
                               "removal.pd"=rep(NA, A.hcwC.init),"VL"=abs(rnorm(A.hcwC.init, 8, 1)*0.5), "VL_waning"=rep(8/runif(1, 15, 21),A.hcwC.init),
                               "V_doses" = rep(0,A.hcwC.init), "V_refused" = rep(NA,A.hcwC.init),  
                               "V1_t"=rep(NA,A.hcwC.init), "V2_t"=rep(NA,A.hcwC.init),                          
                               "VE_i"=rep(0,A.hcwC.init), "VE_s"=rep(0,A.hcwC.init), "VE_p"=rep(0,A.hcwC.init),
                               "Inf.pd"=rep(parms[["gamma"]],A.hcwC.init),"Inf.days"=rep(0,A.hcwC.init), # can change if we want to assume they are partway into inf period
                               "ID.pd"=round(runif(A.hcwC.init, 1, parms[["test_freq_hcw"]])),
                               "ID.days"=rep(0,A.hcwC.init),
                               "Infectiousness"=rep(1,A.hcwC.init)))
    
    total <- total + A.hcwC.init
  } else{
    A.hcwC=as.data.frame(cbind("ID"=as.numeric(),"VL_rise" = as.numeric(),
                               "removal.pd"=as.numeric(),"VL"=as.numeric(), "VL_waning"=as.numeric(),
                               "V_doses" = as.numeric(), "V_refused" = as.numeric(),  
                               "V1_t"=as.numeric(), "V2_t"=as.numeric(),                        
                               "VE_i"=as.numeric(), "VE_s"=as.numeric(), "VE_p"=as.numeric(), 
                               "Inf.pd"=as.numeric(),"Inf.days"=as.numeric(),
                               "ID.pd"=as.numeric(),"ID.days"=as.numeric(),
                               "Infectiousness"=as.numeric())) 
  }
  
  if (I.hcwC.init > 0){
    I.hcwC=as.data.frame(cbind("ID"=(total+1):(total+ I.hcwC.init), "VL_rise" = round(runif(I.hcwC.init, 1, 4)),
                               "removal.pd"=rep(NA, I.hcwC.init),"VL"=abs(rnorm(I.hcwC.init, 8, 1)*0.5), "VL_waning"=rep(8/runif(1, 15, 21),I.hcwC.init),
                               "V_doses" = rep(0,I.hcwC.init), "V_refused" = rep(NA,I.hcwC.init),  
                               "V1_t"=rep(NA,I.hcwC.init), "V2_t"=rep(NA,I.hcwC.init),                          
                               "VE_i"=rep(0,I.hcwC.init), "VE_s"=rep(0,I.hcwC.init), "VE_p"=rep(0,I.hcwC.init),
                               "Inf.pd"=rep(parms[["gamma"]],I.hcwC.init),"Inf.days"=rep(0,I.hcwC.init), # can change if we want to assume they are partway into inf period
                               "ID.pd"=min(round(runif(I.hcwC.init, 1, parms[["test_freq_hcw"]])), parms[["id.I"]]),
                               "ID.days"=rep(0,I.hcwC.init),
                               "Infectiousness"=rep(1,I.hcwC.init)))
    
    total <- total + I.hcwC.init
  } else{
    I.hcwC=as.data.frame(cbind("ID"=as.numeric(),"VL_rise" = as.numeric(),"removal.pd"=as.numeric(),
                               "VL"=as.numeric(), "VL_waning"=as.numeric(),
                               "V_doses" = as.numeric(), "V_refused" = as.numeric(),  
                               "V1_t"=as.numeric(), "V2_t"=as.numeric(),                        
                               "VE_i"=as.numeric(), "VE_s"=as.numeric(), "VE_p"=as.numeric(), 
                               "Inf.pd"=as.numeric(),"Inf.days"=as.numeric(),
                               "ID.pd"=as.numeric(),"ID.days"=as.numeric(),
                               "Infectiousness"=as.numeric())) 
  }
  
  if (R.hcwC.init > 0){
    R.hcwC=as.data.frame(cbind("ID"=(total+1):(total + R.hcwC.init),"VL"=rep(0,R.hcwC.init),"VL_waning"=rep(0,R.hcwC.init),
                               "V_doses" = rep(0,R.hcwC.init), "V_refused" = rep(NA,R.hcwC.init),  
                               "V1_t"=rep(NA,R.hcwC.init), "V2_t"=rep(NA,R.hcwC.init),                          
                               "VE_i"=rep(0,R.hcwC.init), "VE_s"=rep(0,R.hcwC.init), "VE_p"=rep(0,R.hcwC.init),
                               "Rec.days"=rep(0,R.hcwC.init)))
    total <- total + R.hcwC.init
  } else{
    R.hcwC=as.data.frame(cbind("ID"=as.numeric(),"VL"=as.numeric(),"VL_waning"=as.numeric(), 
                               "V_doses" = as.numeric(), "V_refused" = as.numeric(),  
                               "V1_t"=as.numeric(), "V2_t"=as.numeric(),                        
                               "VE_i"=as.numeric(), "VE_s"=as.numeric(), "VE_p"=as.numeric(), 
                               "Rec.days"=as.numeric()))
  }
  
  if (I.hcwH.init > 0){
    I.hcwH=as.data.frame(cbind("ID"=(total+1):(total+ I.hcwH.init), "VL_rise" = round(runif(I.hcwH.init, 1, 4)),
                               "VL"=abs(rnorm(I.hcwH.init, 8, 1)*0.5), "VL_waning"=rep(8/runif(1, 15, 21),I.hcwH.init),
                               "V_doses" = rep(0,I.hcwH.init), "V_refused" = rep(NA,I.hcwH.init),  
                               "V1_t"=rep(NA,I.hcwH.init), "V2_t"=rep(NA,I.hcwH.init),                          
                               "VE_i"=rep(0,I.hcwH.init), "VE_s"=rep(0,I.hcwH.init), "VE_p"=rep(0,I.hcwH.init),
                               "Inf.pd"=rep(parms[["gamma"]],I.hcwH.init),"Inf.days"=rep(0,I.hcwH.init),
                               "Home.pd"=rep(parms[["gamma"]],I.hcwH.init),"Home.days"=rep(0,I.hcwH.init), # can change if we want to assume they are partway into inf period
                               "Infectiousness"=rep(1,I.hcwC.init)))
    
    total <- total + I.hcwC.init
  } else{
    I.hcwH=as.data.frame(cbind("ID"=as.numeric(),"VL_rise" = as.numeric(),"VL"=as.numeric(), "VL_waning"=as.numeric(),
                               "V_doses" = as.numeric(), "V_refused" = as.numeric(),  
                               "V1_t"=as.numeric(), "V2_t"=as.numeric(),                        
                               "VE_i"=as.numeric(), "VE_s"=as.numeric(), "VE_p"=as.numeric(), 
                               "Inf.pd"=as.numeric(),"Inf.days"=as.numeric(),
                               "Home.pd"=as.numeric(),"Home.days"=as.numeric(),
                               "Infectiousness"=as.numeric())) 
  }
  
  rNC <- c(S.rNC$ID, E.rNC$ID, A.rNC$ID, I.rNC$ID,R.rNC$ID)
  
  # create list of empty
  rooms <- as.data.frame(cbind("Room"=seq(1,ceiling(length(rNC)/2),1),"Res1"=NA,"Res2"=NA))
  
  ## then randomly assign people to each room (list entry)
  rooms$Res1[1:ceiling(length(rNC)/2)] <- sample(rNC,ceiling(length(rNC)/2),replace=FALSE)
  rooms$Res2[1:floor(length(rNC)/2)] <- sample(setdiff(rNC,rooms$Res1))
  
  #room.list <- as.list(as.data.frame(rbind(rooms$Res1, rooms$Res2)))
  #names(room.list) <- c(1:ceiling(length(rNC)/2))
  
  Ns <- list("S.rNC"=S.rNC,
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
             "inc_r"=0,
             "inc_hcw"=0,
             "cum_inc_r"=0,
             "cum_inc_hcw"=0,
             "cum_inc_vax1"=0,
             "cum_inc_vax2"=0,
             "cum_inc_community"=0,
             "mortality"=0,
             "discharges"=0,
             "total"=total,
             "rooms"=rooms, 
             "vax_count1"=0,
             "vax_count2"=0, 
             "asymptomatic_inc_r"=0,
             "symptomatic_inc_r"=0,
             "asymptomatic_inc_hcw"=0,
             "symptomatic_inc_hcw"=0)
  
  return(Ns)
}
