library(tidyverse)
VL <- read_rds('./2020-09-2873_VL_master_simulations.rds')

VL %>% 
  subset(State %in% c("A.hcwC", "A.hcwNC", "A.rNC", "I.hcwC", "I.hcwNC", "I.rC", "I.rNC" )) %>%
  subset(Testing=="none" & Intervention=="A) No intervention") %>%
  mutate(Infectiousness = case_when(VL<4 ~ 0, 
                                    VL>=4 & VL<7 ~ 0.5, 
                                    VL>=7 ~ 1)) %>%
  group_by(ID,Testing,Intervention) %>%
  summarise(I_days = sum(Infectiousness)) -> infectious_days

infectious_days %>%
  ungroup() %>%
  summarise(mean_days=mean(I_days))-> mean_days
mean_days

round(((1-(1-0.02)**10)*1 + (1-(1-0.02)**3)*6) * mean_days,2) # residents
round(((1-(1-0.02)**3)*6 + 0.02*2) * mean_days,2) # staff


# View(VL %>% subset(ID==203 & Testing=="none" & Intervention=="A) No intervention" &
#                      State %in% c("A.hcwC", "A.hcwNC", "A.rNC", "I.hcwC", "I.hcwNC", "I.rC", "I.rNC" )))
# 
# View(VL %>% subset(Testing=="none" & Intervention=="A) No intervention" &
#                      State %in% c( "I.hcwC", "I.hcwNC", "I.rC", "I.rNC" )))
# 
# View(VL %>% subset(ID==1 & Testing=="daily_antigen" & Intervention=="B) Resident intervention" &
#                      State %in% c("A.hcwC", "A.hcwNC", "A.rNC", "I.hcwC", "I.hcwNC", "I.rC", "I.rNC" )))
# 
# VL %>% 
#   subset(State %in% c("A.hcwC", "A.hcwNC", "A.rNC", "I.hcwC", "I.hcwNC", "I.rC", "I.rNC" )) %>%
#   group_by(ID,Testing,Intervention,Sim.day,State) %>%
#   tally() -> temp
