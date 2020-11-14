library(tidyverse)
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
  summarise(median_days=median(I_days))-> median_days
median_days

round(((1-(1-0.02)**10)*1 + (1-(1-0.02)**3)*6) * median_days,2) # residents
round(((1-(1-0.02)**3)*6 + 0.02*2) * median_days,2) # staff

