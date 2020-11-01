require(ggpubr)
require(dplyr)
require(tidyr)
require(stringr)
require(readr)

data_file = "Inf"  ###### reset this to either baseline, staff, IC, etc depending on which sensitivity analysis this is so that plot names update

res_master <- read.csv(paste0("2020-10-02res_master_",data_file,".csv")) 
nrow(res_master)


# summary values (mean) across simulations

res_master %>%
  mutate(Intervention = case_when(Intervention == "C) HCW intervention" ~ "C) Immunity-based staffing", 
                                  Intervention == "B) Resident intervention" ~ "B) Resident cohorting",
                                  Intervention == "A) No intervention" ~ "A) None",
                                  Intervention == "D) Both interventions" ~ "D) Both",
                                  TRUE ~ as.character(Intervention))) %>%
  group_by(Sim, Intervention, Testing) %>%
  mutate(cumulative_mortality = cumsum(mortality), 
         proportion_infected_r = cum_inc_r/(100+cumulative_mortality),
         proportion_infected_hcw = (cum_inc_hcw)/(total - (100+cumulative_mortality)), 
         proportion_infected_comm = cum_inc_community/(total - (100+cumulative_mortality)),
         n_temps = total - cumulative_mortality - 200) -> res_master


res_master %>%
  group_by(time, Intervention, Testing) %>%
  summarise(med.r.S = mean((S.rNC)),
            med.hcw.S = mean((S.hcwNC+S.hcwC)),
            med.r.E = mean(E.rNC),
            med.hcw.E = mean((E.hcwNC+E.hcwC)),
            med.r.R = mean((R.rC+R.rNC)),
            med.hcw.R = mean((R.hcwNC+R.hcwC)),
            
            mean.inc.R = mean(inc_r),
            mean.inc.hcw = mean(inc_hcw),
            
            med.cum.inc.r = mean(cum_inc_r),
            med.cum.inc.hcw = mean(cum_inc_hcw),
            med.cum.inc.community = mean(cum_inc_community),
            
            med.proportion.r = mean(proportion_infected_r),
            med.proportion.hcw = mean(proportion_infected_hcw),
            med.proportion.comm = mean(proportion_infected_comm),
            med.proportion.allstaff = med.proportion.hcw + med.proportion.comm,
            
            lower_R = quantile(proportion_infected_r, 0.025),
            upper_R = quantile(proportion_infected_r, 0.975),
            
            lower_hcw = quantile(proportion_infected_hcw, 0.025),
            upper_hcw = quantile(proportion_infected_hcw, 0.975), 
            
            max_n_temps = max(n_temps)
            ) -> summary


test_order <- c("none", "weekly_antigen", "daily_staff_antigen", "daily_antigen", "antigen_highLOD", "daily_PCR", "twice_weekly_PCR", "weekly_PCR",  "fast_PCR", "weekly_PCR_slow")
test_names <- c("None", "Weekly Antigen", "Daily Antigen, Staff only", 
                "Daily Antigen", "Daily Antigen, LOD = 10^7", "Daily PCR", "Twice Weekly PCR",
                "Weekly PCR", "Weekly PCR, 1 day", "Weekly PCR, 7 day")

test_names_labeller = c("none"="None", "weekly_antigen"="Weekly Antigen", "daily_staff_antigen"="Daily Antigen staff only", 
                        "daily_antigen"="Daily Antigen", "antigen_highLOD"="Daily Antigen, LOD = 10^7", 
                        "daily_PCR"="Daily PCR", "twice_weekly_PCR"="Twice Weekly PCR",
                        "weekly_PCR"="Weekly PCR", "fast_PCR"="Weekly PCR, 1 day", "weekly_PCR_slow"="Weekly PCR, 7 day")


## stacked bar plots
summary %>%
  filter(time==181) %>%
  rename(Resident=med.proportion.r, Staff = med.proportion.hcw, Community = med.proportion.allstaff) %>%
  dplyr::select(Intervention, Testing,Staff, Resident, Community, lower_R, upper_R, lower_hcw, upper_hcw) %>%
  gather(Population, Final_size, Staff:Community) %>%
  mutate(Pop_cat = case_when(Population=="Resident" ~ "Resident",
                             Population=="Staff" ~ "Staff",
                             Population=="Community" ~ "Staff"),
         Pop_alpha = case_when(Population=="Resident" ~ "Nursing home",
                               Population=="Staff" ~ "Nursing home",
                               Population=="Community" ~ "Community"),
         Upper_quartile = case_when(Population=="Staff" ~ upper_hcw,
                                    Population=="Resident" ~upper_R),
         Lower_quartile = case_when(Population=="Staff" ~ lower_hcw,
                                    Population=="Resident" ~lower_R)) %>%
  dplyr::select(-c(lower_R, upper_R, lower_hcw, upper_hcw)) -> summary_plots
# 
# summary_plots %>%
#   subset(Pop_alpha == "Nursing home") -> NH
# 
# summary_plots %>%
#   subset(Pop_alpha == "Community") -> Community
# 
# legend_plot <- ggplot()+
#   geom_col(data = NH, aes(x=factor(Testing, levels = test_order),
#                           y=Final_size,  fill=Intervention,alpha=factor(Pop_alpha)), position="dodge", color="grey")+
#   geom_col(data=Community, aes(x=factor(Testing, levels = test_order),
#                                y=Final_size,  fill=Intervention,alpha=factor(Pop_alpha)), position="dodge", color="grey")+
#   geom_errorbar(data=NH,aes(x=factor(Testing, levels = test_order), ymin=Lower_quartile, ymax=Upper_quartile, fill=Intervention),
#                 width=.3, position=position_dodge(0.9)) +
#   scale_alpha_discrete(range=c(1, 0.5), name="Population")+
#   scale_x_discrete(labels=test_names, name="testing strategy")+
#   scale_y_continuous(name = "Cumulative incidence (Proportion)", limits = c(0,1))+
#   #ylab()+
#   #xlab("testing strategy")+
#   facet_wrap(vars(factor(Pop_cat)),nrow=2) + labs(alpha="Source of infection") +
#   theme_bw()
# 
# legend <- get_legend(legend_plot)
# 
# 
# final_size_bar_stacked <- ggplot()+
#   geom_col(data = NH, aes(x=factor(Testing, levels = test_order),
#                           y=Final_size,  fill=Intervention), position="dodge", color="grey")+
#   geom_col(data=Community, aes(x=factor(Testing, levels = test_order),
#                                y=Final_size,  fill=Intervention), position="dodge", color="grey",alpha=0.4)+
#   geom_errorbar(data=NH,aes(x=factor(Testing, levels = test_order), ymin=Lower_quartile, ymax=Upper_quartile, fill=Intervention),
#                 width=.3, position=position_dodge(0.9)) +
#   scale_alpha_discrete(range=c(1, 0.5), name="Population")+
#   scale_x_discrete(labels=test_names, name="testing strategy")+
#   scale_y_continuous(name = "Cumulative incidence (Proportion)", limits = c(0,1))+
#   #ylab()+
#   #xlab("testing strategy")+
#   facet_wrap(vars(factor(Pop_cat)),nrow=2) +
#   theme_bw() + theme(legend.position = "none")
# 
# 
# final_size_bar_stacked_legend <- ggarrange(final_size_bar_stacked,legend,widths=c(5,1))
# final_size_bar_stacked_legend
# ggsave(paste0(Sys.Date(),"_finalsize_bar_stacked_",data_file,".pdf"), final_size_bar_stacked_legend)



## Violin plot

res_master %>%
  group_by(time, Intervention, Testing) %>%
  filter(time==181) %>%
  rename(Resident=proportion_infected_r, Staff = proportion_infected_hcw, Community = proportion_infected_comm) %>%
  dplyr::select(Sim, Intervention, Testing, Staff, Resident, Community) %>%
  gather(Population, Final_size, Staff:Community) %>%
  mutate(Pop_cat = case_when(Population=="Resident" ~ "Resident",
                             Population=="Staff" ~ "Staff",
                             Population=="Community" ~ "Staff"),
         Pop_alpha = case_when(Population=="Resident" ~ "Nursing home",
                               Population=="Staff" ~ "Nursing home",
                               Population=="Community" ~ "Community"))-> violin_data

violin_plot <- ggplot() +
  geom_violin(data=violin_data, aes(x=factor(Testing, levels = test_order), 
                                       y=Final_size,  fill=Intervention), color = NA) +
  facet_wrap(vars(factor(Population, levels = c("Resident", "Staff", "Community")),),nrow=3, 
             labeller = labeller(.cols=c("Resident" = "Resident cases", "Staff"= "Staff cases, nursing home origin", "Community" = "Staff cases, community origin"))) +
  theme_bw() + 
  theme(strip.background =element_rect(fill="white"), 
        panel.grid.major.x=element_blank(), legend.position = "bottom")+
  geom_vline(xintercept=seq(1,length(test_order))+.5,color="lightgrey")+
  geom_vline(xintercept=c(1, 5)+.5,color="lightgrey", lwd=2) +
  scale_x_discrete(labels = function(x) str_wrap(test_names, width = 12), name="Testing strategy")+
  scale_y_continuous(name = "Cumulative incidence (Proportion)", limits = c(0,1)) + 
  labs(fill="Intervention")

violin_plot
ggsave(paste0(Sys.Date(),"_violin_",data_file,".png"), violin_plot, width = 9, height = 6)



# table of results
summary_plots %>%
  ungroup() %>%
  mutate(`Test Strategy` = case_when(Testing=="none" ~ "1. None",
                                    Testing=="weekly_antigen" ~ "2. Weekly Antigen",
                                    Testing=="daily_staff_antigen" ~ "3. Daily Antigen, Staff only",
                                    Testing=="daily_antigen" ~ "4. Daily Antigen",
                                    Testing=="antigen_highLOD" ~ "5. Daily Antigen, low sensitivity",
                                    Testing=="daily_PCR" ~ "6. Daily PCR",
                                    Testing=="twice_weekly_PCR" ~ "7. Twice Weekly PCR",
                                    Testing=="weekly_PCR" ~ "8. Weekly PCR",
                                    Testing=="fast_PCR" ~ "9. Weekly PCR, fast",
                                    Testing=="weekly_PCR_slow" ~ "10. Weekly PCR, delayed"),
         Final_size = format(round(Final_size,2),nsmall=2),
         Lower_quartile = format(round(Lower_quartile,2),nsmall=2),
         Upper_quartile = format(round(Upper_quartile,2),nsmall=2),
         `Cumulative Incidence (95% interval)` = paste0(Final_size," (",Lower_quartile," - ",Upper_quartile,")")) %>%   # is it a 95% confidence interval? 
  dplyr::select(Intervention, `Test Strategy`, Population, Final_size) %>%
  spread(Intervention,Final_size) -> Table3

Table3 %>%
  subset(Population == "Resident") %>%
  dplyr::select(-Population)-> Table3_res

Table3 %>%
  subset(Population == "Staff") %>%
  dplyr::select(-Population)-> Table3_staff

write.csv(Table3_res, file="Table3_residents.csv")
write.csv(Table3_staff, file="Table3_staff.csv")



# Plot side-by-side dynamics to look at no testing vs slow PCR 
colors <- c("Susceptible" = "darkgreen", "Recovered" = "blue", "Incidence" = "red")
linetypes <- c("Resident" = "solid", "Staff" = "dotdash")

summary %>%
  subset(Testing =="none" | Testing =="weekly_PCR_slow" | Testing == "daily_antigen") %>%
  subset(Intervention == "A) None") %>%
  ggplot()+
  geom_line(aes(x=time, y=med.r.S,col="Susceptible",lty="Resident"))+
  geom_line(aes(x=time, y=med.hcw.S, col="Susceptible",lty="Staff"))+
  # geom_line(aes(x=time, y=med.r.E),col="red",lty=1)+
  # geom_line(aes(x=time, y=med.hcw.E),col="red",lty=4)+
  geom_line(aes(x=time, y=med.r.R,col="Recovered",lty="Resident"))+
  geom_line(aes(x=time, y=med.hcw.R,col="Recovered",lty="Staff"))+
  geom_col(aes(x=time, y=mean.inc.hcw*20+mean.inc.R*20, fill = "Incidence"),col=NA,alpha=0.25)+ # put on different scale
  #geom_line(aes(x=time, y=med.cum.inc.community),col="green",lty=4)+ # put on different scale
  scale_y_continuous(name="People", sec.axis = sec_axis(~ . / 20, name = "Cases per day")
                     )+
  scale_x_continuous(limits=c(1,181), breaks=c(0, 30, 60, 90, 120, 150, 180))+
  facet_wrap(vars(factor(Testing, levels = c("none", "weekly_PCR_slow", "daily_antigen"))), 
            labeller = labeller(.cols=c("none" = "No Testing", "weekly_PCR_slow"= "Weekly PCR, 7 day",  "daily_antigen"="Daily Antigen")))+
  theme_bw() +
  scale_color_manual(breaks=c("Susceptible", "Recovered"), values = c("Susceptible" = "darkgreen", "Recovered" = "blue"))+
  scale_linetype_manual(values = linetypes) + 
  labs(fill="", color="", linetype = "")+
  theme(strip.background =element_rect(fill="white"),
        panel.grid.major = element_blank(), panel.grid.minor=element_blank(),
        legend.position = "bottom")  -> zoom_in_notesting

zoom_in_notesting

#ggsave(paste0(Sys.Date(),"_notesting_dynamics_",data_file,".pdf"),zoom_in_notesting, width=10, height=4, units="in")



## Final size curves
final_size_plot <- res_master %>%
  ggplot()+
  #geom_line(aes(x=time, y=cum_inc_r, group=interaction(Sim, Intervention), color=Intervention), lty=1, lwd=0.2)+
  #geom_line(aes(x=time, y=cum_inc_hcw, group=interaction(Sim, Intervention), color=Intervention), lty=4, lwd=0.2)+
  geom_line(data=summary, aes(x=time, y=med.cum.inc.r, color=Intervention), lty=1, lwd=0.8)+
  geom_line(data=summary, aes(x=time, y=med.cum.inc.hcw, color=Intervention), lty=4, lwd=0.8)+
  geom_line(data=summary, aes(x=time, y=med.cum.inc.community, color=Intervention),lty=3, lwd=0.8)+
  scale_y_continuous(name="People", limits=c(0,125))+
  scale_x_continuous(limits=c(0,181), breaks=c(0, 60, 120, 180))+
  facet_wrap(vars(factor(Testing, levels = test_order)), 
             labeller = labeller(.cols=function(x) str_wrap(test_names_labeller, width = 13)), nrow = 2) +
  theme_bw()+
  theme(strip.background =element_rect(fill="white"), legend.position = "bottom")+
  labs(color = "Interventions")

final_size_plot
#ggsave(paste0(Sys.Date(),"_finalsize_",data_file,".pdf"), final_size_plot, width=10, height=5, units="in")


## plot together as figure 3
Figure_3 <- ggarrange(zoom_in_notesting, final_size_plot, ncol=1, labels = c("A", "B"))
ggsave(paste0(Sys.Date(),"_Figure_3.png"), Figure_3, width=10, height=9, units="in")





#plot individual simulations to see spread (Figure S1)
temp <- summary %>%
  subset(Testing =="daily_antigen" | Testing == "weekly_PCR" | Testing == "none") %>%
  subset(Intervention == "A) None" | Intervention == "D) Both")

res_master %>%
  subset(Testing =="daily_antigen" | Testing == "weekly_PCR" | Testing == "none") %>%
  subset(Intervention == "A) None" | Intervention == "D) Both") %>%
  group_by(Sim, Intervention, time) %>%
  ggplot()+
  geom_line(aes(x=time, y=S.rNC, group=Sim),col="darkgreen", lwd=0.1, alpha=0.4)+
  geom_line(aes(x=time, y=S.hcwNC+S.hcwC, group=Sim),col="darkgreen", lwd=0.1, alpha=0.4)+
  geom_line(aes(x=time, y=R.rC+R.rNC, group=Sim),col="blue", lwd=0.1, alpha=0.4)+
  geom_line(aes(x=time, y=R.hcwNC+R.hcwC, group=Sim),col="blue",  lwd=0.1, alpha=0.4)+
  geom_line(aes(x=time, y=E.rNC, group=Sim),col="red", lwd=0.1, alpha=0.4)+
  geom_line(aes(x=time, y=E.hcwNC+E.hcwC, group=Sim),col="red", lwd=0.1, alpha=0.4)+

  geom_line(data=temp, aes(x=time, y=med.r.S),col="darkgreen",lty=1, lwd=0.8)+
  geom_line(data=temp, aes(x=time, y=med.hcw.S),col="darkgreen", lty = 4, lwd=0.8)+
  geom_line(data=temp, aes(x=time, y=med.r.E),col="darkred",lty=1, lwd=0.8)+
  geom_line(data=temp, aes(x=time, y=med.hcw.E),col="darkred", lty = 4, lwd=0.8)+
  geom_line(data=temp, aes(x=time, y=med.r.R),col="blue",lty=1, lwd=0.8)+
  geom_line(data=temp, aes(x=time, y=med.hcw.R),col="blue", lty = 4, lwd=0.8)+
  geom_line(data=temp, aes(x=time, y=med.hcw.R),col="blue", lty = 4, lwd=0.8)+

  scale_y_continuous(name="people")+
  facet_grid(factor(Testing, levels=c("none", "daily_antigen", "weekly_PCR"))~Intervention, 
             labeller = labeller (.rows = c( "none" = "No Testing", "daily_antigen"= "Daily Antigen", "weekly_PCR" = "Weekly PCR"),
                                  .cols = c( "A) None"= "No cohorting interventions", "D) Both interventions" = "Both cohorting interventions")),
             switch = "y"
             )+
  theme_bw() +
  theme(legend.position="none", 
        strip.background =element_rect(fill="white"))  -> sim_plot

sim_plot
ggsave(paste0(Sys.Date(),"_SimulationPlots_",data_file,".png"),sim_plot, width=9, height=9, units="in")



# ## Final size bar plots
# 
# final_size_bar <- summary %>%
#   filter(time==181) %>%
#   rename(Resident=med.proportion.r, HCW = med.proportion.hcw) %>%
#   dplyr::select(Intervention, Testing, HCW, Resident, lower_R, upper_R, lower_hcw, upper_hcw) %>%
#   gather(Population, Final_size, HCW:Resident) %>%
#   mutate(Upper_quartile = case_when(Population=="HCW" ~ upper_hcw, 
#                                     Population=="Resident" ~upper_R),
#          Lower_quartile = case_when(Population=="HCW" ~ lower_hcw, 
#                                     Population=="Resident" ~lower_R)) %>%
#   dplyr::select(-c(lower_R, upper_R, lower_hcw, upper_hcw)) %>%
#   ggplot()+
#   geom_col(aes(x=factor(Testing, levels = test_order), 
#                y=Final_size,  fill=Intervention, 
#                alpha=factor(Population, levels = c("Resident", "HCW"))), position="dodge", color="grey")+
#   geom_errorbar(aes(x=factor(Testing, levels = test_order), ymin=Lower_quartile, ymax=Upper_quartile, 
#                     fill=Intervention, alpha=factor(Population, levels = c("Resident", "HCW"))), 
#                 width=.3, position=position_dodge(0.9)) +
#   scale_alpha_discrete(range=c(1, 0.5), name="Population")+
#   scale_x_discrete(labels=test_names)+
#   scale_y_continuous(name = "Cumulative incidence (Proportion)", limits = c(0,1))+
#   xlab("testing strategy")+
#   theme_bw()
# 
# final_size_bar
# ggsave(paste0(Sys.Date(),"finalsize_bar_baseline.png"), final_size_bar, width=20, height=10, units="in")
# 

### VL ----

VL_master <- read_rds("./2020-09-2873_VL_master_simulations.rds")

VL_master %>%
  #filter(ID == 155) %>%
  ggplot()+
  geom_line(aes(x=Sim.day, y=VL, group=ID))+
  facet_wrap(vars(Testing))+
  scale_x_continuous(limits = c(0,50)) +
  labs(color="Simulation #")


VL_master %>%
  subset(State %in% c("I.rNC", "R.rNC",
                      "I.rC", "R.rC",
                      "A.hcwNC", "I.hcwNC", "R.hcwNC",
                      "A.hcwC", "I.hcwC", "R.hcwC")) %>%
  group_by(Sim,Sim.day,Intervention) %>%
  summarise(mean=quantile(VL)[3],
            lower=quantile(VL)[2],
            upper=quantile(VL)[4]) -> VL_summary

ggplot() +
  geom_point(data=VL_summary,aes(x=Sim.day, y=mean))+
  geom_errorbar(data=VL_summary,aes(x=Sim.day, ymin=lower, ymax=upper))+
  geom_line(data=res_master,aes(x=time,y=(inc_r+inc_hcw)),color="red")+
  facet_wrap(vars(Sim,Intervention)) +
  labs(x="Day",y="Viral load (mean and IQR)") -> VL_lines_inc

VL_lines_inc

#ggsave(paste0(Sys.Date(),"_VLplots_",data_file,".pdf"),VL_lines_inc, width=8, height=6, units="in")


# VL_master %>%
#   subset(Sim==2 & VL!=0) %>%
#   ggplot() + geom_histogram(aes(VL)) +
#   facet_wrap(vars(Sim.day,Intervention)) -> VL_bars_HCWint

#
# ggsave("VL_lines_HCWint.png",VLlines_HCWint)
# ggsave("VL_bars_HCWint.png",VL_bars_HCWint)


VL_master_plot <- VL_master %>%
  subset(!is.na(VL) & Intervention == "A) No intervention" & ID < 10000 & ID != 100) %>%
  group_by(ID, Testing, Intervention) %>%
  mutate(Sim.day2 = Sim.day-min(Sim.day)+1) 

VL_master_plot %>%
  subset(Testing == "daily_antigen") %>%
  group_by(ID) %>%
  ggplot() +
  geom_hline(yintercept=c(4, 7), color="blue", lwd = 0.25)+
  geom_hline(yintercept=c(5), color="red",  lwd = 0.25)+
  geom_line(aes(x=Sim.day2, y=VL, group=ID), alpha=0.5, lwd=0.1)+
  #geom_line(data=mean_VL_person, aes(x=infection.day, y=VL, group=1), lwd=1) +
  scale_x_continuous(name="infectious day", limits=c(0, 35)) +
  scale_y_continuous(breaks=seq(0, 12, by=2), labels = c("0", "10^2", "10^4", "10^6", "10^8", "10^10","10^12"))+
  #facet_wrap(vars(Testing, Intervention))+
  theme_classic() -> VL_schematic

VL_schematic

ggsave("VL_schematic.pdf",VL_schematic, width=6, height=4, units="in")

