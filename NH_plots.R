require(ggpubr)
require(dplyr)
require(tidyr)
require(stringr)
require(readr)
require(ggplot2)
require(lemon)

directories <- c("PPE","IC","res","staff","inf", "discharge", "spillover","specificity", "baseline")

for (i in 1:length(directories)){
  data_file = directories[i]  ###### reset this to the directory listed above depending on which sensitivity analysis this is so that plot names update
  print(data_file)
  
  res_master <- read.csv(paste0("2020-11-13res_master_",data_file,".csv")) 
  nrow(res_master)
  
  res_master %>% 
    mutate(prop_hcw_nc = (S.hcwNC + E.hcwNC + A.hcwNC + I.hcwNC + R.hcwNC) / 
             (S.hcwNC + E.hcwNC + A.hcwNC + I.hcwNC + R.hcwNC + S.hcwC + E.hcwC + A.hcwC+ I.hcwC +R.hcwC), 
           prop_r_nc = (S.rNC + E.rNC + A.rNC + I.rNC + R.rNC) / 
             (S.rNC + E.rNC + A.rNC + I.rNC + R.rNC + I.rC +R.rC), 
           total_hcw = (S.hcwNC + E.hcwNC + A.hcwNC + I.hcwNC + R.hcwNC + S.hcwC + E.hcwC + A.hcwC+ I.hcwC +R.hcwC), 
           total_r = S.rNC + E.rNC + A.rNC + I.rNC + R.rNC + I.rC +R.rC, 
           difference = prop_hcw_nc - prop_r_nc) -> res_master
  
  res_master %>%
    group_by(Intervention, Testing) %>%
    summarise(mean_differnce = mean(abs(difference)), 
              max_difference = max(difference)) -> summary

  # res_master_2 <- read.csv(paste0("2020-09-28res_master_baseline.csv")) 
  # res_master <- bind_rows(res_master,res_master_2)
  # nrow(res_master)
  
  res_master %>% 
    mutate(staff = S.hcwNC + E.hcwNC + A.hcwNC + I.hcwNC + R.hcwNC + 
             S.hcwC + E.hcwC + A.hcwC + I.hcwC + R.hcwC,
           res = S.rNC + E.rNC + A.rNC + I.rNC + R.rNC + 
             I.rC + R.rC) -> temp
  unique(temp$res)
  unique(temp$staff)
  
  
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
           proportion_infected_NH = (cum_inc_r + cum_inc_hcw + cum_inc_community)/total,
           proportion_infected_comm = cum_inc_community/(total - (100+cumulative_mortality)),
           n_temps = total - cumulative_mortality - 200) -> res_master
  
  summary(res_master$n_temps)
  
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
              med.proportion.NH = mean(proportion_infected_NH),
              
              lower_R = quantile(proportion_infected_r, 0.025),
              upper_R = quantile(proportion_infected_r, 0.975),
              
              lower_hcw = quantile(proportion_infected_hcw, 0.025),
              upper_hcw = quantile(proportion_infected_hcw, 0.975), 
              
              max_n_temps = max(n_temps)
              ) -> summary
  
  
  test_order <- c("none", "weekly_antigen", "7 day staff", "3 day both", "3 day staff", "2 day both", "2 day staff","daily_antigen",  "1 day staff",
                 "daily_staff_antigen", "antigen_highLOD", "daily_PCR","2 day both PCR", "twice_weekly_PCR", "weekly_PCR",  "fast_PCR", "weekly_PCR_slow")
  test_names <- c("None", "Weekly Antigen", "Weekly Antigen, Staff only", "2.3x/Week Antigen", "2.3x/Week Antigen, Staff only",
                  "3.5x/Week Antigen", "3.5x/Week Antigen, Staff only", "Daily Antigen", "Daily Antigen, Staff only",
                  "Daily Antigen, Staff (w/ Res PCR)", 
                   "Daily Antigen, LOD = 10^7", "Daily PCR","3.5x/Week PCR", "2.3x/Week PCR",
                  "Weekly PCR", "Weekly PCR, 1 day", "Weekly PCR, 7 day")
  
  test_names_labeller = c("none"="None", "weekly_antigen"="Weekly Antigen", "7 day staff" ="Weekly Antigen, Staff only",
                          "3 day both"="2.3x/Week Antigen", "3 day staff"="2.3x/Week Antigen, Staff only",
                          "2 day both"="3.5x/Week Antigen", "2 day staff"="3.5x/Week Antigen, Staff only",
                          "daily_antigen"="Daily Antigen", "1 day staff"="Daily Antigen, Staff only",
                          "daily_staff_antigen"="Daily Antigen, Staff (w/ Res PCR)", 
                          "antigen_highLOD"="Daily Antigen, LOD = 10^7", 
                          "daily_PCR"="Daily PCR","2 day both PCR"="3.5x/Week PCR", "twice_weekly_PCR"="2.3x/Week PCR",
                          "weekly_PCR"="Weekly PCR", "fast_PCR"="Weekly PCR, 1 day", "weekly_PCR_slow"="Weekly PCR, 7 day")
  
 
  
  ## Violin plot (full version, for supplement)
  
  
  res_master %>%
    group_by(time, Intervention, Testing) %>%
    filter(time==180) %>%
    mutate(Resident=proportion_infected_r, Staff = proportion_infected_hcw, Community = proportion_infected_comm) %>%
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
    geom_vline(xintercept=c(1, 11)+.5,color="lightgrey", lwd=2) +
    scale_x_discrete(labels = function(x) str_wrap(test_names, width = 12), name="Testing strategy")+
    scale_y_continuous(name = "Cumulative incidence (Proportion)", limits = c(0,1)) + 
    labs(fill="Intervention")
  
  violin_plot
  ggsave(paste0(Sys.Date(),"_violin_",data_file,".png"), violin_plot, width = 13, height = 6)
  
  
  
  
  ## Restrict testing strategies for main text (Figure 3 and supplements)
  
  test_order_A <- c("none", 
                    "weekly_antigen", "7 day staff", "3 day both", "3 day staff", "daily_antigen", "1 day staff",
                    "weekly_PCR", "twice_weekly_PCR",  "daily_PCR")
  
  test_names_A <- c("None", 
                    "Weekly Antigen", "Weekly Antigen, Staff only", 
                    "2.3x/Week Antigen", "2.3x/Week Antigen, Staff only", 
                    "Daily Antigen", "Daily Antigen, Staff only",
                    "Weekly PCR", "2.3x/Week PCR", "Daily PCR")
  
  
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  n = 8
  cols = gg_color_hue(n)
  
  violin_data %>%
    subset(Testing %in% test_order_A) %>%
    subset(Intervention != "D) Both") %>%
    ggplot() +
    geom_violin(aes(x=factor(Testing, levels = test_order_A), 
                                      y=Final_size,  fill=Intervention), color = NA) +
    facet_rep_wrap(vars(factor(Population, levels = c("Resident", "Staff", "Community")),),nrow=3, 
                   repeat.tick.labels = TRUE,
                   labeller = labeller(.cols=c("Resident" = "Resident cases", "Staff"= "Staff cases, nursing home origin", "Community" = "Staff cases, community origin"))) +
  
    theme_bw() + 
    theme(strip.background =element_rect(fill="white"), 
          panel.grid.major.x=element_blank(), legend.position = "bottom")+
    geom_vline(xintercept=seq(1,length(test_order))+.5,color="lightgrey")+
   # geom_vline(xintercept=c(3, 5, 8,9)+.5,color="lightgrey", lwd=1) +
    geom_vline(xintercept=c(1, 7)+.5,color="grey", lwd=2) +
    scale_fill_manual(values=cols[c(1,3,5)])+
    scale_x_discrete(labels = function(x) str_wrap(test_names_A, width = 12), name="Testing strategy")+
    scale_y_continuous(name = "Cumulative incidence (Proportion)", limits = c(0,1)) + 
    labs(fill="Intervention") -> violin_plot_A
  
  violin_plot_A
  
  ggsave(paste0(Sys.Date(),"_small_violin_",data_file,".png"), violin_plot_A, width = 12, height = 8)
}



## summary data for tables
summary %>%
  filter(time==181) %>%
  mutate(Resident=med.proportion.r, Staff = med.proportion.hcw, Community = med.proportion.allstaff, NH = med.proportion.NH) %>%
  dplyr::select(Intervention, Testing,Staff, Resident, Community, NH,lower_R, upper_R, lower_hcw, upper_hcw) %>%
  gather(Population, Final_size, Staff:NH) %>%
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

# table of results
summary_plots %>%
  ungroup() %>%
  subset(Testing %in% test_order_A) %>%
  mutate(`Test Strategy` = case_when(Testing=="none" ~ "1. None",
                                    Testing=="weekly_antigen" ~ "2. Weekly Antigen",
                                    Testing=="7 day staff" ~ "3. Weekly Antigen, Staff only",
                                    Testing=="3 day both" ~ "4. 2.3x/Week Antigen",
                                    Testing=="3 day staff" ~ "5. 2.3x/Week Antigen, Staff only",
                                    Testing=="daily_antigen" ~ "6. Daily Antigen",
                                    Testing=="1 day staff" ~ "7. Daily Antigen, Staff only",
                                    Testing=="weekly_PCR" ~ "8. Weekly PCR",
                                    Testing=="twice_weekly_PCR" ~ "9. 2.3x/Week PCR",
                                    Testing=="daily_PCR" ~ "10. Daily PCR"),
         Final_size = format(round(Final_size,2),nsmall=2),
         Lower_quartile = format(round(Lower_quartile,2),nsmall=2),
         Upper_quartile = format(round(Upper_quartile,2),nsmall=2),
         `Cumulative Incidence (95% interval)` = paste0(Final_size," (",Lower_quartile," - ",Upper_quartile,")")) %>%   # is it a 95% confidence interval? 
  dplyr::select(Intervention, `Test Strategy`, Population, Final_size) %>%
  spread(Intervention,Final_size) -> Table3

Table3 %>%
  subset(Population == "NH") -> Table3_overall

# summary_plots %>%
#   ungroup() %>%
#   group_by(Testing,Intervention) %>%
#   summarise(Final = sum(Final_size)) -> overall
#   
# summary_plots %>%
#   subset(Population %in% c("Staff","Community")) %>%
#   ungroup() %>%
#   group_by(Testing,Intervention) %>%
#   summarise(Final = sum(Final_size)) -> Staff
# 
# summary_plots %>%
#   subset(Population %in% c("Staff","Resident")) %>%
#   ungroup() %>%
#   group_by(Testing,Intervention) %>%
#   summarise(Final = sum(Final_size)) -> NH_overall
  
Table3 %>%
  subset(Population == "Resident") %>%
  dplyr::select(-Population)-> Table3_res

Table3 %>%
  subset(Population == "Staff") %>%
  dplyr::select(-Population)-> Table3_staff

write.csv(Table3_res, file="Table3_residents.csv")
write.csv(Table3_staff, file="Table3_staff.csv")
write.csv(Table3_overall, file="Table3_overall.csv")





##############################################################################################################c
## Figure 3
##############################################################################################################

colors <- c("Susceptible" = "darkgreen", "Recovered" = "blue", "Incidence" = "red")
linetypes <- c("Resident" = "solid", "Staff" = "dotdash")

## Dynamics curves
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


## Final size curves

test_names_labeller_A <- c("none" ="None", 
                  "weekly_antigen" ="Weekly Antigen", "7 day staff" = "Weekly Antigen, Staff only", 
                  "3 day both" = "2.3x/Week Antigen", "3 day staff" = "2.3x/Week Antigen, Staff only", 
                  "daily_antigen" = "Daily Antigen", "1 day staff" = "Daily Antigen, Staff only",
                  "weekly_PCR" = "Weekly PCR", "twice_weekly_PCR" = "2.3x/Week PCR",  "daily_PCR" = "Daily PCR")


final_size_plot <- summary %>%
  subset(Testing %in% test_order_A & Intervention != "D) Both") %>%
  ggplot()+
  geom_line(aes(x=time, y=med.cum.inc.r, color=Intervention), lty=1, lwd=0.6)+
  geom_line(aes(x=time, y=med.cum.inc.hcw, color=Intervention), lty=4, lwd=0.6)+
  geom_line(aes(x=time, y=med.cum.inc.community, color=Intervention),lty=3, lwd=0.6)+
  scale_y_continuous(name="People", limits=c(0,125))+
  scale_x_continuous(limits=c(0,181), breaks=c(0, 60, 120, 180))+
  facet_wrap(vars(factor(Testing, levels = test_order_A)), 
             labeller = labeller(.cols=function(x) str_wrap(test_names_labeller_A, width = 13)), nrow = 2) +
  theme_bw()+
  scale_color_manual(values=cols[c(1,3,5)])+
  theme(strip.background =element_rect(fill="white"), legend.position = "bottom")+
  labs(color = "Interventions")

final_size_plot
#ggsave(paste0(Sys.Date(),"_finalsize_",data_file,".pdf"), final_size_plot, width=10, height=5, units="in")


## plot together as figure 3
Figure_3 <- ggarrange(zoom_in_notesting, final_size_plot, ncol=1, labels = c("A", "B"))
Figure_3
ggsave(paste0(Sys.Date(),"_Figure_3.png"), Figure_3, width=10, height=9, units="in")



##############################################################################################################c
## Figure S1
##############################################################################################################


temp <- summary %>%
  subset(Testing =="daily_antigen" | Testing == "weekly_PCR" | Testing == "none") %>%
  subset(Intervention == "A) None" | Intervention == "C) Immunity-based staffing")

res_master %>%
  subset(Testing =="daily_antigen" | Testing == "weekly_PCR" | Testing == "none") %>%
  subset(Intervention == "A) None" | Intervention == "C) Immunity-based staffing") %>%
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
                                  .cols = c( "A) None"= "No interventions", "C) Immunity-based staffing" = "Immunity-based staffing")),
             switch = "y"
             )+
  theme_bw() +
  theme(legend.position="none", 
        strip.background =element_rect(fill="white"))  -> sim_plot

sim_plot
ggsave(paste0(Sys.Date(),"_SimulationPlots_",data_file,".png"),sim_plot, width=9, height=9, units="in")



##############################################################################################################c
## Figure 4
##############################################################################################################

data_file = "Figure4" 
fig_4_data <- read.csv(paste0("2020-11-13res_master_",data_file,".csv")) 
nrow(fig_4_data)

names(fig_4_data)[30] <- "Prevalence"

# summary values (mean) across simulations

fig_4_data %>%
  #subset(Prevalence != 0.005) %>%
  subset(Testing %in% c("1 day both","2 day both", "3 day both","7 day both")) %>%
  group_by(Sim, Prevalence, Testing) %>%
  mutate(cumulative_mortality = cumsum(mortality), 
         proportion_infected_r = cum_inc_r/(100+cumulative_mortality),
         proportion_infected_hcw = (cum_inc_hcw)/(total - (100+cumulative_mortality)), 
         proportion_infected_comm = cum_inc_community/(total - (100+cumulative_mortality)),
         n_temps = total - cumulative_mortality - 200, 
         outbreak = case_when(cum_inc_hcw + cum_inc_r > 0 ~ 1, TRUE ~ 0), 
         outbreak_size = case_when(outbreak==1 ~ cum_inc_hcw + cum_inc_r)
  ) -> fig_4_data


fig_4_data %>%
  group_by(time, Prevalence, Testing) %>%
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
            
            med.proportion.r = median(proportion_infected_r),
            med.proportion.hcw = median(proportion_infected_hcw),
            med.proportion.comm = median(proportion_infected_comm),
            med.proportion.allstaff = med.proportion.hcw + med.proportion.comm,
            
            pr_outbreak = mean(outbreak),
            med.outbreak.size = median(outbreak_size, na.rm=TRUE),
            lower.outbreak.size = quantile(outbreak_size,0.25, na.rm=TRUE),
            upper.outbreak.size = quantile(outbreak_size,0.75, na.rm=TRUE),
            
            lower_R = quantile(proportion_infected_r, 0.25),
            upper_R = quantile(proportion_infected_r, 0.75),
            
            lower_hcw = quantile(proportion_infected_hcw, 0.25),
            upper_hcw = quantile(proportion_infected_hcw, 0.75), 
            
            max_n_temps = max(n_temps)
  ) -> summary_fig4


test_order_fig4 <- c("1 day both", "3 day both","7 day both")#,"1 day staff", "2 day staff", "3 day staff","7 day staff")
test_names_fig4 <- c("Daily", "2.3 times per week","Weekly")#),
#"Daily (staff only)", "3.5 times per week (staff only)", "2.3 times per week (staff only)","Weekly (staff only)")

Prevalence_order <- c(0,0.00005, 0.0002, 0.001, 0.005)
Prevalence_names <- c("Single imported case", "5 per 100,000", "20 per 100,000", "100 per 100,000", "500 per 100,000")

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

n = 8
cols = gg_color_hue(n)

prob_outbreak <- summary_fig4 %>%
  subset(time==180 & Testing %in% test_order_fig4) %>%
  ggplot() + 
  geom_col(aes(x=factor(Prevalence, levels = Prevalence_order),
               y=pr_outbreak,  fill=factor(Testing, levels = test_order_fig4)), position="dodge", color="white") + 
  scale_x_discrete(labels=function(x) str_wrap(Prevalence_names, width=11), name="Prevalence")+
  scale_y_continuous(name = "Probability of an outbreak", limits = c(0,1))+
  scale_fill_manual(values=cols[c(8,2,4,6)],labels=test_names_fig4,name="Testing strategy")+
  theme_bw() + theme(legend.position = "bottom")
prob_outbreak

#ggsave(paste0(Sys.Date(),"_prob_outbreak_",data_file,".png"), prob_outbreak)

size_outbreak <-  summary_fig4 %>%
  subset(time==180 & Testing %in% test_order_fig4) %>%
  ggplot()+
  geom_col(aes(x=factor(Prevalence, levels = Prevalence_order),
               y=med.outbreak.size,  fill=factor(Testing, levels = test_order_fig4)), position="dodge", color="white") + 
  geom_errorbar(aes(x=factor(Prevalence, levels = Prevalence_order), ymin=lower.outbreak.size, ymax=upper.outbreak.size, fill=factor(Testing,levels=test_order_fig4)),
                width=.3, position=position_dodge(0.9)) +
  scale_x_discrete(labels=function(x) str_wrap(Prevalence_names, width=11), name="Prevalence")+
  scale_y_continuous(name = "Median outbreak size")+
  scale_fill_manual(values=cols[c(8,2,4,6)],labels=test_names_fig4,name="Testing strategy")+
  theme_bw() + theme(legend.position = "bottom") 
size_outbreak

#ggsave(paste0(Sys.Date(),"_size_outbreak_",data_file,".png"), size_outbreak)

outbreaks <- ggarrange(prob_outbreak,size_outbreak,
                       common.legend = TRUE, legend = "bottom",
                       labels="AUTO")
outbreaks

#ggsave(paste0(Sys.Date(),"_outbreaks_",data_file,".png"), outbreaks, width = 7, height = 4)




##############################################################################################################c
## Figure SX (Individual simulations -- incidence)
##############################################################################################################


## Cumulative incidence per simulation

fig_4_data %>%
  subset(time <= 180 & Testing %in% test_order_fig4) %>%
  group_by(Sim, Testing, time) %>%
  ggplot()+
  geom_line(aes(x=time, y=cum_inc_hcw + cum_inc_r, group=Sim), alpha=0.8, lwd=0.1)+ # put on different scale
  facet_grid(factor(Testing, labels = test_names_fig4) ~ factor(Prevalence,labels=Prevalence_names)) + 
  theme_bw() +
  scale_y_continuous(name = "Cumulative Incidence (individuals)")+
  labs(fill="", color="", linetype = "", subtitle="Daily probability of infection from the community (among staff only)")+
  theme(strip.background =element_rect(fill="white"),
        panel.grid.major = element_blank(), panel.grid.minor=element_blank(),
        legend.position = "bottom")  -> incidence_plot

incidence_plot

ggsave(paste0(Sys.Date(),"_incidence_plot_",data_file,".png"),incidence_plot, width=8, height=8, units="in")



##############################################################################################################c
## VL Figure
##############################################################################################################

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

