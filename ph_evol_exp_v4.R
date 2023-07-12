# v4 03 july 2023
# editing from FEMS reviewers
# emmeans post-hoc tests

setwd("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/2021_evolution_exp/data/pH_measures")
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidyr) 
library(vegan) #needed for permanova
library(emmeans) #needed for posthoc tukeys pairwise tests 


ph_data <- readxl::read_xlsx(path="ph_evol_exp.xlsx", sheet = "forR") 
ph_data <- ph_data %>% filter(!is.na(pH)) %>% 
  filter(name != "M_8")  #replicate M8 lost early in transfers, therefore remove throughout
milk <- filter(ph_data, community == "milk") # separate milk out before you remove it
ph_data$community <- factor(ph_data$community, levels = c("full", "med", "low", "synt")) #define ordering later for legends

ph_data2 <- filter(ph_data
       , transfer != 2 #remove transfer 2 because not measured cold 1 day later
       , community != "milk"
       , transfer < 18 #only want up to transfer 17
) 

# plot individual replicates each (fig. S1)
ph_data2 %>% 
  ggplot(mapping = aes(x = transfer, y= pH, colour=community))+
  geom_point(colour = 'white') + #remove dots
  geom_line(aes(group = name)) + #will connect dots based on "name"
  # annotate("text", x = 2, y = 3.875, size = 7, label = "B") +
  theme_bw(base_size = 14) 


# plot average pH by community overtime with error bars (fig. 3)
# first create summary file where mean, sd, se are calculated for communinity x transfer
ph_summary <- 
  filter(ph_data2) %>% 
  group_by(community, transfer) %>% 
  summarise(avg_ph = mean(pH), sd = sd(pH), se = sd(pH)/sqrt(8))  #8 is number samples 
# View(ph_summary)

ph_summary %>% 
  filter(transfer < 18) %>% 
  ggplot(mapping = aes(x = transfer, y= avg_ph, colour=community))+
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=avg_ph-se, ymax=avg_ph+se), width=.2,
                position=position_dodge(0.05)) +
  # annotate(geom="text", x=6, y=3.475, label="bars = std.error")+
  # annotate(geom="text", x=6, y=3.45, label="replication = 8")+
  labs(y = "mean pH") +
  # annotate("text", x = 2, y = 3.875, size = 7, label = "A") +
  theme_bw(base_size = 14) 



###### STATISTICAL TESTS #######
#### t-test of transfer 17, synthetic ph vs. others
# ph_data_17 <- ph_data2 %>% filter(transfer == "17") 
# 
library(lme4)
library(car)
# aov_ph17 <- lm(pH ~ community, data = ph_data_17)  ###m not needed once pairwise emmeans done below
# Anova(aov_ph17, type = 3) 
# summary(aov_ph17)

## post-hoc tests
ph_data2$transfer<- as.factor(ph_data2$transfer)
lm_ph <- lm(pH ~ community*transfer, data = ph_data2) 
Anova(lm_ph, type = 3) 
summary(lm_ph)
emmeans(lm_ph, pairwise ~ community|transfer) 
emmeans(lm_ph, pairwise ~ transfer|community) 




###### EXTRA PLOTS ###########
# plot individual communities

synt <- filter(ph_data2, community == "synt") 
low <- filter(ph_data2, community == "low")
med <- filter(ph_data2, community == "med")
full <- filter(ph_data2, community == "full")

synt %>%
  ggplot(mapping = aes(x = transfer, y= pH))+
# geom_point(colour= 'white') +
# geom_line(colour= 'tomato1', aes(group = name)) +
# geom_point(colour= 'white') +
# geom_line(colour= 'deepskyblue2', aes(group = name)) +
# geom_point(colour= 'white') +
# geom_line(colour= 'olivedrab3', aes(group = name)) +
geom_point(colour= 'white') +
geom_line(colour= 'mediumpurple2', aes(group = name)) +
labs(title = "synt") +
ylim(3.35, 3.9) +
  theme_bw()


# # 'tomato1', 'deepskyblue2','olivedrab3', 'mediumpurple2'
# # full=red, low=blue, med=green,  synt=violet


