# v3 ready for GitHub upload: 23-jan-2023

setwd("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/2021_evolution_exp/data/pH_measures")

if(!require(easypackages)) install.packages("easypackages")
easypackages::packages(c("tidyverse", "ggthemes", "manipulate", "plotly"))

library(readxl)
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
  labs(y = "average pH") +
  # annotate("text", x = 2, y = 3.875, size = 7, label = "A") +
  theme_bw(base_size = 14) 



###### STATISTICAL TESTS #######
#### t-test of transfer 17, synthetic ph vs. others
ph_data_17 <- ph_data2 %>% filter(transfer == "17") 

library(lme4)
library(car)
model.ph <- lm(pH ~ community, data = ph_data_17) 
Anova(model.ph, type = 3) 
summary(model.ph)


# library(lme4)
# library(car)
# model.ph <- lm(pH ~ community * transfer, data = ph_data2) # interaction included
# Anova(model.ph, type = 3) # interaction is significant
# summary(model.ph)




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


