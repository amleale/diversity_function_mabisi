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
       , transfer != 2 #remove transfer 2 because not measured cold 1day later
       , community != "milk"#remove milk negative controls
       , transfer < 18 #only want up to transfer 17
) 

synt <- filter(ph_data2, community == "synt") 
low <- filter(ph_data2, community == "low")
med <- filter(ph_data2, community == "med")
full <- filter(ph_data2, community == "full")


ph_data2 %>% 
  ggplot(mapping = aes(x = transfer, y= pH, colour=community))+
  geom_point() +
  geom_line(aes(group = name)) + #will connect dots based on "name"
  annotate("text", x = 2, y = 3.875, size = 7, label = "B") +
  ylim(3.35, 3.9) +
  theme_bw() 

synt %>% 
  ggplot(mapping = aes(x = transfer, y= pH))+
  # geom_point(colour= 'tomato1') +
  # geom_line(colour= 'tomato1', aes(group = name)) +
  # geom_point(colour= 'deepskyblue2') +
  # geom_line(colour= 'deepskyblue2', aes(group = name)) +
  # geom_point(colour= 'olivedrab3') +
  # geom_line(colour= 'olivedrab3', aes(group = name)) +
  geom_point(colour= 'mediumpurple2') +
  geom_line(colour= 'mediumpurple2', aes(group = name)) +
  labs(title = "synt") +
  ylim(3.35, 3.9) +
  theme_bw() 

# 'tomato1', 'deepskyblue2','olivedrab3', 'mediumpurple2'
# full=red, low=blue, med=green,  synt=violet 


library(lme4)
library(car)
model.ph <- lm(pH ~ community * transfer, data = ph_data2) # interaction included
Anova(model.ph, type = 3) # interaction is significant
summary(model.ph)


### avgs
ph_summary <- 
  filter(ph_data2) %>% 
  group_by(community, transfer) %>% 
  summarise(avg_ph = mean(pH), sd = sd(pH), se = sd(pH)/sqrt(8))  #8 is number samples (not accurate for med8, because only 7 sampels there)
# View(ph_summary)

# ### avgs
# ph_summary_test <- 
#   filter(ph_data, transfer != "2") %>% 
#   group_by(community, transfer) %>% 
#   summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))
# View(ph_summary_test)

# 
# col_points <- ph_summary %>% filter(transfer == 23 | transfer == 21)
# col_milk <- milk %>% filter(transfer == 23 | transfer == 21)

library(ggplot2)
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
  annotate("text", x = 2, y = 3.875, size = 7, label = "A") +
  ylim(3.35, 3.9) +
  # geom_point(data = col_points, aes(x = transfer, y= avg_ph), colour = "red")+ #highlight potential contaminated samples
  theme_bw() 



milk %>% 
  ggplot(mapping = aes(x = transfer, y= pH))+
  geom_point() +
  geom_point(data = col_milk, aes(x = transfer, y= pH), colour = "red")+ 
  theme_bw() 




ph_t0 <- ph_data %>% 
  ggplot(mapping = aes(x = as.factor(t0_dilution), y=t0_pH_dil )) + 
  # geom_boxplot( ) +
  geom_point(position = position_jitter(seed = 0, width = 0.05)) +
  theme_bw()





