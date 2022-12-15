#clustered_v5.R (November 17, 2022)
# PERMANOVAs with adonis2 on community compositions
# centroids of nmds plots with arrows connecting 
# STILL NEED TO ORDER LEGEND of community compositions

#clustered_v5.R (December 15, 2022)
# REORDER LEGEND of community compositions ....must be done by hand :(
# order x axis of community compositions by replicate and add missing barcodes
# fixing visualisation of nmds plots 


# Q for Ben: rarefaction curve??? normalised???? 

setwd("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/2021_evolution_exp/data/bioinformatics/clustered")
library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidyr) 
library(vegan) #needed for permanova


CLUST <-read.csv("output_barcoded_mabisi_July29.csv", sep=(",")) 
CLUST$cluster <- gsub(".*;","",CLUST$cluster) # summarise long complicated name
CLUST <- CLUST %>% rename(barcode = sample)
CLUST$barcode <- gsub("barcode","bc",CLUST$barcode) #take "barcode" and replace with "bc"
CLUST$barcode <- gsub(".summary.txt","",CLUST$barcode) #take ".summary.txt" and replace with nothing
CLUST <- CLUST[,2:4]
#dot - pick up everything, * is multiple of, up to ; then replace before ; with nothing
# https://regex101.com/

#inner_join [add information of community, transfer]
META <-read.csv("220208_META.csv", sep=(",")) #descriptor variables for each barcode 
META <- META %>% rename(barcode = X.NAME)
META$COMMUNITY <- gsub("full ","full", META$COMMUNITY) #fix error with space after "full "

merged <- merge(CLUST, META, by = "barcode", all = TRUE) 
merged <- aggregate(.~barcode + cluster + SAMPLE + TRANSFER + COMMUNITY, merged, FUN = sum)  #need to combine when multiple clusters of same species >1%
merged <- merged %>% filter(abundance > 0.01) #filter only clusters of abundance over 1%
merged$TRANSFER <- factor(merged$TRANSFER, levels = c("t01", "t05", "t17")) # makes plotting better later
merged$COMMUNITY <- factor(merged$COMMUNITY, levels = c("full", "med", "low", "synt")) # makes plotting better later

#### make colour scheme without controls (Anneloes = 86, neg = 90 , zymo = 94)
reduced <- merged %>% 
  filter(barcode != "bc90") %>% 
  filter(barcode != "bc94") %>% 
  filter(barcode != "bc86") %>% 
  separate(SAMPLE, c("time", "replicate"), sep = "-", remove = FALSE)


#manually add missing barcodes 
reduced <- rbind(reduced, c("bc200","Acetobacter_lovaniensis", "1-F5", 1, "F5", "t01", "full", 0.001))
reduced <- rbind(reduced, c("bc201","Acetobacter_lovaniensis", "1-F7", 1, "F7", "t01", "full", 0.001))
reduced <- rbind(reduced, c("bc202","Acetobacter_lovaniensis", "1-M8", 1, "M8", "t01", "med", 0.001))
reduced <- rbind(reduced, c("bc207","Acetobacter_lovaniensis", "1-S2", 1, "S2", "t01", "synt", 0.001))
reduced <- rbind(reduced, c("bc203","Acetobacter_lovaniensis", "1-L2", 1, "L2", "t01", "low", 0.001))
reduced <- rbind(reduced, c("bc204","Acetobacter_lovaniensis", "5-M8", 5, "M8", "t05", "med", 0.001))
reduced <- rbind(reduced, c("bc205","Acetobacter_lovaniensis", "5-L4", 5, "L4", "t05", "low", 0.001))
reduced <- rbind(reduced, c("bc206","Acetobacter_lovaniensis", "17-M8", 17, "M8", "t17", "med", 0.001))

reduced$replicate <- substr(reduced$replicate, 2, 2) #remove letter from replicate to just have number
reduced$abundance <- as.numeric(reduced$abundance) #rbind as mad it a character, so need to go back to numeric



# see how many unique clusters there are
unique <- unique(reduced$cluster) 
#TAKE THIS into .csv file and manually chose colour
# https://chichacha.netlify.app/post/2018-12-09-having-bit-of-party-with-material-colour-palette_files/figure-html/materialColourCheatSheet-1.png
# write.csv(unique,"~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/2021_evolution_exp/data/bioinformatics/clustered//unique.csv", row.names = FALSE)

## need to make list that also includes cluster name with colour
# if just list of colours, then they are reordered to the clusters found at each plot (i.e., changes based on community / trasnfer)
COLOURS <- read.csv("colours.csv", sep=(","))
COLOURS_use <- COLOURS$colour
names(COLOURS_use) <- COLOURS$type

# #SPECIFY ORDER OF COLOURS
# reduced$cluster <- factor(reduced$cluster, levels = COLOURS_use$type) 
new_names <- read.csv("new_names.csv", sep=(",")) #rename the groups (i.e., not species name)

new_names$name <- factor(new_names$name, levels =c("Limosilactobacillus", "Lactobacillus A", "Lactobacillus B", "Lactobacillus C",
                   "Acetobacter A", "Acetobacter B", "Acetobacter C",
                   "Propionibacterium", "Rhodanobacter", "Pseudomonas", "Staphylococcus", "Moraxella",
                   "Enterococcus A", "Enterococcus B", "NA"))

#### order by time point
reduced %>% 
  ggplot(aes(x = replicate, y = abundance, fill = cluster)) + 
  geom_col() + ylim(0,1) +
  scale_fill_manual(labels = new_names$name, values = COLOURS_use)+
  facet_wrap(
    ~TRANSFER +
      ~COMMUNITY, 
    scales="free_x", nrow=3) +
  theme_bw() +
  xlab("replicate") +
  theme(axis.ticks.x = element_blank(),
        # axis.text.x = element_blank(),
        # legend.position="none"
  )


# #### order by treatment
# reduced %>% 
#   ggplot(aes(x = replicate, y = abundance, fill = cluster)) +
#   geom_col() + ylim(0,1) +
#   #me trying to change the legend order...
#   scale_fill_manual(labels = new_names$name, values = COLOURS_use)+
#   facet_wrap(
#     ~COMMUNITY +
#     ~TRANSFER, 
#     scales="free_x", nrow=2) +
#   theme_bw() +
#   xlab("replicate") +
#   theme(axis.ticks.x = element_blank()
#         # axis.text.x = element_blank()
#         # legend.position="none"
#   )



  
####### NMDS PLOTS ######

pivoted <- reduced %>% 
  pivot_wider(names_from = cluster, values_from = abundance, 
              id_cols = c("barcode", "SAMPLE", "COMMUNITY", "TRANSFER"))


pivoted <- pivoted[, -c(11:18)] # remove clusters that are just contamination/rare types b/c this introduces too many NAs
# pivoted <- pivoted %>% filter(COMMUNITY == "full")

pivoted <- as.data.frame(pivoted)
pivoted[is.na(pivoted)] <- 0
com = pivoted[,5:ncol(pivoted)] #take just columns with data

set.seed(123)
nmds = vegan::metaMDS(com, distance = "bray") # currently ERRORS b/c insufficient data
plot(nmds)

### plotting taken from:
# https://jkzorz.github.io/2019/06/06/NMDS.html#:~:text=Non%2Dmetric%20Multi%2Ddimensional%20Scaling,a%202D%20representation%20or%20ordination.

data.scores = as.data.frame(scores(nmds)$sites) #makes output of axis scores, appears to keep in order so next lines work
data.scores$SAMPLE = pivoted$SAMPLE # adds information for plotting/grouping
data.scores$TRANSFER = pivoted$TRANSFER
data.scores$COMMUNITY = pivoted$COMMUNITY

## finding centroids and plotting them 
data.scores$group <- paste0(data.scores$TRANSFER, "_",data.scores$COMMUNITY) # paste puts two vectors together (can also use dyplr), adds underscore between

centroid <- aggregate(cbind(NMDS1, NMDS2) ~ group, data = data.scores, FUN = mean) #take mean on x and mean on y to head centroid
centroid$COMMUNITY <- gsub(".*_", "", centroid$group) #take everything before _, replace with nothing
centroid$TRANSFER <- gsub("_.*", "", centroid$group) #take everything after _, replace with nothing

centroid %>% 
  ggplot(aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(colour = COMMUNITY, shape =  TRANSFER
  ))

# create the two lines 
int_1 <- data.frame(treat = c("full","low","med","synt"),
                    xstart = subset(centroid,TRANSFER=="t01")$NMDS1,
                    ystart = subset(centroid,TRANSFER=="t01")$NMDS2,
                    xend = subset(centroid,TRANSFER=="t05")$NMDS1,
                    yend = subset(centroid,TRANSFER=="t05")$NMDS2)

int_2 <- data.frame(treat = c("full","low","med","synt"),
                    xstart = subset(centroid,TRANSFER=="t05")$NMDS1,
                    ystart = subset(centroid,TRANSFER=="t05")$NMDS2,
                    xend = subset(centroid,TRANSFER=="t17")$NMDS1,
                    yend = subset(centroid,TRANSFER=="t17")$NMDS2)

# plot centroids and connect t1 to t5 to t17 with lines 
centroid %>% 
  ggplot(aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(colour = COMMUNITY, shape =  TRANSFER))+
  geom_segment(data=int_1,aes(x=xstart,xend=xend,y=ystart,yend=yend),arrow=arrow(length=unit(0.2,"cm"))) +
  geom_segment(data=int_2,aes(x=xstart,xend=xend,y=ystart,yend=yend),arrow=arrow(length=unit(0.2,"cm"))) +
  theme_bw() 

#making plot with ALL nmds points 
data.scores %>% 
ggplot(aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(colour = COMMUNITY,shape = TRANSFER ))+ 
  theme_bw() +
  labs(x = "NMDS1", colour = "COMMUNITY", y = "NMDS2", shape = "TRANSFER")  
 

##### ADONIS PERMANOVA ########
library(vegan)

reduced <- merged %>%  # check you have "reduced" before manipulating for visualisations
  filter(barcode != "bc90") %>% 
  filter(barcode != "bc94") %>% 
  filter(barcode != "bc86") 


# transform to horizontal
wide <- reduced  %>% 
  pivot_wider(names_from = cluster, values_from = abundance, values_fill = 0)

#everything, with interaction
adonis2(wide[ ,5:18] ~ COMMUNITY*TRANSFER, data = wide[,1:4], method = "bray") 
# adonis2(wide[ ,5:18] ~ wide$COMMUNITY*wide$TRANSFER, method = "bray") #alternative 

#by transfer
adonis2(wide[wide$TRANSFER=="t01",5:18] ~ COMMUNITY, 
        data = wide[wide$TRANSFER=="t01",1:4], method = "bray") 

adonis2(wide[wide$TRANSFER=="t05",5:18] ~ COMMUNITY, 
        data = wide[wide$TRANSFER=="t05",1:4], method = "bray") 

adonis2(wide[wide$TRANSFER=="t17",5:18] ~ COMMUNITY, 
        data = wide[wide$TRANSFER=="t17",1:4], method = "bray") 

#by community
adonis2(wide[wide$COMMUNITY=="full",5:18] ~ TRANSFER, 
        data = wide[wide$COMMUNITY=="full",1:4], method = "bray") 

adonis2(wide[wide$COMMUNITY=="med",5:18] ~ TRANSFER, 
        data = wide[wide$COMMUNITY=="med",1:4], method = "bray") 

adonis2(wide[wide$COMMUNITY=="low",5:18] ~ TRANSFER, 
        data = wide[wide$COMMUNITY=="low",1:4], method = "bray") 

adonis2(wide[wide$COMMUNITY=="synt",5:18] ~ TRANSFER, 
        data = wide[wide$COMMUNITY=="synt",1:4], method = "bray") 



####### RATIOS OF ACETOBACTERS vs. LACTIC ACID #########
wide <- wide %>%
  mutate(AAB = Acetobacter_lovaniensis + Acetobacter_orientalis + Acetobacter_sp._SRT1) %>%
  mutate(LAB = Lactobacillus_delbrueckii_subsp._lactis + Lactobacillus_fermentum + Lactobacillus_helveticus + Lactobacillus_sp._RA2113)

combined <- wide %>%
  group_by(barcode, SAMPLE, TRANSFER, COMMUNITY) %>%
  summarise(AAB2 = sum(AAB), LAB2 = sum(LAB)) %>%
  mutate(TOTAL = AAB2 + LAB2) %>%
  mutate(RATIO = AAB2 / LAB2) 

combined$TRANSFER2 <- gsub("t"," ", combined$TRANSFER)
combined$REP <- gsub(".*-"," ", combined$SAMPLE)

### PLOT relative abundances of AAB and LAB 
combined %>%
  ggplot(aes(colour = COMMUNITY, group = REP)) + 
  geom_line(aes(x = as.numeric(TRANSFER2), y = LAB2)) +
  geom_line(aes(x = as.numeric(TRANSFER2), y = AAB2), linetype= "longdash") +
  theme_bw() +
  xlab("transfer") +
  ylab("relative abundance") +
  scale_x_continuous(breaks=c(1,5,17)) +
  annotate("text", x = 10, y = 0.3, label = "acetic acid bacteria") +
  annotate("text", x = 10, y = 0.67, label = "lactic acid bacteria")
                  

# # EXTRA PLOTS
# #### plotting individual clusters
# df <- wide %>%
#   group_by(SAMPLE, TRANSFER, COMMUNITY) %>%
#   summarise(aceA = sum(Acetobacter_lovaniensis), 
#             aceB = sum(Acetobacter_orientalis),
#             aceC = sum(Acetobacter_sp._SRT1),
#             lacA = sum(Lactobacillus_delbrueckii_subsp._lactis),
#             lacB = sum(Lactobacillus_helveticus),
#             lacC = sum(Lactobacillus_sp._RA2113),
#             limA = sum(Lactobacillus_fermentum))
# 
# df$TRANSFER2 <- gsub("t"," ", df$TRANSFER)
# df$REP <- gsub(".*-"," ", df$SAMPLE)
# 
# df %>%
#   ggplot(aes(colour = COMMUNITY, group = REP)) + 
#   geom_line(aes(x = as.numeric(TRANSFER2), y = lacA)) +
#   geom_line(aes(x = as.numeric(TRANSFER2), y = lacB), linetype= "longdash") +
#   geom_line(aes(x = as.numeric(TRANSFER2), y = lacC), linetype= "dotted") + #lactobacillus C only seen in low at t0
#   theme_bw() +
#   xlab("transfer") +
#   ylab("relative abundance") +
#   ggtitle("Lactobacillus A, B, C") +
#   scale_x_continuous(breaks=c(1,5,17))+ 
#   annotate("text", x = 13, y = 0.3, label = "Lactobacillus A (solid)") +
#   annotate("text", x = 13, y = 0.6, label = "Lactobacillus B (dashed)")
# 
# df %>%
#   ggplot(aes(colour = COMMUNITY, group = REP)) + 
#   geom_line(aes(x = as.numeric(TRANSFER2), y = aceA)) +
#   geom_line(aes(x = as.numeric(TRANSFER2), y = aceB), linetype= "longdash") +
#   geom_line(aes(x = as.numeric(TRANSFER2), y = aceC), linetype= "dotted") +
#   theme_bw() +
#   xlab("transfer") +
#   ylab("relative abundance") +
#   ggtitle("Acetobacter A, B, C") +
#   scale_x_continuous(breaks=c(1,5,17)) + 
#   annotate("text", x = 13, y = 0.2, label = "Acetobacter A (solid)") +
#   annotate("text", x = 13, y = 0.1, label = "Acetobacter B (dashed)")
# 
#   
###### EXTRA PLOTS #######
# ### TRANSFER
# reduced %>% 
#   filter(TRANSFER == "t17") %>%
#   ggplot(aes(x = barcode, y = abundance, fill = cluster)) + 
#   geom_col() + ylim(0,1) +
#   scale_fill_manual(values = COLOURS_use) +
#   facet_wrap(
#     ~factor(COMMUNITY, levels=c('full','med','low','synt', 'Anneloes')) +
#       ~TRANSFER, scales="free_x", nrow=1) +
#   theme_bw() +
#   xlab("replicates") +
#   theme(axis.ticks.x = element_blank(),
#         axis.text.x = element_blank(),
#         # legend.position="none"
#         )
# 
# ### COMMUNITY
# reduced %>% 
#   filter(COMMUNITY == "full") %>%
#   ggplot(aes(x = barcode, y = abundance, fill = cluster)) + 
#   geom_col() + ylim(0,1) +
#   scale_fill_manual(values = COLOURS_use) + 
#   facet_wrap(~TRANSFER, scales="free_x", nrow=1)+
#   labs(title = "full") +
#   theme_bw() +
#   xlab("replicates") +
#   theme(axis.ticks.x = element_blank(),
#         axis.text.x = element_blank(),
#         legend.position="none")
# 
# #### SINGLE BARCODE
# merged %>% 
#   filter(barcode == "bc94") %>%
#   ggplot(aes(x = barcode, y = abundance, fill = cluster)) + 
#   geom_col() + ylim(0,1) +
#   # theme(legend.position="none") +
#   scale_fill_manual(values = COLOURS_use)
#   facet_wrap(~barcode, scales="free_x", nrow=1)

# ZYMO CONTROL:
# Listeria monocytogenes - 12%, Pseudomonas aeruginosa - 12%, Bacillus subtilis - 12%
# Escherichia coli - 12%, Salmonella enterica - 12%, Lactobacillus fermentum - 12%
# Enterococcus faecalis - 12%, Staphylococcus aureus - 12%, Saccharomyces cerevisiae - 2%, 
# Cryptococcus neoformans - 2%.new 


  
  