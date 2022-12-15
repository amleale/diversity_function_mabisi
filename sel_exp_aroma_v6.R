### this v6 R script first made June 3, 2022
# June 3, 2022 - update colours~community for biplots
# add A, B, C to biplots july 18, 2022

#input data is still the same file
setwd("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/2021_evolution_exp/data/gcms/v5_norm_stand")

library(readxl) #needed to read in excel file
library(dplyr) #needed for data manipulation
library(plyr) #needed for data manipulation
library(tidyverse) #needed for data manipulation
library(ggfortify) #needed for ggplot2 (i.e., pca plots)
library(heatmaply) #needed for heatmaps
library(matrixStats)
library(factoextra) #needed for 


#### data INPUT & quick initial changes ########

#### SEE EXCEL FILE TAB "read.me" for VARIABLE EXPLANATIONS
# zeros were replaced with 0.001 to allow PCA analysis (check in chromeleon that these are true zero value peaks)
aroma <- readxl::read_xlsx(path="rerun_v4_raw.xlsx", sheet ="forR.001") 
aroma <- aroma %>%  #create new variable of community x transfer
  unite("comm_tr", community:transfer, sep= "_", remove = FALSE)

### specify order of factors for later plotting (legends will make more sense)
aroma$transfer <- factor(aroma$transfer, levels = c("t1", "t3", "t5", "t11", "t17"))
aroma$community <- factor(aroma$community, levels = c("full", "med", "low", "synt"))
aroma <- filter(aroma, sample != "M8") # remove all M8 samples (M8 replicate was lost during experiment)


###NORMALISE BY AROMA (ie. column) #####
# first put aroma data frame into new dataframe "aroma_norm" 
aroma_norm <- aroma 

#replace "A1-A19" column names with actual names
colnames(aroma_norm)[7:25] <- c("Acetaldehyde","Acetone","Ethanol", "Hexanal", "2-Heptanone","1-Butanol, 3-methyl",
                                "Hexanoic acid, ethyl ester", "2-Butanone, 3-hydroxy", "2-Heptanol", "5-Hydroxy-4-octanone",
                                "2-Butanone, 4-hydroxy", "2-Nonanone","Acetic acid","Propanoic acid, 2-methyl",
                                "2-Undecanone","Butanoic acid, 3-methyl","Propanedioic acid, propyl", "Octanoic acid", "n-decanoic acid")


# the aroma values are in columns 7 to 25, "2" sets calculation by column, calculate median
# stores as list of values called "col_med"
col_med = apply(aroma_norm[7:25], 2, median) 

# replace values in all rows, for columns 7:25 with [ value/col_med ] 
# need to TRANSPOSE OF THE TRANSPOSE since R automatically reads by row...crazy confusing...but it works
aroma_norm[,7:25] <- t(t(aroma_norm[,7:25])/col_med) 

# replace now updated values in all rows, for columns 7:25 log2(value)
# now data is normalised by aroma (i.e., by column)
aroma_norm[,7:25] <- log2(aroma_norm[,7:25])

# # check that column normalisation works [don't use first row value to check!]
# aroma[3,7] # f1_t1, A1 = 38648139 (takes value in 3rd row, 7th column)
# x=38648139
# # column median of A1 from above = 6585649
# med <- median(aroma$A1) # yes the same...
# aroma_norm[3,7]# value from above normalisation = 2.55
# log2(x/med) # yaaay the same :)
# 


### STANDARDISE BY SAMPLE  (ie. row)#####
# SKIP THIS if your sample volumes have little variation !!!

# calculate mean of row for values in columns 7:25 ("1" specifies action is by row)
# stores as list of values called "row_mean"
row_mean = apply(aroma_norm[7:25], 1, mean)
# and then do the same for standard deviation
row_sd = apply(aroma_norm[7:25], 1, sd) 

# unlike above don't need to transpose b/c R defaults to match value list to order of row
aroma_norm[,7:25] <- aroma_norm[,7:25]-row_mean
aroma_norm[,7:25] <- aroma_norm[,7:25]/row_sd

# ## check that column standardisation works [don't use first row!]. CHECK THIS with aroma_norm <- aroma 
# ## then if good, actually do as continuation from first normalisation)
# aroma[3,7] # f1_t1, A1 = 38648139
# x=38648139
# # row mean of row 3 =
# row_mean #take 3rd value
# mean <- 53795007.1
# row_sd #take 3rd value
# sd <-148932839
# aroma_norm[3,7]# value from above normalisation = -0.102
# (x-mean)/sd # yaaay the same :)
# 


#############  COMPARE WITHIN TRANSFER ########

# exclude milk & kefir,  select only time point of interest
# creates new data frame called aroma_norm2
aroma_norm2 <- aroma_norm %>%
  filter(community != "milk") %>%
  filter(community != "kefir") %>%
  filter(transfer == "t17")  

###### PCA PLOT - transfer 
# autoplot uses ggplot2 (need ggplot2 installed)
# create new dataframe called df2 with only aroma columns (i.e., 7-25).
# look at aroma_norm2 to CONFIRM CHOOSE ONLY the 19 aromas
df2 <- aroma_norm2[7:25] 

### PCA analysis using prcomp() function 
# stores as matrix "pca_res". this will be used for making plots
# scale = TRUE standardizes the input data so mean=0 and variance=1 before doing PCA
pca_res <- prcomp(na.omit(df2), scale. = TRUE)

### SUMMARY STATS of PCA analysis (transfer)
summary(pca_res)
fviz_eig(pca_res)  ### scree plot of dimension contributions
pca_res[["rotation"]] ### aroma contributions to each PC
# write.csv(rotation, "rot_t1.csv") # save output in spreadsheet, will go to source file location (i.e., working directory)
pca_res[["x"]] ### sample contributions to each PC (sorry not working...)

### PLOTS of PCA analysis
## classic PCA plot
autoplot(pca_res, data = aroma_norm2, 
         colour = "community", # colours based on factor "community"
         main = "transfer 17", 
         frame = TRUE, # TRUE puts ellipse around points
         frame.type = 'norm' # "norm" makes it an ellipse
         )+
  annotate("text", x = -0.45, y = 0.8, size = 7, label = "C") + 
  ylim(-0.85, 0.87) +
  xlim(-0.5, 0.5) 


## biplot (shows weightings of aromas)
fviz_pca_biplot(pca_res, labelsize = 3, repel = TRUE,
                title = "transfer 17", 
                label ="var",
                col.ind = aroma_norm2$community,
                pointsize = 2.5) +
  annotate("text", x = -4.5, y = 5.5, size = 7, label = "C") + 
  ylim(-6, 6) +
  xlim(-5, 5) +
  scale_shape_manual(values=c(19,19,19,19)) #makes all points filled circles

# col.ind = "community")
# palette = c('tomato1','deepskyblue2','olivedrab3','mediumpurple2'))


## HEATMAP
# create colour palette to match pca plot (order: full=red, low=blue, med=green,  synt=violet )
pal2 <- colorRampPalette(c('tomato1', 'deepskyblue2','olivedrab3', 'mediumpurple2')) # lists alphabetical

heatmaply(df2,
  Rowv = TRUE,#false keeps "injections" in order
  Colv = FALSE, #false keeps aromas A1-A19 in order
  seriate = "OLO", # other options = "OLO", "mean", "GW, "none"
  showticklabels = c(TRUE, FALSE),
  row_side_colors = aroma_norm2[, c("community")], row_side_palette = pal2, # add column showing community grouping 
  main = "transfer 17", xlab = "aroma compound", column_text_angle = 90)
  


############################################ 
######### COMPARE WITHIN COMMUNITY #########
# doing same as above, but looking within community across different time points

### exclude milk & kefir, filter by community
aroma_norm3 <- aroma_norm %>%
  filter(community != "milk") %>%
  filter(community != "kefir") %>%
  filter(community == "synt") 

df3 <- aroma_norm3[7:25]
pca_res <- prcomp(df3, scale. = TRUE)

### SUMMARY STATS of PCA analysis (community)
summary(pca_res)
fviz_eig(pca_res)  ### scree plot of dimension contributions
# rotation <- pca_res[["rotation"]] ### aroma contributions to each PC
# write.csv(rotation, "rot_t1.csv") # save output in spreadsheet, will go to source file location (i.e., working directory)
# pca_res[["x"]] ### sample contributions to each PC (sorry not working...)

## PCA plot
autoplot(pca_res, data = aroma_norm3, colour = "transfer",
         main = "synt", # manually change main title
         frame = TRUE, frame.type = 'norm'
)

#### HEATMAP - community 
pal3 <- colorRampPalette(c('tomato1', 'royalblue1','mediumpurple2', 'yellow3', 'chartreuse3'))
### create colour palette to match pca plot (order: t1 =red, t3 =yellow, t5 = green, t11 = blue, t17=violet )

heatmaply(df3,
    Rowv = TRUE,#false keeps injection 1-31 in order
    Colv = FALSE, #false keeps A1-A14 in order
    seriate = "none",# other options = "OLO", "mean", "GW, "none"
    row_side_colors = aroma_norm3[, c("transfer")], row_side_palette = pal3,# add column showing transfer
    # labRow = NA,#removing y labels seems to have a bug in heatmaply :(
    main = "synt", xlab = "aroma compound", ylab = "injection") # must manually change main title


##############################################
############# COMPARE ACROSS EVERYTHING ######

aroma_norm4 <- aroma_norm %>%
  filter(community != "milk")%>%
  filter(community != "kefir")

###### PCA PLOT - everything 
df4 <- aroma_norm4[7:25] #choose only aroma values
# df4 <- subset(df4, select = -c(A7)) ### remove A7 (alot of zero values in synthetic communities )
pca_res <- prcomp(df4, scale. = TRUE) 
autoplot(pca_res, data = aroma_norm4, 
         colour = "community",  # choose to colour by community or transfer
         # colour = "transfer",
         main = "all",
         frame = TRUE, frame.type = 'norm'
)

# reminder of colour palettes 
pal2 <- colorRampPalette(c('tomato1', 'deepskyblue2','olivedrab3', 'mediumpurple2')) # lists alphabetical
pal3 <- colorRampPalette(c('tomato1', 'royalblue1','mediumpurple2', 'yellow3', 'chartreuse3'))

 # aroma_norm4[7:25] %>%
  # subset( select = -c(A7)) %>% ### remove A7 (has zero values)#choose only values
  # subset( select = -c(A4)) %>%
  heatmaply(df4,
    Rowv = TRUE,#false keeps injection 1-31 in order
    Colv = FALSE, #false keeps A1-A19 in order
    seriate = "none",
    row_side_colors = aroma_norm4[, c("community")],
    row_side_palette = pal2,
    # row_side_colors = aroma_norm4[, c("transfer")],
    # row_side_palette = pal3,
    main = "all", xlab = "aroma compound", ylab = "injection") 


#################################################
############ OTHER VISUALIZATIONS ####### 
  #  I was playing around here and have not updated or annotated!!!! # sorry for any confusion 
########### 5 #############
## http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/

aroma_norm5 <- aroma_norm %>%
  filter(community != "milk") %>%
  filter(community != "kefir") %>%
  filter(transfer == "t1" | transfer == "t17")
# filter(transfer == "t17" )

aroma_row_name <- aroma_norm5 %>% 
  remove_rownames %>% 
  column_to_rownames(var="full_name") ### changes row names to be the full name (i.e., 1-F1, 3-S5, etc.)

df5 <- aroma_row_name[6:24]  # need to change to 6:24 because made "full_name" into row name
res.pca <- prcomp(na.omit(df5), scale. = TRUE) ##scale = TRUE standardizes the input data so mean=0 and variance=1 before doing PCA.

# # same PCA style as before (colours don't match yet with other graphs...)
# autoplot(res.pca, data = aroma_norm5, colour = "comm_tr", shape = "transfer",
#          frame = TRUE, frame.type = 'norm', frame.level = 0.95, ### ellipse settings
#          main = "t1 & t17, 0.95 ellipse", #manually change to be correct
# )

#### look into component contributions 
summary(res.pca)
fviz_eig(res.pca)  ### scree plot of dimension contributions

rotation <- res.pca[["rotation"]] ### aroma contributions to each PC
write.csv(rotation, "rot_t17.csv") # save output in spreadsheet

res.pca[["x"]] ### sample contributions to each PC

# comm <- as.factor(aroma_norm5$community)

fviz_pca_biplot(res.pca, 
                title = "transfer 1_17")
                # col.ind = comm, 
                # palette = c('tomato1','deepskyblue2','olivedrab3','mediumpurple2'))

### different PCR style
groups <- as.factor(aroma_norm5$comm_tr) # use variable of interest (i.e., community, transfer, etc.)

fviz_pca_ind(res.pca,
             col.ind = groups, # color by groups, ## if manually colouring
             palette = c('tomato1','tomato1', 'deepskyblue2', 'deepskyblue2', 'olivedrab3','olivedrab3','mediumpurple2', 'mediumpurple2'),## if manually colouring
             addEllipses = TRUE, 
             label = "none",
             ellipse.type = c("confidence"), 
             ellipse.level = 0.95,
             legend.title = "community", #change manually
             title = "t1, t17: 0.95 confidence ellipse")


# scale_color_manual(name = "community"), ###me trying to reorder legend...unsuccessfully 
#                    # labels = c("full_t1", "full_t17", "med_t1", "med_t17",
#                    #            "low_t1",  "low_t17", "synt_t1", "synt_t17"),
#                    # values= c('tomato1','tomato1', 'olivedrab3','olivedrab3',
#                    #           'deepskyblue2', 'deepskyblue2', 'mediumpurple2', 'mediumpurple2')),



### find cluster centres (make groups = comm_tr) #####
### find all individual coordinates (make groups = full_name)
# I havent matched the colours here yet, so that full =read, med=green, low=blue, synt=purple

# 1. Individual coordinates
res.ind <- get_pca_ind(res.pca)

# make sure groups created
groups <- as.factor(aroma_norm5$comm_tr) # use variable of interest (i.e., comm_tr, or full_name for every individual point)

# 2. Coordinate of groups
coord.groups1 <- res.ind$coord %>%
  as_tibble() %>%
  select(Dim.1, Dim.2) %>%
  ### trying to decide if both these lines are needed? seems very repetitive...but works. 
  mutate(comm_tr = groups) %>% ### define what to group by (i.e., comm_tr or full_name ) 
  group_by(comm_tr) %>% ##define what to group by (i.e., comm_tr or full_name)
  summarise(
    Dim.1 = mean(Dim.1),
    Dim.2 = mean(Dim.2) ### maybe can find "cloud" width / std also? 
  )
coord.groups1
###can find distance between t1 to t17, BUT how to determine is statistically significant????

#### lines between cluster centres ####
coord.groups1 <- coord.groups1 %>%
  separate(comm_tr, c("community", "transfer"))   ### create two new variables for plotting

coord.groups1 %>% 
  ggplot(aes(x=Dim.1, y=Dim.2, colour=community)) + 
  geom_point() +
  # geom_line(data = . %>% filter(transfer %in% c("t1" , "t5"))) +
  # geom_line(data = . %>% filter(transfer %in% c("t5" , "t17"))) +
  geom_line(data = . %>% filter(transfer %in% c("t1" , "t17"))) +
  geom_label(aes(label = transfer)) +
  xlim(-4, 4) +
  ylim(-4, 4)
  

#### lines between individual points  ###### --- need redo grouping with full_name instead of comm_tr
groups <- as.factor(aroma_norm5$full_name) 

# 1. Coordinate of groups
coord.groups2 <- res.ind$coord %>%
  as_tibble() %>%
  select(Dim.1, Dim.2) %>%
  ### trying to decide if both these lines are needed? seems very repetitive...but works. 
  mutate(full_name = groups) %>% ### define what to group by (i.e., comm_tr or full_name ) 
  group_by(full_name) %>% ##define what to group by (i.e., comm_tr or full_name)
  summarise(
    Dim.1 = mean(Dim.1),
    Dim.2 = mean(Dim.2) ### maybe can find "cloud" width / std also? 
  )
coord.groups2

coord.groups2 <- coord.groups2 %>% 
  separate(full_name, c("transfer", "sample"))   ### create two new variables for plotting

# coord.groups3 <- coord.groups3 %>%
#   separate(sample, c("community", ))   ### need to make row naming full, med, low, synthetic, based on F2 

coord.groups2 <- mutate(coord.groups2, community = ifelse(grepl("F", sample), "full",
                                      ifelse(grepl("M", sample), "med", 
                                      ifelse(grepl("L", sample), "low", 
                                      ifelse(grepl("S", sample), "synt", 
                                             "Other")))))

#reset ordering for colouring
coord.groups2$community <- factor(coord.groups2$community, levels = c("full", "med", "low", "synt"))

coord.groups2 %>% 
  ggplot(aes(x=Dim.1, y=Dim.2)) + 
  geom_point(aes(colour=community)) +
  # geom_line(data = . %>% filter(transfer %in% c("t1" , "t5"))) +
  # geom_line(data = . %>% filter(transfer %in% c("t5" , "t17"))) +
  geom_line(aes(group = sample, colour = community), 
            data = . %>% filter(transfer %in% c("1" , "17"))) +
  geom_text(aes(label = transfer), nudge_x = 0.1, size = 3)


#################################################
############ TOTAL ION COUNT ########
# non-normalised data - see if pipetting error actually made volumes different? or at least aroma different
# if actually wanted to check if total aroma is weaker in synthetic/low, would need to measure WEIGHT into vials

aroma_nomilk <- aroma %>%
  filter(community != "milk") %>%
  filter(community != "kefir")

aroma_nomilk %>%
  # aroma %>%
  ggplot(mapping = aes(x = community, y= log(row_sum), colour = transfer)) +
  # geom_point( position=position_dodge(.5)) +
  geom_boxplot() +
  theme_bw() +
  labs(y= "log(sum ion count)")

### anova of total ion count
library(lme4)
library(car)

model.ions <- lm(log(row_sum) ~ community * transfer, data = aroma_nomilk) 
Anova(model.ions, type = 3) 
summary(model.ions)
## there is a SIGNIFICANT EFFECT OF COMMUNITY (pipetting error with viscosity? )
## so should use normalised values, not total ion count 



