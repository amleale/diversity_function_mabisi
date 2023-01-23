# v9 for GitHub upload: 23-jan-2023

#input data is still the same file
setwd("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/2021_evolution_exp/data/gcms/v5_norm_stand")

library(readxl) #needed to read in excel file
library(dplyr) #needed for data manipulation
library(plyr) #needed for data manipulation
library(tidyverse) #needed for data manipulation
library(ggfortify) #needed for ggplot2 (i.e., pca plots)
library(heatmaply) #needed for heatmaps

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

## check that column standardization works [don't use first row!]. CHECK THIS with aroma_norm <- aroma
# ## then if good, actually do as continuation from first normalization)
# aroma[3,7] # f1_t1, A1 = 38648139
# x=38648139
# # row mean of row 3 =
# row_mean #take 3rd value
# mean <- 53795007.1
# row_sd #take 3rd value
# sd <-148932839
# aroma_norm[3,7]# value from above normalization = -0.102
# (x-mean)/sd # yaaay the same :)
# 


#############  COMPARE WITHIN TRANSFER ########

# exclude milk & kefir,  select only time point of interest
# creates new data frame called aroma_norm2
aroma_norm2 <- aroma_norm %>%
  filter(community != "milk") %>%
  filter(community != "kefir") %>%
  filter(transfer == "t5")  

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
## PCA with biplot arrows  
aroma_pca <- autoplot(pca_res, data = aroma_norm2,
                      colour = "community", # colours based on factor "community"
                      main = "B: transfer 5",
                      loadings = TRUE, loadings.colour = 'grey20',
                      loadings.label = TRUE, loadings.label.colour = 'grey20',
                      loadings.label.size = 4, loadings.label.repel=T) +
  #NEED TO reorder so arrows and labels are top layer
  stat_ellipse(geom = "polygon", level = 0.95, aes(fill = community), alpha = 0.25) +
  theme_bw(base_size = 14) 
# +  
#   theme(legend.position="none")  #don't know why I can't remove legend in theme_bw...but okay


#re-order layers 
aroma_pca$layers <- aroma_pca$layers[c(4,1,2,3)]
aroma_pca
# ggsave("aroma_pca.svg",width=9,height=6.5)

#need to go back to line 97 and run from here to alter which transfer is plotted 

# # combining graphs. I actually will just do this in powerpoint...
# library(cowplot)
# library(magick)
# 
# t1 <- ggdraw() + draw_image("f2_A.pdf")
# t2 <- ggdraw() + draw_image("f2_B.pdf")
# t3 <- ggdraw() + draw_image("f2_C.pdf")
# 
# plot_grid(t1, t2, t3,  labels="NULL")
# #^^This will add capital labels to the plots automatically, but can be customised
# plot_grid(t1,t2,t3, labels=c("A: Transfer 1","B: Transfer 10","C: Transfer 17" ))


############################################ 
########### not in manuscript ##############
######### COMPARE WITHIN COMMUNITY #########
# doing same as above, but looking within community across different time points

### exclude milk & kefir, filter by community
aroma_norm3 <- aroma_norm %>%
  filter(community != "milk") %>%
  filter(community != "kefir") %>%
  filter(community == "full") 

df3 <- aroma_norm3[7:25]
pca_res <- prcomp(df3, scale. = TRUE)

### SUMMARY STATS of PCA analysis (community)
summary(pca_res)
# fviz_eig(pca_res)  ### scree plot of dimension contributions
# rotation <- pca_res[["rotation"]] ### aroma contributions to each PC
# write.csv(rotation, "rot_t1.csv") # save output in spreadsheet, will go to source file location (i.e., working directory)
# pca_res[["x"]] ### sample contributions to each PC (sorry not working...)

## PCA plot
autoplot(pca_res, data = aroma_norm3, colour = "transfer", main = "full",
         # loadings = TRUE, loadings.colour = 'grey20'
         # , loadings.label = TRUE, loadings.label.colour = 'grey20', loadings.label.size = 4
         ) +
  stat_ellipse(geom = "polygon", level = 0.95, aes(fill = transfer), alpha = 0.25) +
  theme_bw(base_size = 14)






