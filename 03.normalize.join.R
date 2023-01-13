library(data.table)

setwd("minimap2_hits")

summary_files <- list.files(pattern="summary.txt")

#exclude 16 since no data
summary_files <- summary_files[summary_files != "barcode16.summary.txt"]
summary_files <- summary_files[summary_files != "barcode43.summary.txt"]
summary_files <- summary_files[summary_files != "barcode49.summary.txt"]
summary_files <- summary_files[summary_files != "barcode73.summary.txt"]

whole_file <- data.frame()

for (i in summary_files){
	print(i)
	t <- fread(i,strip.white=T,header=F,sep=" ")
	print(nrow(t))
	print(head(t))
	t$sample <- i
	t$V1 <- as.numeric(t$V1)
	t <- t[t$V1 > 5,]
	t$V1 <- t$V1/sum(t$V1)
	colnames(t) <- c("abundance","cluster","sample")
	print(head(t))
	whole_file <- rbind(t,whole_file)
}

write.csv(whole_file,"output_barcoded_mabisi_July29.csv")
