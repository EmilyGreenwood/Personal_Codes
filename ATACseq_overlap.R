library(dplyr)
library(tidyr)

#tested is a file with 3 key columns: 1) SNP name titled "SNP", 2) position of SNP in GrCH38/hg38 Coordinates titled "bp"
# and 3) chromosome number titled "Chr"
tested<-read.csv('name_of_file.csv')
tested <- tested %>% distinct(SNP, .keep_all=T)

#ensure column values are numeric
tested$bp <- as.numeric(tested$bp)
tested$Chr <- as.numeric(tested$Chr)

#select chromosome of interest from tested list and assign to smaller datatable
cred <- tested %>% select(SNP, bp, Chr)%>%
  filter(Chr==1) #specify chromosome here
cred <- cred[order(cred$bp),] #put in ascending order by position
cred<-na.omit(cred) #remove any empty lines

#read in bed file with ATACseq data with column V1 as chromosome number, V2 as start position of peak, and V3 as end position of peak
bed <- as.data.frame(read.table("name_of_file_here.bed",header = F, sep="\t",stringsAsFactors=FALSE, quote=""))
bed <- bed %>% select(V1,V2,V3)

#ensure column values are numeric
bed$V2 <- as.numeric(bed$V2)
bed$V3 <- as.numeric(bed$V3)

#calculate length of peak
bed$length <- bed$V3 - bed$V2
data1 <- bed %>% filter(V1=='chr1')  #select matching chromosome of interest
data1 <- data1[order(data1$V2),] #put in ascending order by position of starts of peaks
data1 <- na.omit(data1)  #remove any empty lines


i=1
j=1
for (j in 1:dim(data1[1])) {
  for (i in 1:dim(cred)[1]) {
    if ((cred$bp[i] >= data1$V2[j]) & (cred$bp[i] <= data1$V3[j])){
      snp <- cred$SNP[i]
      cred_bp <- cred$bp[i]
      chr<- data1$V1[j]
      bed_start <- data1$V2[j]
      bed_end <- data1$V3[j]
      length <- data1$length[j]
      info[nrow(info) + 1,] = c(snp,cred_bp, chr,bed_start,bed_end,length)
      i= i + 1
      j=1
    }else{
      j= j +1
    }
  }
  write.csv(info, 'Chr1_ATACseq_Overlap.csv')
}

#resulting DF will include any lines in cred table that overlapped iwth ATACseq peak for specified chromosome



