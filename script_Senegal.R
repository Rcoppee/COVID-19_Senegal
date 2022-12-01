#setwd("C:/Users/Romain/Desktop/senegal")

#Import R packages
library(dplyr)
library(lubridate)
library(ggplot2)
library(cowplot)
library(lessR)
library(tidyr)
library(readr)

#Import metadata
my_data = read_tsv("senegal_metadata.tsv")
#lineages and list of mutations
nextclade = read_tsv("nextclade.tsv")

#Add year of collection as a new column
for(i in 1:nrow(my_data)){
  if(my_data$Collection_date[i]<="2020-12-31"){
    my_data$year[i]=2020
    }else if(my_data$Collection_date[i]>="2021-01-01" && my_data$Collection_date[i]<="2021-12-31"){
      my_data$year[i]=2021
    }else{
      my_data$year[i]=2022
    }
}

#Focusing on data collected in 2020
variants_region_2020=my_data[my_data$year==2020,]
#Keep only cities with enough data
variants_region_2020=variants_region_2020[variants_region_2020$City %in% c("Dakar", "Diourbel","Kedougou",
                                                                           "Louga","Matam","Saint-Louis","Thies"),]
#Group by pango lineages
variants = variants_region_2020 %>%
  group_by(Pangolin) %>%
  summarise(count=n())

#mark as "other variants" the lineages found at low frequency
for(i in 1:nrow(variants_region_2020)){
  for(j in 1:nrow(variants)){
    if(variants_region_2020$Pangolin[i]==variants$Pangolin[j]){
      if(variants$count[j]<10){
        variants_region_2020$corrected[i]="Other Variants"
      }else{
        variants_region_2020$corrected[i]=variants_region_2020$Pangolin[i]
      }
    }
  }
}

#calculate the proportion of lineages for each city
my_2020 = variants_region_2020 %>%
  group_by(City, corrected) %>%
  summarise(count=n()) %>%
  mutate(prop=count/sum(count)*100)


#Focusing on data collected in 2021
variants_region_2021=my_data[my_data$year==2021,]
#Keep only cities with enough data
variants_region_2021=variants_region_2021[variants_region_2021$City %in% c("Dakar", "Diourbel","Fatick","Kaolack",
                                                                           "Louga","Saint-Louis","Thies"),]

#Group by pango lineages
variants = variants_region_2021 %>%
  group_by(Pangolin) %>%
  summarise(count=n())

#mark as "other variants" the lineages found at low frequency
for(i in 1:nrow(variants_region_2021)){
  for(j in 1:nrow(variants)){
    if(variants_region_2021$Pangolin[i]==variants$Pangolin[j]){
      if(variants$count[j]<10){
        variants_region_2021$corrected[i]="Other Variants"
      }else{
        variants_region_2021$corrected[i]=variants_region_2021$Pangolin[i]
      }
    }
  }
}

#calculate the proportion of lineages for each city
my_2021 = variants_region_2021 %>%
  group_by(City, corrected) %>%
  summarise(count=n()) %>%
  mutate(prop=count/sum(count)*100)

#plot the proportion of each lineage per city in 2020
p1<-ggplot(data=my_2020, aes(x=City, y=prop, fill=corrected)) +
  geom_bar(stat="identity")+
  scale_y_continuous(limits = c(0,101), expand = c(0, 0))+
  scale_fill_manual(values=c("#ee1f25","#f16423","#ffc608","#d3de25",
                             "#8bc63f","#4ad819","#6ec59d","#3f7bbe"))+
  theme_classic()+
  theme(axis.text.x = element_text(color="#000000", size=12, angle=45, margin = margin(20, 0, 0, 0)),
        axis.text.y = element_text(color="#000000", size=12),
        axis.line = element_line(colour = "#000000", size = 1, linetype = "solid"),
        plot.title = element_text(color="#000000", size=14, face="bold"),
        axis.title.x = element_text(color="#000000", size=12, face="bold"),
        axis.title.y = element_text(color="#000000", size=12, face="bold"),
        legend.title = element_text(colour="#000000", size=12, face="bold"))+
  ggtitle("Variants by regions (2020)") +
  ylab("Frequency (%)") + xlab("") + labs(fill='Lineage') 


#plot the proportion of each lineage per city in 2021
p2<-ggplot(data=my_2021, aes(x=City, y=prop, fill=corrected)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values=c("#ed1665","#d74699","#9d4f9f","#ee1f25",
                             "#3b52a4","#ffc608","#44afe4","#4ad819",
                             "#067e04","#004406","#3f7bbe"))+
  scale_y_continuous(limits = c(0,101), expand = c(0, 0))+
  theme_classic()+
  theme(axis.text.x = element_text(color="#000000", size=12, angle=45, margin = margin(20, 0, 0, 0)),
        axis.text.y = element_text(color="#000000", size=12),
        axis.line = element_line(colour = "#000000", size = 1, linetype = "solid"),
        plot.title = element_text(color="#000000", size=14, face="bold"),
        axis.title.x = element_text(color="#000000", size=12, face="bold"),
        axis.title.y = element_text(color="#000000", size=12, face="bold"),
        legend.title = element_text(colour="#000000", size=12, face="bold"))+
  ggtitle("Variants by regions (2021)") +
  ylab("Frequency (%)") + xlab("") + labs(fill='Lineage') 

#combine the two plots
plot_grid(p1, p2, labels=c("A", "B"), ncol = 2, nrow = 1)


#combine 2020 and 2021 data
data_2020_2021=rbind(variants_region_2020,variants_region_2021)

#produce a piechart indicating the proportion of each lineages in 2020 and 2021 in Senegal
PieChart(corrected, data = data_2020_2021,
         fill = c("#ed1665", "#d74699", "#9d4f9f", "#ee1f25","#f16423", "#3b52a4", "#ffc608",
                  "#44afe4", "#d3de25", "#8bc63f", "#4ad819","#067e04", "#004406", "#6ec59d","#3f7bbe"),
         color = "white", hole=.7, lwd = 2, lty = 1, main = NULL,
         clockwise=TRUE, values_size=.7, labels_cex=.8)

#update data at the month scale
my_data$year=""
for(i in 1:nrow(my_data)){
  if(my_data$Collection_date[i]>="2020-03-01" && my_data$Collection_date[i]<="2020-03-31"){
    my_data$year[i]="2020-03-01"
  }else if(my_data$Collection_date[i]>="2020-04-01" && my_data$Collection_date[i]<="2020-04-30"){
    my_data$year[i]="2020-04-01"
  }else if(my_data$Collection_date[i]>="2020-05-01" && my_data$Collection_date[i]<="2020-05-31"){
    my_data$year[i]="2020-05-01"
  }else if(my_data$Collection_date[i]>="2020-06-01" && my_data$Collection_date[i]<="2020-06-30"){
    my_data$year[i]="2020-06-01"
  }else if(my_data$Collection_date[i]>="2020-07-01" && my_data$Collection_date[i]<="2020-07-31"){
    my_data$year[i]="2020-07-01"
  }else if(my_data$Collection_date[i]>="2020-08-01" && my_data$Collection_date[i]<="2020-08-31"){
    my_data$year[i]="2020-08-01"
  }else if(my_data$Collection_date[i]>="2020-09-01" && my_data$Collection_date[i]<="2020-09-30"){
    my_data$year[i]="2020-09-01"
  }else if(my_data$Collection_date[i]>="2020-10-01" && my_data$Collection_date[i]<="2020-10-31"){
    my_data$year[i]="2020-10-01"
  }else if(my_data$Collection_date[i]>="2020-11-01" && my_data$Collection_date[i]<="2020-11-30"){
    my_data$year[i]="2020-11-01"
  }else if(my_data$Collection_date[i]>="2020-12-01" && my_data$Collection_date[i]<="2020-12-31"){
    my_data$year[i]="2020-12-01"
  }else if(my_data$Collection_date[i]>="2021-01-01" && my_data$Collection_date[i]<="2021-01-31"){
    my_data$year[i]="2021-01-01"
  }else if(my_data$Collection_date[i]>="2021-02-01" && my_data$Collection_date[i]<="2021-02-28"){
    my_data$year[i]="2021-02-01"
  }else if(my_data$Collection_date[i]>="2021-03-01" && my_data$Collection_date[i]<="2021-03-31"){
    my_data$year[i]="2021-03-01"
  }else if(my_data$Collection_date[i]>="2021-04-01" && my_data$Collection_date[i]<="2021-04-30"){
    my_data$year[i]="2021-04-01"
  }else if(my_data$Collection_date[i]>="2021-05-01" && my_data$Collection_date[i]<="2021-05-31"){
    my_data$year[i]="2021-05-01"
  }else if(my_data$Collection_date[i]>="2021-06-01" && my_data$Collection_date[i]<="2021-06-30"){
    my_data$year[i]="2021-06-01"
  }else if(my_data$Collection_date[i]>="2021-07-01" && my_data$Collection_date[i]<="2021-07-31"){
    my_data$year[i]="2021-07-01"
  }else if(my_data$Collection_date[i]>="2021-08-01" && my_data$Collection_date[i]<="2021-08-31"){
    my_data$year[i]="2021-08-01"
  }else if(my_data$Collection_date[i]>="2021-09-01" && my_data$Collection_date[i]<="2021-09-30"){
    my_data$year[i]="2021-09-01"
  }else if(my_data$Collection_date[i]>="2021-10-01" && my_data$Collection_date[i]<="2021-10-31"){
    my_data$year[i]="2021-10-01"
  }else if(my_data$Collection_date[i]>="2021-11-01" && my_data$Collection_date[i]<="2021-11-30"){
    my_data$year[i]="2021-11-01"
  }else if(my_data$Collection_date[i]>="2021-12-01" && my_data$Collection_date[i]<="2021-12-31"){
    my_data$year[i]="2021-12-01"
  }else{
    my_data$year[i]=NA
  }
}

#remove uncomplete data
my_data = my_data[complete.cases(my_data), ]

#group by lineages
variants = my_data %>%
  group_by(Pangolin) %>%
  summarise(count=n())

#mark as "other variants" the lineages found at low frequency
for(i in 1:nrow(my_data)){
  for(j in 1:nrow(variants)){
    if(my_data$Pangolin[i]==variants$Pangolin[j]){
      if(variants$count[j]<10){
        my_data$corrected[i]="Other Variants"
      }else{
        my_data$corrected[i]=my_data$Pangolin[i]
      }
    }
  }
}

#calculate the monthly proportion of each lineage
my_final_data = my_data %>%
  group_by(year,corrected) %>%
  summarise(count=n()) %>%
  mutate(prop=count/sum(count)*100)

#plot the proportion of each lineage for each month in 2020 and 2021
ggplot(data=my_final_data, aes(x=year, y=prop, fill=corrected)) +
  geom_bar(stat="identity")+
  scale_y_continuous(limits = c(0,101), expand = c(0, 0))+
  scale_fill_manual(values=c("#f1a9f0","#d7c0d7","#ed1665","#d74699",
                             "#563856","#9d4f9f","#ee1f25","#f16423",
                             "#3b52a4","#44afe4","#ffc608","#d3de25",
                             "#8bc63f","#4ad819","#067e04","#004406",
                             "#6ec59d","#3f7bbe"))+
  theme_classic()+
  theme(axis.text.x = element_text(color="#000000", size=12, angle=45, margin = margin(20, 0, 0, 0)),
        axis.text.y = element_text(color="#000000", size=12),
        axis.line = element_line(colour = "#000000", size = 1, linetype = "solid"),
        plot.title = element_text(color="#000000", size=14, face="bold"),
        axis.title.x = element_text(color="#000000", size=12, face="bold"),
        axis.title.y = element_text(color="#000000", size=12, face="bold"),
        legend.title = element_text(colour="#000000", size=12, face="bold"))+
  ylab("Frequency (%)") + xlab("") + labs(fill='Lineage') 

#count the number of samples for each nextclade lineage
variants_main = my_data %>%
  group_by(Clade) %>%
  summarise(count=n())

#mark as "other variants" the lineages found at low frequency
for(i in 1:nrow(my_data)){
  for(j in 1:nrow(variants_main)){
    if(my_data$Clade[i]==variants_main$Clade[j]){
      if(variants_main$count[j]<10){
        my_data$corrected2[i]="Other Variants"
      }else{
        my_data$corrected2[i]=my_data$Clade[i]
      }
    }
  }
}

#calculate the monthly proportion of each nexstrain lineage in 2020 and 2021
my_final_data_clade = my_data %>%
  group_by(year,corrected2) %>%
  summarise(count=n()) %>%
  mutate(prop=count/sum(count)*100)


#####GENETIC DIVERSITY#####

#retain only samples that match with our previous analysis
my_nexclade = nextclade[nextclade$Accession_ID %in% my_data$Accession_ID,]

#keep only the identifiers, the lineages and the total number of substitutions
my_nexclade = my_nexclade[,c(1,2,3,4,7)]

#combine the metadata with the number of substitutions
merging = merge(my_nexclade, my_data, by="Accession_ID")

#generate boxplots showing the total number of substitutions for each pango lineage 
plot_subs = ggplot(merging)+
  geom_boxplot(aes(x=corrected, y=totalSubstitutions, fill=corrected))+
  ylim(0,50)+
  scale_fill_manual(values=c("#f1a9f0","#d7c0d7","#ed1665","#d74699",
                             "#563856","#9d4f9f","#ee1f25","#f16423",
                             "#3b52a4","#44afe4","#ffc608","#d3de25",
                             "#8bc63f","#4ad819","#067e04","#004406",
                             "#6ec59d","#3f7bbe"))+
  theme(axis.line.x = element_line(size = 1, colour = "black"),
        axis.line.y = element_line(size = 1, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text=element_text(size = 16, family="Arial"),
        axis.text.x=element_text(colour="black", size = 16, angle=45),
        axis.text.y=element_text(colour="black", size = 16),
        legend.position = "none")

plot_subs

#reinitialize the nextclade table
my_nexclade = nextclade[nextclade$Accession_ID %in% my_data$Accession_ID,]
my_nexclade = as.data.frame(my_nexclade)

#initialize a table that will indicate the mutations for each viral gene
list_mut = data.frame(sample=character(), gene=character(), mutation=character())

#identify for each sample the different mutations for each viral gene
for(i in 1:nrow(my_nexclade)){
  print(i)
  if(!is.na(my_nexclade$aaSubstitutions[i])){
    nextclade_sample = my_nexclade$Accession_ID[i]
    nextclade_mut = my_nexclade$aaSubstitutions[i]
    #each gene is separared with this expression
    my_mut = strsplit(nextclade_mut, ":|,")
    for(j in 1:length(my_mut[[1]])){
      if(my_mut[[1]][j]=="ORF1a"){
        my_gene = "ORF1a"
      }else if(my_mut[[1]][j]=="ORF1b"){
        my_gene = "ORF1b"
      }else if(my_mut[[1]][j]=="M"){
        my_gene = "M"
      }else if(my_mut[[1]][j]=="N"){
        my_gene = "N"
      }else if(my_mut[[1]][j]=="E"){
        my_gene = "E"
      }else if(my_mut[[1]][j]=="ORF3a"){
        my_gene = "ORF3a"
      }else if(my_mut[[1]][j]=="ORF6"){
        my_gene = "ORF6"
      }else if(my_mut[[1]][j]=="ORF7a"){
        my_gene = "ORF7a"
      }else if(my_mut[[1]][j]=="ORF7b"){
        my_gene = "ORF7b"
      }else if(my_mut[[1]][j]=="ORF8"){
        my_gene = "ORF8"
      }else if(my_mut[[1]][j]=="ORF9b"){
        my_gene = "ORF9b"
      }else if(my_mut[[1]][j]=="S"){
        my_gene = "S"
      }
        list_mut[nrow(list_mut)+1,]=list(nextclade_sample, my_gene, my_mut[[1]][j])
    }
  }
}

#cleaning step to remove "non-mutation"
for(i in 1:nrow(list_mut)){
  if(list_mut$gene[i]==list_mut$mutation[i]){
    list_mut$clean[i]="to_remove"
  }else{
    list_mut$clean[i]="ok"
  }
}
list_mut=list_mut[list_mut$clean=="ok",]

#count the occurence of each mutation
mutations_freq = list_mut %>%
  group_by(gene, mutation) %>%
  summarize(freq=n())

#at least 5%
#78 samples corresponds to 5% of the total
mutations_freq = mutations_freq[mutations_freq$freq >= 78,]
#extract the position
mutations_freq$number = extract_numeric(mutations_freq$mutation)
#retrieve and order mutations per gene
mutations_freq_M = mutations_freq[mutations_freq$gene == "M",]
mutations_freq_M = mutations_freq_M[order(mutations_freq_M$number),]
mutations_freq_1a = mutations_freq[mutations_freq$gene == "ORF1a",]
mutations_freq_1a = mutations_freq_1a[order(mutations_freq_1a$number),]
mutations_freq_1b = mutations_freq[mutations_freq$gene == "ORF1b",]
mutations_freq_1b = mutations_freq_1b[order(mutations_freq_1b$number),]
mutations_freq_N = mutations_freq[mutations_freq$gene == "N",]
mutations_freq_N = mutations_freq_N[order(mutations_freq_N$number),]
mutations_freq_E = mutations_freq[mutations_freq$gene == "E",]
mutations_freq_E = mutations_freq_E[order(mutations_freq_E$number),]
mutations_freq_3a = mutations_freq[mutations_freq$gene == "ORF3a",]
mutations_freq_3a = mutations_freq_3a[order(mutations_freq_3a$number),]
mutations_freq_6 = mutations_freq[mutations_freq$gene == "ORF6",]
mutations_freq_6 = mutations_freq_6[order(mutations_freq_6$number),]
mutations_freq_7a = mutations_freq[mutations_freq$gene == "ORF7a",]
mutations_freq_7a = mutations_freq_7a[order(mutations_freq_7a$number),]
mutations_freq_7b = mutations_freq[mutations_freq$gene == "ORF7b",]
mutations_freq_7b = mutations_freq_7b[order(mutations_freq_7b$number),]
mutations_freq_8 = mutations_freq[mutations_freq$gene == "ORF8",]
mutations_freq_8 = mutations_freq_8[order(mutations_freq_8$number),]
mutations_freq_9b = mutations_freq[mutations_freq$gene == "ORF9b",]
mutations_freq_9b = mutations_freq_9b[order(mutations_freq_9b$number),]
mutations_freq_S = mutations_freq[mutations_freq$gene == "S",]
mutations_freq_S = mutations_freq_S[order(mutations_freq_S$number),]

#combine and order data
order_mut = rbind(mutations_freq_1a, mutations_freq_1b, mutations_freq_S,
                  mutations_freq_3a, mutations_freq_E, mutations_freq_M,
                  mutations_freq_7a, mutations_freq_7b, mutations_freq_8,
                  mutations_freq_N, mutations_freq_9b)
order_mut$mutation <- factor(order_mut$mutation, levels = unique(order_mut$mutation))

#calculate the frequency of each mutation
order_mut$frequency <- order_mut$freq/nrow(my_data)*100

#plot the frequency of the mutations for each viral gene
plot_mut <- ggplot(data=order_mut, aes(x=mutation, y=frequency, fill=gene, width=.8)) +
  geom_bar(stat="identity")+
  ylim(0,100)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  theme(axis.line.x = element_line(size = 1, colour = "black"),
        axis.line.y = element_line(size = 1, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text=element_text(size = 16, family="Arial"),
        axis.text.x=element_text(colour="black", size = 7),
        axis.text.y=element_text(colour="black", size = 16),
        legend.position = "right")

#show the plot 
plot_mut

# #Unused version: frequency of mutations along the genome
# #initialize a dataframe
# list_mut2 = data.frame(sample=character(), mutation=integer())
# 
# #find the mutation at the nucleotide level
# for(i in 1:nrow(my_nexclade)){
#   print(i)
#   if(!is.na(my_nexclade$substitutions[i])){
#     nextclade_sample = my_nexclade$Accession_ID[i]
#     nextclade_mut = my_nexclade$substitutions[i]
#     my_mut = strsplit(nextclade_mut, ",")
#     for(j in 1:length(my_mut[[1]])){
#       nucl = parse_number(my_mut[[1]][j])
#       list_mut2[nrow(list_mut2)+1,]=list(nextclade_sample, nucl)
#     }
#   }
# }
# 
# #count the number of mutation for each nucleotide position
# mutations_freq2 = list_mut2 %>%
#   group_by(mutation) %>%
#   summarize(freq=n())
# 
# #calculate the frequency
# mutations_freq2$frequency = mutations_freq2$freq/nrow(my_data)*100
# 
# #indicate 0 for all nucleotide position with no mutation, then order
# ##29903 size of wuhan sars-cov-2
# for(i in 1:29903){
#   if(i %in% mutations_freq2$mutation){
#     #nothing
#   }else{
#     mutations_freq2[nrow(mutations_freq2)+1,]=list(i,0,0)
#   }
# }
# mutations_freq2 = mutations_freq2[order(mutations_freq2$mutation),]
# 
# plot_mut_gen <- ggplot(data=mutations_freq2, aes(x=mutation, y=frequency)) +
#   geom_line()+
#   theme_classic()
# plot_mut_gen

# #same analysis, but using a sliding window
# window_size=data.frame(window=integer(),frequency=integer())
# 
# ####size of 100 nucl, pas de 50 nucl
# for(i in 1:nrow(mutations_freq2)){
#   calcul=0
#   if(i==1){
#     calcul = sum(mutations_freq2$frequency[i:(i+99)])/100
#     window_size[nrow(window_size)+1,]=list(i, calcul)
#   }
#   if(i%%50==0){
#     calcul = sum(mutations_freq2$frequency[i:(i+99)])/100
#     window_size[nrow(window_size)+1,]=list(i, calcul)
#   }
# }
# 
# plot_sliding <- ggplot(data=window_size, aes(x=window, y=frequency)) +
#   geom_line()+
#   theme_classic()
# plot_sliding

#initialize  dataframe for spike mutations
list_mut3 = data.frame(sample=character(), gene=character(), mutation=character(), lineage=character(), pango=character())

#detect mutation in S for each sample
for(i in 1:nrow(my_nexclade)){
  print(i)
  if(!is.na(my_nexclade$aaSubstitutions[i])){
    nextclade_sample = my_nexclade$Accession_ID[i]
    nextclade_mut = my_nexclade$aaSubstitutions[i]
    nextclade_pango = my_nexclade$Nextclade_pango[i]
    nextclade_clade = my_nexclade$clade[i]
    my_mut = strsplit(nextclade_mut, ":|,")
    for(j in 1:length(my_mut[[1]])){
      if(my_mut[[1]][j]=="S"){
        my_gene = "S"
        list_mut3[nrow(list_mut3)+1,]=list(nextclade_sample, my_gene, my_mut[[1]][j+1], nextclade_clade, nextclade_pango)
      }
    }
  }
}

#count the occurence of each mutation
mutations_freq3 = list_mut3 %>%
  group_by(pango, mutation) %>%
  summarize(freq=n())

#retain only mutations observed in at least five samples
mutations_freq3 = mutations_freq3[mutations_freq3$freq>=5,]

#produce a dataframe that list the occurence of mutations for each pango lineage
prop_sample_variants = as.data.frame(table(my_nexclade$Nextclade_pango))

#combine tables to calculate the ratio (frequency) of each mutation in a given pango lineage
for(i in 1:nrow(mutations_freq3)){
  my_lineage=mutations_freq3$pango[i]
  for(j in 1:nrow(prop_sample_variants)){
    total_lineage=as.character(prop_sample_variants$Var1[j])
    if(my_lineage==total_lineage){
      mutations_freq3$ratio[i]=(((mutations_freq3$freq[i])/(prop_sample_variants$Freq[j]))*100)
    }
  }
}

#import data.table for dcast and other functions
library(data.table)

#produce a matrix of mutations for each lineage, and order the mutations
matrix_freq = dcast(mutations_freq3, pango ~ mutation, value.var="ratio")
matrix_freq = matrix_freq[,order(parse_number((colnames(matrix_freq))))]

#save the matrix, which was then imported in microsoft excel
write.table(matrix_freq, file="mutation_pango_lineages.txt", col.names = T, sep="\t", row.names = F)