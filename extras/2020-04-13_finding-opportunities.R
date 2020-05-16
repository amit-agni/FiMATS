if(!require(pacman)) { install.packages("pacman"); library(pacman)}
p_load(here,data.table,tidyverse,tictoc)
p_load(dendextend)
rm(list = ls())
options(scipen=999) 
if(!require(cutlery)) { devtools::install_github("amit-agni/cutlery"); library(cutlery)}

DT_hist = readRDS(file=here::here("100_data_raw-input","DT_hist.Rds"))
DT_stats = readRDS(file=here::here("100_data_raw-input","DT_stats.Rds"))


#ASX 100
asx100 <- DT_stats[category=='stock' & country == 'Australia']

names(asx100)

cols <- grep('_change_',names(asx100),value = T)
#asx100[,c("symbol",..cols)]

dist <- dist(asx100[,..cols], method = "euclidean")
hc <- hclust(dist,method = "single")
k<-5
plot(color_branches(as.dendrogram(hc),k=k))

asx100$clusters <- cutree(hc, k = k)
table(asx100$clusters)
asx100[, sapply(.SD, function(x) list(mean = mean(x))), .SDcols = cols, by = clusters]

asx100[clusters==4,c("name",..cols)]


##Anomaly detection
p_load(outliers,FNN,dbscan,lattice,cluster)

k<-3
# Calculate the 5 nearest neighbors distance
#symbol != 'APT.AX'
nn <- FNN::get.knn(asx100[,..cols], k = k,algorithm = "CR")

# Create score by averaging distances
avg_dist <- rowMeans(nn$nn.dist)

avg_dist[order(avg_dist)]

# Print row index of the most anomalous point
asx100[which.max(avg_dist)]

asx100[order(-avg_dist)][1:5]


