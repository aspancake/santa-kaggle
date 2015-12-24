# Clustering your trips (Includes 2 Approaches)

library(data.table)
library(ggplot2)
library(ggmap)
library(sqldf)

setwd('...')
gifts <- read.csv('.../gifts.csv')
gifts <- as.data.table(gifts) 

# Setting up shop
north_pole <- c(0, 90)
weight_limit <- 1000
base_weight <- 10
num_gifts <- nrow(gifts)

num_base_clusters <- 50000
seed_value <- 5432
Max <- vector("numeric")
Base <- vector("numeric")
Value <- vector("numeric")

#Kmeans clustering by Longtitude Only
set.seed(seed_value)
clusters <- kmeans(gifts[,3, with=FALSE], num_base_clusters, iter.max = 100)
longs <- order(clusters$centers)
gifts$Clusters <- clusters$cluster

#Weight per Cluster
weight_per_cluster <- gifts[, sum(Weight), by=Clusters]
weight_per_cluster <- weight_per_cluster[match(longs, weight_per_cluster$Clusters)]

#Clean it up
names(weight_per_cluster)[names(weight_per_cluster)=='V1'] <- 'total_weight'
names(weight_per_cluster)[names(weight_per_cluster)=='Clusters'] <- 'cluster_id'
WPC <- weight_per_cluster
rm(weight_per_cluster)


#Method 1 of calculating clusters - Comparing relative WRW values before joining
colnames(gifts)[5] <- "TripId"
i <- 1
j <- 1
while (i <= num_base_clusters){
  opt_1 <- 2
  opt_2 <- 1
  while ((WPC$total_weight[i] + WPC$total_weight[i+j] + base_weight < weight_limit) & (opt_2<opt_1)){
    Calc_WRW(gifts[gifts$TripId == WPC$cluster_id[i]])
    opt_1_1 <- Score
    Calc_WRW(gifts[gifts$TripId == WPC$cluster_id[i+j]])
    opt_1_2 <- Score
    opt_1 <- opt_1_1 + opt_1_2
    ay <- gifts[gifts$TripId == (WPC$cluster_id[i])]
    ay <- rbind(ay,gifts[gifts$TripId == WPC$cluster_id[i+j]])
    ay <- setorder(ay,-Latitude)
    ay$TripId <- WPC$cluster_id[i]
    Calc_WRW(ay)
    opt_2 <- Score
    if (opt_2 < opt_1){
      gifts$TripId[gifts$TripId == WPC$cluster_id[i+j]] <- WPC$cluster_id[i]
      WPC$total_weight[i] <- WPC$total_weight[i] + WPC$total_weight[i+j]
      j <- j + 1
    }
    if (is.na(WPC$total_weight[i+j])) break
  }
  i <- i + j
  j <- 1
}

rm(gifts)
rm(WPC)

#Method 2: Combine consecutive clusters below weight limit
i <- 1
n <- 1
while (i <= num_base_clusters-1){
  if (!is.na(WPC$total_weight[i+n])){
    while (WPC$total_weight[i] + WPC$total_weight[i+n] + base_weight < weight_limit){
      gifts$Clusters[gifts$Clusters == WPC$cluster_id[i+n]] <- WPC$cluster_id[i]
      WPC$total_weight[i] <- WPC$total_weight[i] + WPC$total_weight[i+n]
      n <- n + 1
      if (is.na(WPC$total_weight[i+n])) break
      if (n > max_join) break
    }
  }
  i <- i + n
  n <- 1
}

submission <- setorder(gifts, -Latitude)
rm(gifts)
rm(WPC)

#Put submission in format for Kaggle Submission 
#Do not use if planning on optimizing!
submission <- submission[, .(GiftId, TripId)]
colnames(submission)[2] <- "TripId"



