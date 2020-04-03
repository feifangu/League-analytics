# clustering based on team style
library(data.table)
library(tidyverse)
library(factoextra)
library(NbClust)

RSS_result <- fread("RegularSeasonStats.csv")
RSS <- fread("RegularSeasonStats_2.csv")
WGS <- fread("WorldsGroupsStats.csv")

RSS[,Spring_KPG:=Spring_K/Spring_GP]
RSS[,Spring_DPG:=Spring_D/Spring_GP]
RSS[,Spring_KPM:=Spring_KPG/Spring_AGT]
RSS[,Spring_DPM:=Spring_DPG/Spring_AGT]
RSS[,Spring_CKPM:=(Spring_K+Spring_D)/(Spring_GP*Spring_AGT)]
RSS[,Summer_KPG:=Summer_K/Summer_GP]
RSS[,Summer_DPG:=Summer_D/Summer_GP]
RSS[,Summer_KPM:=Summer_KPG/Summer_AGT]
RSS[,Summer_DPM:=Summer_DPG/Summer_AGT]
RSS[,Summer_CKPM:=(Summer_K+Summer_D)/(Summer_GP*Summer_AGT)]

# model data
names(RSS)
#modeldata <- scale(RSS[,c(6,9:20,24,27:47)])
modeldata <- scale(RSS[,c(6,9:20,24,27:46)])

### decide the best k
# elbow method
fviz_nbclust(modeldata, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(modeldata, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(modeldata, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

### comparing the three methods, the elbow method is the most reasonable one, decide k=4

### clustering
km <- kmeans(modeldata, centers = 4, nstart = 25)
str(km)
rownames(modeldata) <- RSS$V1
fviz_cluster(km, data = modeldata,geom = "text")

RSS$cluster = km$cluster

# clustering result
result <- RSS[,c("V1","cluster")]

fwrite(result,"clustering_result.csv")
modeldata <- as.data.table(modeldata)
modeldata$team <- RSS$V1
modeldata$cluster <- RSS$cluster
fwrite(modeldata,"clustering_modeldata.csv") # feature chosen and scaled
fwrite(RSS,"clustering_modeldata_raw.csv") # pre chosen and scaled data

