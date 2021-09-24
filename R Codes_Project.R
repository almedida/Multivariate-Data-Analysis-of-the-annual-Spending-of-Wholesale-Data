


#### Importing Relevant Packages


library(plotly)
library(dplyr)
library(ggplot2)
library(dendextend)
library(ggfortify)
library(car)
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
library(mclust)
library(cluster)
library(fpc)
library(nFactors)
library(FactoMineR)
library(factoextra)


### Importing Dataset and Data Processing

library(readr)
Wholesale <- read_csv("~/Documents/Final Submission/Wholesale.csv")
#Wholesale <- read.csv("C:/Users/Chamari/Desktop/project 764/Wholesale.csv")
attach(Wholesale)
names(Wholesale)

# Change the data type of Channel and Region

Channel = as.factor(Channel)
Region  = as.factor(Region)

dfnew <- data.frame(Fresh,Milk,Grocery,Frozen,Detergents_Paper,Delicassen)

cdata <- scale(dfnew) # Standardizing variables

## Summary statistics

summary(dfnew)
sd(Fresh)
sd(Milk)
sd(Grocery)
sd(Frozen)
sd(Detergents_Paper)
sd(Delicassen)

Channel = as.factor(Channel)
Region  = as.factor(Region)

levels(Channel)=c("Customer","Retail")
levels(Region)=c("Lisnon","Oporto","Other")


## Graphical representation of variables



boxplot(dfnew,main="Boxplots of all the variables",xlab="Variable",ylab="Value")

par(mfrow = c( 2,3 ))
boxplot(Fresh~Channel,xlab="Channel",ylab="Fresh")
boxplot(Milk~Channel,xlab="Channel",ylab="Milk")
boxplot(Grocery~Channel,xlab="Channel",ylab="Grocery")
boxplot(Frozen~Channel,xlab="Channel",ylab="Frozen")
boxplot(Detergents_Paper~Channel,xlab="Channel",ylab="Detergent Paper")
boxplot(Delicassen~Channel,xlab="Channel",ylab="Delicassen")




## Summary statistics by grouping variables

tapply(Fresh, Channel, summary)
tapply(Milk, Channel, summary)
tapply(Grocery, Channel, summary)
tapply(Frozen, Channel, summary)
tapply(Detergents_Paper, Channel, summary)
tapply(Delicassen, Channel, summary)


tapply(Fresh, Channel, sd)
tapply(Milk, Channel, sd)
tapply(Grocery, Channel, sd)
tapply(Frozen, Channel, sd)
tapply(Detergents_Paper, Channel, sd)
tapply(Delicassen, Channel, sd)

#### Scatter Plot Matrix of Variables

X <- Wholesale[,-c(1,2)]

clPairs(X, Channel)

# Number of observations in each channel

table(Channel)


# Checking Multivariate Normality

library(MVN)
result <- mvn(data = dfnew, mvnTest = "royston", univariatePlot = "qqplot")
# create univariate histograms
result <- mvn(data = dfnew, mvnTest = "royston", univariatePlot = "histogram")



### Checking Covariance and Correlation Structure of Variables


( S = cov(dfnew))
( R = cor(dfnew))



#### Obtaining Eigen Values and Eigen Vectors


## This will calculate the proportion of variance explained by each principal components.

( r.eigen <- eigen(R) )

for (r in r.eigen$values) {
  print(r / sum(r.eigen$values))
}



 ### Principal Component Analysis



# PCA using correlation matrix

pca <- princomp(cdata,cor = TRUE,scores = T)
summary(pca)
pca$loadings



#### Scree Plot 

plot(pca,type="lines")




#### Plotting PC1 and PC2


pca.plot1 <- autoplot(pca, data = Wholesale, colour = 'Channel')
pca.plot1


pca.plot2 <- autoplot(pca, data = Wholesale, colour = 'Region')
pca.plot2



 pca.scaled <- prcomp(dfnew, center=TRUE,scale = TRUE)
 pca.scaled$rotation
  
## Biplot of PCs
 
 # In the plots we have projecting vectors of each variables. Their projected values on each PC show how much weight 
 # they have on that PC.Also, the angle between two vectors indicates how correlate those two variables are.
 # Smaller angles represent the larger correlation between variables.
 
 
p <- ggbiplot(pcobj = pca.scaled, 
              scale = 0,
              alpha = 0,groups='Channel' )
p1 <- p + geom_point(aes(color = Channel ))+scale_y_reverse() +scale_x_reverse()


ggplotly(p1)


 ### Cluster Analysis


########### splitting data into test/train #########################

train <- Wholesale[1:300,]
test <- Wholesale[301:440,]
fit0 <- kmeans(train[,-1],3,nstart = 3, iter.max = 10)
fit0

km.res0 <- eclust(test[,-1], "kmeans", k=3, nstart = 3, iter.max = 10, graph = FALSE)
km.res0

km.res0$cluster
test$Channel==km.res0$cluster
table(test$Channel==km.res0$cluster)

mean(test$Channel==km.res0$cluster)
mean(test$Channel!=km.res0$cluster)

###############################################################

#### K-Means Cluster Analysis


set.seed(123)
# K-Means Clustering with 5 clusters
fit1 <- kmeans(cdata,5,nstart = 10,iter.max = 10)
fit1

set.seed(123)
km.res <- eclust(cdata,"kmeans",k=5,nstart = 5,iter.max = 10,graph = FALSE)
km.res
fviz_cluster(km.res, geom = "point", frame.type = "norm")



#### Obtaining Cluster memberships when k=5



(cm1 <- unique(cbind(value=Wholesale, clust=km.res$cluster)))
t1<- table(cm1$value.Channel,cm1$value.Region,cm1$clust)
colnames(t1)=c("Region1","Region2","Region3")
rownames(t1)=c("Channel1","Channel2")
t1
write.csv(cm1,"clus_km5.csv") # The clustermembership file will be written in your working directory


# K-Means Clustering with 3 clusters
set.seed(123)
fit2 <- kmeans(cdata,3,nstart = 3, iter.max = 10)
fit2

set.seed(123)
km.res1 <- eclust(cdata,"kmeans",k=3,nstart = 3,iter.max = 10,graph = FALSE)
km.res1
fviz_cluster(km.res1, geom = "point", frame.type = "norm")


#### Obtaining Cluster memberships when k=3


(cm2 <- unique(cbind(value=Wholesale, clust=km.res1$cluster)))
t2<- table(cm2$value.Channel,cm2$value.Region,cm2$clust)
colnames(t2)=c("Region1","Region2","Region3")
rownames(t2)=c("Channel1","Channel2")
t2
write.csv(cm2,"clus_km31.csv")



### Hierachical Cluster Analysis


#### Dendrogram for the Cluster Analysis Using Ward's Method using Observed data. 


res.hc <- eclust(cdata, "hclust", k = 5,
                 method = "ward.D2", graph = FALSE) 
obsclus <- cutree(res.hc, k = 5)

# Dendrogram
fviz_dend(res.hc, rect = FALSE, as.ggplot=TRUE,show_labels = FALSE) + labs(title = "Cluster Dendrogram for Ward's Method using Observed Data" ) #palette="jco", 

#### Obtaining Cluster memberships using Ward's method (Used Observed data)


(cm3 <- unique(cbind(value=Wholesale, clust=obsclus)))
t3<- table(cm3$value.Channel,cm3$value.Region,cm3$clust)
colnames(t3)=c("Region1","Region2","Region3")
rownames(t3)=c("Channel1","Channel2")
t3
write.csv(cm3,"clus_ward.csv")


#### Dendrogram for the Cluster Analysis Using Average linkage Method using Observed data. 
# Enhanced hierarchical clustering

res.hc1 <- eclust(cdata, "hclust", k = 3,
                  method = "average", graph = FALSE) 
obsclus1 <- cutree(res.hc1, k = 3)

fviz_dend(res.hc1, rect = FALSE, as.ggplot=TRUE,show_labels = TRUE) + labs(title = "Cluster Dendrogram for Average Linkage Method using Observed Data" ) #palette="jco", 

#### Obtaining Cluster memberships using Average linkage method (Used Observed data)


(cm31 <- unique(cbind(value=Wholesale, clust=obsclus1)))
t31<- table(cm31$value.Channel,cm31$value.Region,cm31$clust)
colnames(t31)=c("Region1","Region2","Region3")
rownames(t31)=c("Channel1","Channel2")
t31
write.csv(cm31,"clus_avg.csv")




#### Dendrogram for the Cluster Analysis Using Ward Method Using PCA Scores. 


# clusters using ward method

pcaWS <- eclust(pca$scores, "hclust", k = 5,
                method = "ward.D2", graph = FALSE)
# cut the dendrogram into 5 clusters
WSClusters <- cutree(pcaWS, k = 5)

# Dendrogram
fviz_dend(pcaWS, rect = FALSE, as.ggplot=TRUE,show_labels = TRUE) + labs(title = "Cluster Dendrogram for Ward's Method using PC Scores" ) #palette="jco", 



# add cluster to data frame of scores
WSDf <- data.frame(pca$scores, "cluster" = factor(WSClusters))
WSDf <- transform(WSDf, cluster_name = paste("Cluster",WSClusters))



#### Obtaining Cluster memberships using Ward's method (Used PCA scores)


(cm4 <- unique(cbind(value=Wholesale, clust=WSClusters)))
t4<- table(cm4$value.Channel,cm4$value.Region,cm4$clust)
colnames(t4)=c("Region1","Region2","Region3")
rownames(t4)=c("Channel1","Channel2")
t4




p3 <- plot_ly(WSDf, x = WSDf$Comp.1 , y = WSDf$Comp.2, text = rownames(WSDf),
              mode = "markers", color = WSDf$cluster_name, marker = list(size = 11)) 

p3 <- layout(p3, title = "PCA Clusters from Hierarchical Clustering Using Ward's Method", 
             xaxis = list(title = "PC 1"),
             yaxis = list(title = "PC 2"))

p3




dend <- pca$scores %>%
  dist() %>%
  hclust(method = "ward.D") %>%
  as.dendrogram()

## Obtain 5 clusters
dend2 <- color_branches(dend, 5)

p <- ggplot(dend2, horiz = FALSE, offset_labels = -3)+ggtitle("Dendogram Using Ward's Method (PCA scores used")
ggplotly(p)


### Finding Optimal Number of Cluster Solutions

#Source: https://www.datanovia.com/en/lessons/determining-the-optimal-number-of-clusters-3-must-know-methods/


# Compute the number of clusters

library(NbClust)
nb1 <- NbClust(cdata, distance = "euclidean", min.nc = 2,
        max.nc = 6, method = "ward.D", index ="all")
# Visualize the result
library(factoextra)
fviz_nbclust(nb1)+ theme_minimal()+labs(subtitle="Ward's Method")



nb2 <- NbClust(cdata, distance = "euclidean", min.nc = 2,
        max.nc = 6, method = "average", index ="all")
# Visualize the result
fviz_nbclust(nb2) + theme_minimal()+labs(subtitle="Average Linkage Method")


nb3 <- NbClust(cdata, distance = "euclidean", min.nc = 2,
        max.nc = 6 , method = "kmeans", index ="all")
# Visualize the result
fviz_nbclust(nb3) + theme_minimal()+labs(subtitle="Kmeans Method")



### Factor Analysis


library(nFactors)
library(FactoMineR)
library(psych)
library(GPArotation)

# Varimax rotation

fit_f <- fa(cdata, 2,440, rotate="varimax")
fit_f



### Communality Estimates



fit_f$communalities



### Uniqueness/Specific variance Estimates



fit_f$uniquenesses



### Factor Loadings


load <-fit_f$loadings[,1:2]
load

 


#### Plotting factor 1 by factor 2



plot(load,type="p",col="blue",pch=16,xlab = "Factor 1", ylab="Factor 2", main="Graphical Representation of Factors") # set up plot
abline(h=0,v=0,lty=2)
text(load,labels=rownames(load),cex=.7) # add variable names

