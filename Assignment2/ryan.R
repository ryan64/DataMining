install.packages("fpc")
library(fpc)
library(cluster)

whitewine_table <- read.table("/home/ryan/Downloads/winequality-white.csv", header = TRUE, sep = ";", col.names = c("a","b","c","d","e","f","g","h","i","j","k","l"))
complex9_rn32 <- read.table("/home/ryan/Downloads/Complex9_RN32.txt", header = FALSE, sep = ",")

firstdframe<-data.frame(a=whitewine_table[,1],b=whitewine_table[,2],c=whitewine_table[,3],d=whitewine_table[,4],e=whitewine_table[,5],f=whitewine_table[,6],g=whitewine_table[,7],h=whitewine_table[,8],i=whitewine_table[,9],j=whitewine_table[,10],k=whitewine_table[,11],l=whitewine_table[,12])
dframe<-data.frame(a=firstdframe[,1],b=firstdframe[,2],c=firstdframe[,3],d=firstdframe[,4],e=firstdframe[,5],f=firstdframe[,6], g=firstdframe[,7],h=firstdframe[,8],i=firstdframe[,9],j=firstdframe[,10],k=firstdframe[,11])
scaledframe<-scale(dframe)
dataframe<-data.frame(a=scaledframe[,1],b=scaledframe[,2],c=scaledframe[,3],d=scaledframe[,4],e=scaledframe[,5],f=scaledframe[,6], g=scaledframe[,7],h=scaledframe[,8],i=scaledframe[,9],j=scaledframe[,10],k=scaledframe[,11],l=firstdframe[,12])

#Question 0
dataframe$m = dataframe$l
dataframe$m[dataframe$m >= 8] = "A"
dataframe$m[dataframe$m == 7] = "B"
dataframe$m[dataframe$m == 6] = "C"
dataframe$m[dataframe$m == 5] = "D"
dataframe$m[dataframe$m <= 4] = "E"

#Question 1
entropyvec <- function(vec) {
  s <- sum(vec)
  
  totalh <- Reduce(function(acc, elem) {
    prob <- elem / s
    h <- if (prob == 0) 0 else prob * log2(prob)
    return (acc - h)
  }, vec, 0)
  
  return (totalh)
}

entropy <- function(clusterassignment, groundtruth) {
  lvl<-factor(clusterassignment)
  clusterlevels <- levels(lvl)
  clusterclass <- table(clusterassignment, groundtruth)
  
  n <- nrow(clusterclass)
  entropy_weight <- array(dim=c(n, 2))
  population <- 0
  
  for (i in 1:n) {
    clustersize <- sum(clusterclass[i,])
    population <- population + clustersize
    entropy_weight[i,] = c(entropyvec(clusterclass[i,]), clustersize)
  }
  
  percentage <- 0
  if (clusterlevels[1] == "0") {
    outliers <- sum(clusterclass[1,])
    percentage <- outliers / population
    population <- population - outliers
    entropy_weight[1,] = c(0, 0)
  }
  
  totalh <- 0
  if (population > 0) {
    for (i in 1:n) {
      totalh <- totalh + entropy_weight[i, 1] * entropy_weight[i, 2] / population
    }
  }
  
  return (c(totalh, percentage))
}

#Question 2
ordinalVariation <- function(clusters, classes){
       ord_classes <- ordered(classes,levels = c('E', 'D', 'C', 'B', 'A'),labels <- c(0, 1, 2, 3, 4))
       clusterlevels <- levels(ord_classes)
       var_weight <- array(dim = c(length(clusterlevels), 2))
       population <- 0
       counter <- 1
       if (clusterlevels[1] == 0) {
             counter <- 2
             var_weight[1, ] <- c(0, 0)
         }
       for (i in counter:length(clusterlevels)) {
             cluster <- ord_classes[clusters == clusterlevels[i]]
             population <- population + length(cluster)
             if (length(cluster) <= 1) {
                   var_weight[i, ] <- c(0, length(cluster))
               } else {
                     sum <- 0
                     for (j in cluster) {
                           for (k in cluster) {
                                 sum <- sum + abs(as.numeric(j) - as.numeric(k))
                             }
                       }
                     dis_numerator <- sum
                     dis_denom <- length(cluster) * length(cluster) - length(cluster)
                     clust_var <- dis_numerator / dis_denom
                     var_weight[i, ] <- c(clust_var, length(cluster))
                 }
         }
       total <- 0
       for (i in 1:length(clusterlevels)) {
             total <- total + var_weight[i, 1] * var_weight[i, 2] / population
         }
        return(total)
}

#Question 3
variance <- function(clusterassignment, numerical) {
  lvl<-factor(clusterassignment)
  clusterlevels <- levels(lvl)
  n <- length(clusterlevels)
  var_weight <- array(dim=c(n, 2))
  population <- 0
  b <- 1
  outliers <- 0
  
  if (clusterlevels[1] == "0") {
    b <- 2
    var_weight[1,] <- c(0, 0)
    outliers <- length(numerical[clusterassignment == "0"])
  }
  
  for (i in b:n) {
    cluster <- numerical[clusterassignment==clusterlevels[i]]
    population <- population + length(cluster)
    if (length(cluster) == 1) {
      var_weight[i,] <- c(0, length(cluster))
    } else {
      var_weight[i,] <- c(var(cluster), length(cluster))
    }
  }
  
  percentage <- outliers / (outliers + population)
  
  totalv <- 0
  for (i in 1:n) {
    totalv <- totalv + var_weight[i, 1] * var_weight[i, 2] / population
  }
  return (c(totalv, percentage))
}

#Question 4
mdist<-function(d)
{
  scaled<-scale(d)
  
  D<-dist(scaled, method = "manhattan")
  
  return (D)
}

#Question 5
km9_1 <- kmeans(complex9_rn32, 9)
plotcluster(complex9_rn32, km9_1$cluster,  9)
km9_2 <- kmeans(complex9_rn32, 9)
plotcluster(complex9_rn32, km9_2$cluster, 9)

km18_1 <- kmeans(complex9_rn32, 18)
plotcluster(complex9_rn32, km18_1$cluster, 18)
km18_2 <- kmeans(complex9_rn32, 18)
plotcluster(complex9_rn32, km18_2$cluster, 18)

#Question 6
set.seed(4335)
km5_1 <- kmeans(whitewine_table, 5, nstart = 15)
set.seed(4335)
km10_1 <- kmeans(whitewine_table, 10, nstart = 15)
temp_dataframe = data.frame(fixed_acidity = whitewine_table[,1], volatile_acidity = whitewine_table[,2], citric_acid = whitewine_table[,3], residual_sugar = whitewine_table[,4], chlorides = whitewine_table[,5], free_sulfur_dioxide = whitewine_table[,6], total_sulfur_dioxide = whitewine_table[,7], density = whitewine_table[,8], pH = whitewine_table[,9], sulphates = whitewine_table[,10], alcohol = whitewine_table[,11])
d = mdist(temp_dataframe)
PAM5_1 <- pam(d, 5)
entropy(factor(PAM5_1$clustering), dataframe[,13])
PAM10_1 <- pam(d, 10)
entropy(factor(PAM10_1$clustering), dataframe[,13])

#Question 7

