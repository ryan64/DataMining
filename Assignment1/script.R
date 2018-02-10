#lines 2-3 simply store/view the data set for indian diabetes
pima_indians_diabetes <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data", header = FALSE, sep = ",")
View(pima_indians_diabetes)

#------------------------------------------------------------------------------------
#ignore zero's for attributes 3-5 and store mean/sd for attributes 2-6  
meanAtt2 <- mean(pima_indians_diabetes[,2])
sdAtt2 <- sd(pima_indians_diabetes[,2])

meanAtt3 <- mean(pima_indians_diabetes[,3][pima_indians_diabetes[,3] != 0])
sdAtt3 <- sd(pima_indians_diabetes[,3][pima_indians_diabetes[,3] != 0])

meanAtt4 <- mean(pima_indians_diabetes[,4][pima_indians_diabetes[,4] != 0])
sdAtt4 <- sd(pima_indians_diabetes[,4][pima_indians_diabetes[,4] != 0])

meanAtt5 <- mean(pima_indians_diabetes[,5][pima_indians_diabetes[,5] != 0])
sdAtt5 <- sd(pima_indians_diabetes[,5][pima_indians_diabetes[,5] != 0])

meanAtt6 <- mean(pima_indians_diabetes[,6])
sdAtt6 <- sd(pima_indians_diabetes[,6])
#------------------------------------------------------------------------------------
#ignore zero's for attributes 3-5 and store covariance for attributes 2-6  
matAtt2 <- data.matrix(pima_indians_diabetes[,2])
covAtt2 <- cov(matAtt2)

matAtt3 <- data.matrix(pima_indians_diabetes[,3][pima_indians_diabetes[,3] != 0])
covAtt3 <- cov(matAtt3)

matAtt4 <- data.matrix(pima_indians_diabetes[,4][pima_indians_diabetes[,4] != 0])
covAtt4 <- cov(matAtt4)

matAtt5 <- data.matrix(pima_indians_diabetes[,5][pima_indians_diabetes[,5] != 0])
covAtt5 <- cov(matAtt5)

matAtt6 <- data.matrix(pima_indians_diabetes[,6])
covAtt6 <- cov(matAtt6)

#change zero's to NA for atrributes 3-5 and store correlation data
corAtt2Att3 <- cor(pima_indians_diabetes[,2], pima_indians_diabetes[,3], use="na.or.complete")
corAtt2Att4 <- cor(pima_indians_diabetes[,2], pima_indians_diabetes[,4], use="na.or.complete")
corAtt2Att5 <- cor(pima_indians_diabetes[,2], pima_indians_diabetes[,5], use="na.or.complete")
corAtt2Att6 <- cor(pima_indians_diabetes[,2], pima_indians_diabetes[,6], use="na.or.complete")
corAtt3Att4 <- cor(pima_indians_diabetes[,3], pima_indians_diabetes[,4], use="na.or.complete")
corAtt3Att5 <- cor(pima_indians_diabetes[,3], pima_indians_diabetes[,5], use="na.or.complete")
corAtt3Att6 <- cor(pima_indians_diabetes[,3], pima_indians_diabetes[,6], use="na.or.complete")
corAtt4Att5 <- cor(pima_indians_diabetes[,4], pima_indians_diabetes[,5], use="na.or.complete")
corAtt4Att6 <- cor(pima_indians_diabetes[,4], pima_indians_diabetes[,6], use="na.or.complete")
corAtt5Att6 <- cor(pima_indians_diabetes[,5], pima_indians_diabetes[,6], use="na.or.complete")
#------------------------------------------------------------------------------------
#create scatterplot for attributes 3 and 6, create scatterplot for attributes 2 and 7 
