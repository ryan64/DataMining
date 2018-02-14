#lines 2-3 simply store/view the data set for indian diabetes
pima_indians_diabetes <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data", header = FALSE, sep = ",")
View(pima_indians_diabetes)

#change the file location when you run script
pima_indians_diabetes_clean <- read.csv(file = "/home/ryan/Downloads/Assignment 1.csv", header = FALSE)
View(pima_indians_diabetes_clean)

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
pima_indians_diabetes[,3:5][pima_indians_diabetes[,3:5] == 0] <- NA
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
plot(pima_indians_diabetes[,3], pima_indians_diabetes[,6], main="Diastolic Blood Pressure VS. Body Mass Index", xlab="Blood Pressure", ylab="BMI")
plot(pima_indians_diabetes[,2], pima_indians_diabetes[,7], main="Plasma Glucose Concentration VS. Diabetes Pedigree", xlab="Plasma Glucose Concentration", ylab="Diabetes Pedigree")
#------------------------------------------------------------------------------------
#create histogram for attributes 2, 3, and 6
hist(pima_indians_diabetes[,2])
hist(pima_indians_diabetes[,3])
hist(pima_indians_diabetes[,6])
#create histogram for attributes 2, 3, and 6 with instances of class 1 and 0
hist(pima_indians_diabetes[,2][pima_indians_diabetes[,9] == 0])
hist(pima_indians_diabetes[,2][pima_indians_diabetes[,9] == 1])

hist(pima_indians_diabetes[,3][pima_indians_diabetes[,9] == 0])
hist(pima_indians_diabetes[,3][pima_indians_diabetes[,9] == 1])

hist(pima_indians_diabetes[,6][pima_indians_diabetes[,9] == 0])
hist(pima_indians_diabetes[,6][pima_indians_diabetes[,9] == 1])
#------------------------------------------------------------------------------------
#create boxplot for attrubutes 2, 7, 8
boxplot(pima_indians_diabetes[,2])
boxplot(pima_indians_diabetes[,2][pima_indians_diabetes[,9] == 0])
boxplot(pima_indians_diabetes[,2][pima_indians_diabetes[,9] == 1])

boxplot(pima_indians_diabetes[,7])
boxplot(pima_indians_diabetes[,7][pima_indians_diabetes[,9] == 0])
boxplot(pima_indians_diabetes[,7][pima_indians_diabetes[,9] == 1])

boxplot(pima_indians_diabetes[,8])
boxplot(pima_indians_diabetes[,8][pima_indians_diabetes[,9] == 0])
boxplot(pima_indians_diabetes[,8][pima_indians_diabetes[,9] == 1])
#------------------------------------------------------------------------------------
#create supervised scatterplots for all pairs of attributes 2-6
plot(pima_indians_diabetes[,2], pima_indians_diabetes[,3], col = ifelse(pima_indians_diabetes$V9 == 0, "red", "blue"))
plot(pima_indians_diabetes[,2], pima_indians_diabetes[,4], col = ifelse(pima_indians_diabetes$V9 == 0, "red", "blue"))
plot(pima_indians_diabetes[,2], pima_indians_diabetes[,5], col = ifelse(pima_indians_diabetes$V9 == 0, "red", "blue"))
plot(pima_indians_diabetes[,2], pima_indians_diabetes[,6], col = ifelse(pima_indians_diabetes$V9 == 0, "red", "blue"))
plot(pima_indians_diabetes[,3], pima_indians_diabetes[,4], col = ifelse(pima_indians_diabetes$V9 == 0, "red", "blue"))
plot(pima_indians_diabetes[,3], pima_indians_diabetes[,5], col = ifelse(pima_indians_diabetes$V9 == 0, "red", "blue"))
plot(pima_indians_diabetes[,3], pima_indians_diabetes[,6], col = ifelse(pima_indians_diabetes$V9 == 0, "red", "blue"))
plot(pima_indians_diabetes[,4], pima_indians_diabetes[,5], col = ifelse(pima_indians_diabetes$V9 == 0, "red", "blue"))
plot(pima_indians_diabetes[,4], pima_indians_diabetes[,6], col = ifelse(pima_indians_diabetes$V9 == 0, "red", "blue"))
plot(pima_indians_diabetes[,5], pima_indians_diabetes[,6], col = ifelse(pima_indians_diabetes$V9 == 0, "red", "blue"))

#create two 3D scatterplots, one for attributes 2, 3, 6 and one for attributes 2, 4, 6
library(scatterplot3d) #import 3d library
attach(pima_indians_diabetes) #for 3d scatterplots
scatterplot3d(x = pima_indians_diabetes[,2], y = pima_indians_diabetes[,3], z = pima_indians_diabetes[,6], xlab = "Plasma Glucose Concentration", ylab = "Diastolic Blood Pressure", zlab = "BMI", main = "Attributes 2, 3 and 6")
scatterplot3d(x = pima_indians_diabetes[,2], y = pima_indians_diabetes[,4], z = pima_indians_diabetes[,6], xlab = "Plasma Glucose Concentration", ylab = "Tricep Skin Fold Thickness", zlab = "BMI", main = "Attributes 2, 4 and 6")
#------------------------------------------------------------------------------------
#Create Star plot for first 10 instances of class 0 and first 10 instances of class 1 for attributes 1, 2, 3, 8.
stars(head(subset(pima_indians_diabetes_clean[,c("V1","V2","V3","V8")], pima_indians_diabetes_clean$V9 == 0), 10), key.loc = c(7,2))
stars(head(subset(pima_indians_diabetes_clean[,c("V1","V2","V3","V8")], pima_indians_diabetes_clean$V9 == 1), 10), key.loc = c(7,2))
#------------------------------------------------------------------------------------
#Number 8
s <- scale(pima_indians_diabetes_clean)
zscores <- data.frame(a = s[,1], b = s[,2], c = s[,3], d = s[,4], e = s[,5], f = s[,6], g = s[,7], h = s[,8], z = pima_indians_diabetes_clean[,9])
newmodel <- lm(z ~ a + b + c + d + e + f + g + h, data = zscores)
newmodel$coefficients
summary(newmodel)$r.squared
newmodel2 <- lm(z ~ a + b + c + f + g + h, data = zscores)
newmodel2$coefficients
summary(newmodel2)$r.squared
