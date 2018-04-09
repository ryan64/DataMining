############################
#                          #
# Ryan H. Gonzalez         #
#                          #
# Using spambase dataset,  #
# running neural networks, # 
# support vector machines, # 
# and decision tress.      #
#                          #
############################

file <- ("/home/ryan/Desktop/DataMining/Assignment3/spambase/spambase.data") #Romita, please change location of dataset 'spambase.data'
spambase <- read.table(file, header = FALSE, sep = ",")

################ 10-cross fold validation #################

spamnorm = scale(spambase [,1:57])
spamnorm = data.frame(spamnorm)
spamnorm$V58 = spambase[,58]

folds = split(spamnorm, sample(1:10, nrow(spamnorm), replace=T))

################# running svm on dataset ##################
install.packages("e1071")
library(e1071)

svmfit = list()
for (i in 1:10){
  trainData <- folds[-i] 
  trainData = do.call('rbind', trainData)
  svmfit[i] <- svm(V58 ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + V28 + V29 + V30 + V31 + V32 + V33 + V34 + V35 + V36 + V37 + V38 + V39 + V40 + V41 + V42 + V43 + V44 + V45 + V46 + V47 + V48 + V49 + V50 + V51 + V52 + V53 + V54 + V55 + V56 + V57, data = trainData)
}

testtraindata <- folds[-1]
testtraindata <- do.call('rbind', testtraindata)
testsvmfit <- svm(V58 ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + V28 + V29 + V30 + V31 + V32 + V33 + V34 + V35 + V36 + V37 + V38 + V39 + V40 + V41 + V42 + V43 + V44 + V45 + V46 + V47 + V48 + V49 + V50 + V51 + V52 + V53 + V54 + V55 + V56 + V57, data = testtraindata)
print(testsvmfit)

################# running neural networks on dataset ####################

