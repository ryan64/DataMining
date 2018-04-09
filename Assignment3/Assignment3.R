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
folds = split(spamnorm, sample(1:10, nrow(spamnorm), replace=T))

################# running svm on dataset ##################
install.packages("e1071")
library(e1071)

svmfit = list()
for (i in 1:10){
  trainData <- folds[-i] 
  trainData = do.call('rbind', trainData)
  svmfit[i] <- svm(V58 ~., data = trainData)
}

################# running neural networks on dataset ####################

