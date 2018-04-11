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

###########################################################

install.packages("neuralnet")
install.packages("e1071")
install.packages("randomForest")
install.packages("tree")

library(neuralnet)
library(e1071)
library(randomForest)
library(tree)

file <- ("/home/ryan/Desktop/DataMining/Assignment3/spambase/spambase.data") #Romita, please change location of dataset 'spambase.data'
spambase <- read.table(file, header = FALSE, sep = ",")

################ 10-cross fold validation #################

spamnorm = scale(spambase [,1:57])
spamnorm = data.frame(spamnorm)
spamnorm$V58 = spambase[,58]

folds = split(spamnorm, sample(1:10, nrow(spamnorm), replace=T))

################# running svm on dataset ##################

errorsForSVM = list()
for (i in 1:10){
  trainData <- folds[-i] #trained data is every fold except for the current ith iteration.
  trainData = do.call('rbind', trainData)
  svmfit <- svm(V58 ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + V28 + V29 + V30 + V31 + V32 + V33 + V34 + V35 + V36 + V37 + V38 + V39 + V40 + V41 + V42 + V43 + V44 + V45 + V46 + V47 + V48 + V49 + V50 + V51 + V52 + V53 + V54 + V55 + V56 + V57, data = trainData)
  testData <- folds[[i]][,1:57] #test data is the current ith iteration.
  predictedOutcomes <- predict(svmfit, testData)
  errorsForSVM[i] <- (sum((predictedOutcomes - folds[[i]][,58])^2)/nrow(folds[[i]]))^.5
}

averageError = do.call('sum', errorsForSVM)/10

################# running neural networks on dataset ####################

errorsForNN = list()
for(i in 1:10){
  traindata = folds[-i]
  traindata = do.call('rbind', traindata)
  NN = neuralnet(V58 ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + V28 + V29 + V30 + V31 + V32 + V33 + V34 + V35 + V36 + V37 + V38 + V39 + V40 + V41 + V42 + V43 + V44 + V45 + V46 + V47 + V48 + V49 + V50 + V51 + V52 + V53 + V54 + V55 + V56 + V57, data = traindata, linear.output = TRUE, hidden = 2)
  predictedData = compute(NN, folds[[i]][,c(1:57)])
  errorsForNN[i] = (sum((predictedData$net.result - folds[[i]][,58])^2) / nrow(folds[[i]])) ^ 0.5
}

NNAvgError = do.call('sum',errorsForNN)/10

################# running random forests on dataset ####################

err1 = list()
for (i in 1:10){
  trainData <- folds[-i] #trained data is every fold except for the current ith iteration.
  trainData = do.call('rbind', trainData)
  forestFit <- randomForest(V58 ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + V28 + V29 + V30 + V31 + V32 + V33 + V34 + V35 + V36 + V37 + V38 + V39 + V40 + V41 + V42 + V43 + V44 + V45 + V46 + V47 + V48 + V49 + V50 + V51 + V52 + V53 + V54 + V55 + V56 + V57, data = trainData, proximity=TRUE)
  testData <- folds[[i]][,1:57] #test data is the current ith iteration.
  predOutcome <- predict(forestFit, testData)
  err1[i] <- (sum((predOutcome - folds[[i]][,58])^2)/nrow(folds[[i]]))^.5
}

avgRandomForest = do.call('sum', err1)/10

################# running decision trees on dataset ####################
errorTree = list()

for(i in 1:10){
  train = folds[-i]
  train = do.call('rbind', train)
  output.tree = tree(V58 ~ ., train)
  prediction = predict(output.tree, folds[[i]])
  errorTree[i] = mean(prediction != folds[[i]]$V58)
}

# This is how accurate decision tree is NOT error
avgTree = do.call('sum',errorTree)/10