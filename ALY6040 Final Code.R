#Code to Clear Global Environment
rm(list=ls())                            
while (!is.null(dev.list()))  dev.off()
getwd()

library(readr)
library(tidyverse)
library(dplyr)
library(Hmisc)
library(ggplot2)
library(caret)
library(plyr)
library(corrplot)
library(GGally)
library(corrr)
library(mlogit)
library(VGAM)
library(glmnet)
library(nnet)
library(gbm)
library(onehot)
library(MASS)
library(plotly)
library(lars)
library(glmnet)
library(ggcorrplot)
library(rpart)
require(rpart.plot)
library(Rmisc)
library(randomForest)
getwd()
mytrain <- read.csv("~/downloads/costa-rican-household-poverty-prediction/train.csv")
mytest <- read.csv("~/downloads/costa-rican-household-poverty-prediction/test.csv")
mytest$Target <- 0
full <- mytrain
####################################   PREPROCESSING DATA  ##################################################
#We can observe that several family members donot have the same target variable.
#We solve this by giving 
#all household members the same Target as that of head of house.
real_target <- mytrain[mytrain$parentesco1 == 1, c('idhogar', 'Target')]

for(n in nrow(real_target)){
  mytrain$Target[mytrain$idhogar == real_target[n, 'idhogar']] <- real_target[n, 'Target']
}
full <- mytrain
full$Target <- as.factor(full$Target)
full$Target = mapvalues(full$Target , from = c("1","2","3","4"),
                          to = c("extreme","moderate","decent","welloff"))

misdata<-function(x) mean(is.na(x))*100
apply(full[,c(colnames(full)[colSums(is.na(full)) > 0])],2,misdata) #Getting percentage of missing values in each variable
#variables with major missing valuea are....
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
#As we can see in the SQDependency, we can see that no : 0 and yes: 1. And the values are numeric.
full$dependency = mapvalues(full$dependency , from = c("yes","no"),
                              to = c("1","0"))
full$dependency <- as.numeric(levels(full$dependency))[full$dependency]

str(full$dependency)

#edjefa and edjefe are transformed based on their squared column. like "dependency" and "SQBdependency"
full$edjefa = mapvalues(full$edjefa , from = c("yes","no"),
                        to = c("1","0"))
full$edjefa <- as.numeric(levels(full$edjefa))[full$edjefa]

full$edjefe = mapvalues(full$edjefe , from = c("yes","no"),
                        to = c("1","0"))
full$edjefe <- as.numeric(levels(full$edjefe))[full$edjefe]


###### missing values #######
# Now, let us focus on the missing value "v2a1" "v18q1" "rez_esc" "meaneduc" "SQBmeaned"
# v2a1 is monthly rent, The value is missing means they have own house
full$v2a1[(is.na(full$v2a1)) & (full$tipovivi1 == 1)] <- 0
full$v2a1[(is.na(full$v2a1)) & (full$tipovivi2 == 1)] <- 0

#There is NA in number of tablets if individual is not holding any tablets
full$v18q1[is.na(full$v18q1) & (full$v18q == 0)] <- 0

#summary(full$rez_esc)
#rez_sec is NA means they are outside the school age (7~19).
#rez_sec above 5 is made equal to 5. 
#let those NA or 0

full$rez_esc[(is.na(full$rez_esc)) & ((full$age < 7) | (full$age > 19))] <- 0
full$rez_esc[full$rez_esc > 5] = 5
full$meaneduc[(is.na(full$meaneduc)) & (full$age < 19)] <- 0

full$v2a1[is.na(full$v2a1)] <- getmode(full$v2a1)   # insert the mode
full$rez_esc[is.na(full$rez_esc)] <- getmode(full$rez_esc)   # insert the mode
full$meaneduc[is.na(full$meaneduc)] <- getmode(full$meaneduc)   # insert the mode

#The columns are mailnly categorized into 4 ways as ID columns, Individual, Household and squared columns.
#All the eight squared columns have the squared values of the other columns. 
#This clearly indicates that these columns will be highly correlated with their original columns.
#So, we'll remove these squared columns.
SQB_column <- c('SQBescolari', 'SQBage', 'SQBhogar_total', 'SQBedjefe',
                'SQBhogar_nin', 'SQBovercrowding', 'SQBdependency',
                'SQBmeaned')

full[,SQB_column] <- NULL
dim(full)
id_column <- c('Id', 'idhogar', 'Target')

Individual_bool = c('v18q', 'dis', 'male', 'female', 'estadocivil1', 'estadocivil2', 'estadocivil3', 
                    'estadocivil4', 'estadocivil5', 'estadocivil6', 'estadocivil7', 
                    'parentesco1', 'parentesco2',  'parentesco3', 'parentesco4', 'parentesco5', 
                    'parentesco6', 'parentesco7', 'parentesco8',  'parentesco9', 'parentesco10', 
                    'parentesco11', 'parentesco12', 'instlevel1', 'instlevel2', 'instlevel3', 
                    'instlevel4', 'instlevel5', 'instlevel6', 'instlevel7', 'instlevel8', 
                    'instlevel9', 'mobilephone', 'rez_esc-missing')

Individual_ordered = c('rez_esc', 'escolari', 'age')
house_bool = c('hacdor', 'hacapo', 'v14a', 'refrig', 'paredblolad', 'paredzocalo', 
               'paredpreb','pisocemento', 'pareddes', 'paredmad',
               'paredzinc', 'paredfibras', 'paredother', 'pisomoscer', 'pisoother', 
               'pisonatur', 'pisonotiene', 'pisomadera',
               'techozinc', 'techoentrepiso', 'techocane', 'techootro', 'cielorazo', 
               'abastaguadentro', 'abastaguafuera', 'abastaguano',
               'public', 'planpri', 'noelec', 'coopele', 'sanitario1', 
               'sanitario2', 'sanitario3', 'sanitario5',   'sanitario6',
               'energcocinar1', 'energcocinar2', 'energcocinar3', 'energcocinar4', 
               'elimbasu1', 'elimbasu2', 'elimbasu3', 'elimbasu4', 
               'elimbasu5', 'elimbasu6', 'epared1', 'epared2', 'epared3',
               'etecho1', 'etecho2', 'etecho3', 'eviv1', 'eviv2', 'eviv3', 
               'tipovivi1', 'tipovivi2', 'tipovivi3', 'tipovivi4', 'tipovivi5', 
               'computer', 'television', 'lugar1', 'lugar2', 'lugar3',
               'lugar4', 'lugar5', 'lugar6', 'area1', 'area2', 'v2a1-missing')

house_ordered = c('rooms', 'r4h1', 'r4h2', 'r4h3', 'r4m1','r4m2','r4m3', 'r4t1',  'r4t2', 
                  'r4t3', 'v18q1', 'tamhog','tamviv','hhsize','hogar_nin',
                  'hogar_adul','hogar_mayor','hogar_total',  'bedrooms', 'qmobilephone')

house_cont = c('v2a1', 'dependency', 'edjefe', 'edjefa', 'meaneduc', 'overcrowding')

#   data containing only heads are seperated to check their behaviour.
#   heads <- mytrain[(train$parentesco1 == 1),]
##  heads = heads[,c(id_column+house_bool+house_cont+house_ordered)]

corr1 <- c('r4t3', 'tamhog', 'tamviv', 'hhsize', 'hogar_total')
nums <- unlist(lapply(full[,c('r4t3', 'tamhog', 'tamviv', 'hhsize', 'hogar_total')], is.numeric))
c_corr <- full[,corr1]
corr_c <- cor(c_corr)
corrplot(corr_c, method = "square", tl.cex = 0.2, tl.offset = 0.2,tl.srt = 90, cl.ratio = 0.6)
ggcorr(full[, corr1])

#From the above correlation plot we can see that the columns  'r4t3' ,'hhsize','tamhog', 'hogar_total'
#are highly correlated.
#These are toatal persons in household, size of household, Number of individuals in household
#and household size. so we need remove them
full[,c('tamhog', 'hogar_total', 'r4t3')] <- NULL

#area1, and area2 tell us if the individual live in Urban or Rural zones.
#so we just need one of them  we'll remove one.
full$area2 <- NULL

#We'll ceate new feature that will both the negitives and positives of the house. 
#We add in negative if there is no toilet, no electricity, no floor at house, if no water provision and if 
#house have no ceiling.
full$negative <- 1 * (full$sanitario1 + (full$noelec == 1) + full$pisonotiene +
                        full$abastaguano + (full$cielorazo == 0))

#We add in positive if there is refrigirator, if they own tablet in the household, if they have telision and computer.
full$positive <- 1 * (full$refrig + (full$v18q1 > 0) + full$computer +
                        full$television)

ggplot(full, aes(x = Target, y = positive)) + geom_count(aes(col = positive)) + labs(title = "Positive factors v/s Target",
                                                                                     x = "Poverty Level", y = "Positive Count")
ggplot(full, aes(x = Target, negative)) + geom_count(aes(col = negative)) + labs(title = "Negative factors v/s Target",
                                                                                 x = "Poverty Level", y = "Negative Count")
# Above two graphs: In the first graph, the size of dot means the number of family, the color means the
# the level of poverty. When Positive Count = 3, it means the family have 3 positive items of refrigirator, telision, tablet and computer.
# for example, from the last column, we know well-families who have 2 positive items are more than other level.

variables = c('dependency', 'negative', 'meaneduc',
              'r4m1', 'overcrowding')
ggcorr(full[, variables])

#Since there are two columns with male and female, we'll remove male.
full$male <- NULL
ggplot(full, aes(x = Target, y = overcrowding)) + geom_boxplot(aes(group = Target, col = Target))+labs(title = "Overcrowding & Target")
ggplot(mytrain, aes(x = Target, y = r4t3)) + geom_boxplot(aes(group = Target, col = Target))+labs(title = "Total persons in the household & Target")
ggplot(full, aes(x = Target, y = hogar_nin)) + geom_boxplot(aes(group = Target, col = Target))+labs(title = "Number of children & Target")
ggplot(full, aes(x = Target, y = qmobilephone)) + geom_boxplot(aes(group = Target, col = Target))+labs(title = "Mobile phone number")

# Now, besides add miss value, we initally process and know some correlation between some varaibles
# Id, idhogar,agesq,elimbasu5 are removed before modelling
full$Id <- NULL
full$idhogar <- NULL
full$agesq <- NULL
full$elimbasu5 <- NULL

mytrain <- full
mytest <- full[9558:33413,]
dim(mytrain)

####################### Split variable to divide data into trainig and testing ###################
set.seed(1)
train_index2 <- sample(1:nrow(mytrain), 7000)  # sampling 7000 samples from "train" dataset
rtrain <- mytrain[train_index2,]
rtest <- mytrain[-train_index2,]
dim(rtrain)

############################# K-MEAN CLUSTERING ############################# 
########## preprocessing for K-Means Clustering ##############
library(cluster) 
library(factoextra)
CLusterData <- mytrain
Sum_DS <- na.omit(CLusterData) 
Sum_DS$Target <- NULL
# standardizing the data
Sum_DS <- scale(Sum_DS)
# checking the dataset
head(Sum_DS)
# computing a distance matrix
distance <- get_dist(Sum_DS)
fviz_dist(distance, gradient = list(low = "green", mid = "white", high = "orange"))
#visualizing a distance matrix
########## K-Means Clustering ##############
# The first step is grouping the data into 2 clusters
Ds_K2 <- kmeans(Sum_DS,centers = 2, nstart = 25)
str(Ds_K2)
# print the output1
Ds_K2
# visualizing the output1
fviz_cluster(Ds_K2,data = Sum_DS)
# Executing the same process for 3,4,5
Ds_K3 <- kmeans(Sum_DS,centers = 3, nstart = 25)
Ds_K4 <- kmeans(Sum_DS,centers = 4, nstart = 25)

# plots to compare
p2 <- fviz_cluster(Ds_K2,geom = "point",data = Sum_DS)+ ggtitle("k=2")
p3 <- fviz_cluster(Ds_K3,geom = "point",data = Sum_DS)+ ggtitle("k=3")
p4 <- fviz_cluster(Ds_K4,geom = "point",data = Sum_DS)+ ggtitle("k=4")

  library(gridExtra)
  grid.arrange(p2,p3,nrow = 2)

# Determining Optimal Clusters
# There are tree types to determine the optimal cluster, which includes:
# Elbow method  /   Silhouette method  /  Gap statistic
# Elbow method
fviz_nbclust(Sum_DS,kmeans,method = "wss")+
  geom_vline(xintercept = 4,linetype = 2)+
  labs(subtitle = "Elbow method")
# Extracting Result
# Computing k-means clustering wiht k =4
set.seed(1)
Finalresult <- kmeans (Sum_DS, 4, nstart = 25)
print(Finalresult)
fviz_cluster(Finalresult, data = Sum_DS)

###################### DECISION TREE #########################
#Building decision tree
set.seed(1)
tree <- rpart(rtrain$Target~., data=rtrain)
## myAttrition = rtrain  , Attrition = Target
#Draw the tree
prp(tree, type=3, tweak=0.8, main="Target", compress=TRUE )

#Making predictions 
Treefit<-rpart(rtrain$Target~., data=rtrain)
Predict<-predict(Treefit,newdata=rtest, type = "class")
#2nd method of evaluation - MSE & RMSE
#MSE
mean((as.numeric(Predict)-as.numeric(rtest$Target))^2)
#RMSE
sqrt(mean((as.numeric(Predict)-as.numeric(rtest$Target))^2))
table_mat <- table(rtest$Target, Predict)
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test
# https://www.guru99.com/r-decision-trees.html
#https://www.geeksforgeeks.org/confusion-matrix-machine-learning/


###################### RANDOM FOREST  ###################### 
RF = randomForest(rtrain$Target ~ rtrain$meaneduc, data = rtrain)
RF
plot(RF, main = "")
impVar = round(randomForest::importance(RF),2)
impVar[order(impVar[,1], decreasing = TRUE),]
###Tuning Random Forest

tunedRF = tuneRF(rtrain[ , -1], rtrain[ ,1], stepFactor = 0.5, plot = TRUE, ntreeTry = 30, improve = TRUE)
tunedRF

predRF<-predict(RF, newdata=rtest, type="class")

#Confusion Matrix
t2 = tclable(rtrain$Target, predRF)

#RandomForest Model Accuracy
(t2[1] + t2[4])/(nrow(rtrain))


###################### MULTINOMIAL LOGISTIC REGRESSION   ###################### 

mytrain <- full
mytest <- full[9558:33413,]
dim(mytrain)

####################### Split variable to divide data into trainig and testing ###################
set.seed(1)
train_index2 <- sample(1:nrow(mytrain), 7000)  # sampling 7000 samples from "train" dataset
rtrain <- mytrain[train_index2,]
rtest <- mytrain[-train_index2,]
dim(rtrain)
###############################3

ml_fit <- multinom(Target~.,data = rtrain)
summary(ml_fit)
save(ml_fit,file =  "ml_fit.rda")

ml_pred <- predict(ml_fit, rtest)
head(ml_pred)
accuracy <- mean(ml_pred == rtest$Target)
accuracy
cm_ml <- confusionMatrix(ml_pred, rtest$Target)

###################### GRADIENT BOOSTING METHOD ###################### 
# train GBM model1
GBM1<- gbm(
  formula = Target ~ .,
  distribution = "gaussian",
  data = rtrain,
  n.trees = 100,
  interaction.depth = 1,
  shrinkage = 0.001,
  cv.folds = 5,
  n.cores = NULL,
  verbose = FALSE
)  
# Summary gives a table of Variable Importance and a plot of Variable Importance
summary(GBM1) 
#find index for n trees with minimum CV error
cv.error = sqrt(min(GBM1$cv.error)); cv.error

# get MSE and compute RMSE
idx_min_MSE <- which.min(GBM1$cv.error)
sqrt(GBM1$cv.error[idx_min_MSE])
gbm.perf(GBM1, method = "cv")

## plot loss function as a result of n trees added to the ensemble
#  black line plots the training error and the green line plots the validation error
#  blue dashed line shows the optimum iteration
#  need more accurate model, GBM2
GBM2  <- gbm(
  formula = Target ~ .,
  distribution = "gaussian",
  data = rtrain,
  n.trees = 1000,
  interaction.depth = 5,
  shrinkage = 0.001,
  cv.folds = 5,
  n.cores = NULL, 
  verbose = FALSE
)  
summary(GBM2) 
cv.error = sqrt(min(GBM2$cv.error)); cv.error
idx_min_MSE <- which.min(GBM2$cv.error)
sqrt(GBM2$cv.error[idx_min_MSE])
gbm.perf(GBM2, method = "cv")
# need more accurate model, GBM3
GBM3 <- gbm(
  formula = Target ~ .,
  distribution = "gaussian",
  data = rtrain,
  n.trees = 600,
  interaction.depth = 5,
  shrinkage = 0.05,
  cv.folds = 5,
  n.cores = NULL,
  verbose = FALSE
)  
summary(GBM3) 
cv.error = sqrt(min(GBM3$cv.error)); cv.error
idx_min_MSE <- which.min(GBM3$cv.error)
sqrt(GBM3$cv.error[idx_min_MSE])
gbm.perf(GBM3, method = "cv")

# create hyperparameter grid
# create hyperparameter grid
hyper_grid <- expand.grid(
  shrinkage = c(.01, .1, .3),
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(5, 10, 15),
  bag.fraction = c(.65, .8, 1), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

# total number of combinations
nrow(hyper_grid)
#81 

# randomize data
random_index <- sample(1:nrow(rtrain), nrow(rtrain))
random_rtrain <- rtrain[random_index, ] ## random_white_train <- random_rtrain
# grid search 

hyper_grid %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)

#result
#shrinkage interaction.depth n.minobsinnode bag.fraction optimal_trees  min_RMSE
#1       0.01                 5              5         0.80          4926 0.6778586

#put the result into final model
# for reproducibility
set.seed(123)

gbm.final <- gbm(            # gbm.white.final <- gbm.final
  formula = Target ~ .,
  distribution = "gaussian",
  data = rtrain,
  n.trees = 4926,
  interaction.depth = 5,
  shrinkage = 0.01,
  cv.folds = 5,
  n.minobsinnode = 5,
  bag.fraction = .8, 
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

summary(gbm.final) 
cv.error = sqrt(min(gbm.final$cv.error)); cv.error
idx_min_MSE <- which.min(gbm.final$cv.error)
sqrt(gbm.final$cv.error[idx_min_MSE])
gbm.perf(gbm.final, method = "cv")

## Visualizing - Variable Importance
par(mar = c(5, 8, 1, 1))
summary(
  gbm.final, 
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
)
## Predicting(comapre result from model 3 and final model)
# predict values for test data
pred.gbm3 <- predict(GBM3, n.trees = GBM3$n.trees, white_test)
pred.gbm.final <- predict(gbm.final, n.trees = gbm.final$n.trees, white_test)

# results
caret::RMSE(pred.gbm3, white_test$Target)
caret::RMSE(pred.gbm.final, white_test$Target)

