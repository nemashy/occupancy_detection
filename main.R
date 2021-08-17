if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(gbm)) install.packages("gbm", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(formattable)) install.packages("formattable", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(rattle)) install.packages("rattle", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(pROC)) install.packages("pROC", repos = "http://cran.us.r-project.org")
if(!require(rAverage)) install.packages("rAverage", repos = "http://cran.us.r-project.org")
if(!require(caTools)) install.packages("caTools", repos = "http://cran.us.r-project.org")
if(!require(glmnet)) install.packages("glmnet", repos = "http://cran.us.r-project.org")
library(knitr)
library(cowplot)
if(!require(cowplot)) install.packages("cowplot", repos = "http://cran.us.r-project.org")
if(!require(ROCR)) install.packages("ROCR", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(tree)) install.packages("tree", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")

# Exploratory Data Analysis

train <- read_csv("occupancy_training.csv")
test <- read_csv("occupancy_testing.csv")

# Inspecting the training set 

names(train)
head(train,5)
tail(train,5)
str(train)

sum(is.na(train))

unique(train$Occupancy)

# Making occupancy a factor
train$Occupancy = as.factor(train$Occupancy)
test$Occupancy = as.factor(test$Occupancy)

str(train)
str(test)

summary(train)

## Boxplots of the variables
par(mfrow=c(2,3))
boxplot(train$Temperature, ylab = "Temperature")
boxplot(train$Humidity, ylab = "Hunidity")
boxplot(train$Light, ylab = "Light")
boxplot(train$CO2, ylab = "CO2 Concentration")
boxplot(train$HumidityRatio, ylab = "Humidity Ratio")

# Outlier detection
outliers_light <- boxplot(train$Light, plot = F)$out
out_lt <- train[which(train$Light %in% outliers_light),]

outliers_co2 <- boxplot(train$CO2, plot = F)$out
out_co2 <- train[which(train$CO2 %in% outliers_co2),]

min_c <- min(out_co2$date)
max <- max(out_co2$date)

outliers_hr <- boxplot(train$HumidityRatio, plot = F)$out
out_hr <- train[which(train$HumidityRatio %in% outliers_hr),]

outliers_hr <- boxplot(train$HumidityRatio, plot = F)$out
out_hr <- train[which(train$HumidityRatio %in% outliers_hr),]

## removing light outliers
df1 <- train[-which(train$Light %in% outliers_light),]

## removing humidity ratio outliers that have zero occupancy
outliers_hr <- boxplot(df1$HumidityRatio, plot = F)$out
out_hr <- train[which(train$HumidityRatio %in% outliers_hr),]

out_hr_0 <- filter(out_hr, Occupancy == "0")
outliers_hr_0 <- out_hr_0$HumidityRatio


df2 <- df1

## All variables before outlier removal
p5 <- ggplot(train, aes(date, Temperature)) + geom_line(colour = "blue")
p6 <- ggplot(train, aes(date, Humidity)) + geom_line(colour = "red")
p7 <- ggplot(train, aes(date, Light)) + geom_line(colour = "green")
p8 <- ggplot(train, aes(date, CO2)) + geom_line(colour = "orange")
p9 <- ggplot(train, aes(date, Occupancy)) + geom_point(size = 0.1, colour = "brown") 

plot_grid(p5,p6,p7,p8,p9,  align = "v", nrow = 5)


## Outlier Detection

par(mfrow=c(2,3))
boxplot(train$Temperature, ylab = "Temperature")
boxplot(train$Humidity, ylab = "Hunidity")
boxplot(train$Light, ylab = "Light")
boxplot(train$CO2, ylab = "CO2")
boxplot(train$HumidityRatio, ylab = "Humidity Ratio")

p1 <- ggplot(out_co2, aes(date, CO2)) + geom_line()
p2 <- ggplot(out_co2, aes(date, Occupancy)) + geom_line()

plot_grid(p1,p2, nrow = 2,  align = "v") ## Check C02 ouliers

## Feature engineering


## Creating various train and test sets

train_a <- df2
test_a <- test

# Create dataset with Weekday and num_sec
train_b <- df2
test_b <- test

train_b$Weekday <- ifelse((wday(df2$date) == 1 | wday(df2$date) == 7), "0", "1")
train_b$Weekday <- as.factor(train_b$Weekday)
test_b$Weekday <- ifelse((wday(test$date) == 1 | wday(test$date) == 7), "0", "1")
test_b$Weekday <- as.factor(test_b$Weekday)

# find num_sec from the start of the day
## This part of the code was taken from https://github.com/LuisM78/Occupancy-detection-data/blob/master/Model%20Development.R

second_day <- function(x) {
  # x is an object in posixct format
  s <- hour(x)*3600+minute(x)*60+second(x)
}
train_b$num_sec <- second_day(train_b$date)
test_b$num_sec <- second_day(test_b$date)

## Re-ordering
train_b <- dplyr::select(train_b, c(1:6,8,9,7))
test_b <- dplyr::select(test_b, c(1:6,8,9,7))


# Logistic Regression


## Without regularization

## Removing date as a variable
mod1a <- glm(Occupancy~.-date,data = train_a, family = binomial)
s1 <- summary(mod1a)


## model with weekday and num_sec
mod1b <- glm(Occupancy~.-date,data = train_b, family = binomial)
s2 <- summary(mod1b)


## Error on training set
pi_hat_1a <- predict(mod1a, type = 'response')
Y_hat_1a <- ifelse(pi_hat_1a >= 0.5, "1", "0")
N <- length(Y_hat_1a)
Error_1a <- sum((Y_hat_1a != train_a$Occupancy))/N

## Error on test set
pi_hat_2a <- predict(mod1a, newdata = test_a, type = 'response')
Y_hat_2a <- ifelse(pi_hat_2a >= 0.5, "1", "0")
N <- length(Y_hat_2a)
Error_2a <- sum((Y_hat_2a != test_a$Occupancy))/N


pred <- prediction(pi_hat_2a, test_a$Occupancy)
perf <- performance(pred, 'tpr', 'fpr') 

# calculate probabilities for TPR/FPR for predictions 
auc_value <- performance(pred, measure = 'auc')@y.values[[1]]


## Make prediction on the with week and num_sec

pi_hat_1b <- predict(mod1b, type = 'response')
Y_hat_1b <- ifelse(pi_hat_1b >= 0.5, "1", "0")
N <- length(Y_hat_1b)
Error_1b <- sum((Y_hat_1b != train_b$Occupancy))/N

pi_hat_2b <- predict(mod1b, newdata = test_b, type = 'response')
Y_hat_2b <- ifelse(pi_hat_2b >= 0.5, "1", "0")
N <- length(Y_hat_2b)
Error_2b <- sum((Y_hat_2b != test_b$Occupancy))/N


pred <- prediction(pi_hat_2b, test_b$Occupancy)
perf <- performance(pred, 'tpr', 'fpr') # calculate probabilities for TPR/FPR for predictions 
auc_value <- performance(pred, measure = 'auc')@y.values[[1]]


## Logistic Regression Without Regularization

## Logistic Regression with Regularization

### Lasso

## Lasso set_a

X <- data.matrix(train_a[,c(-1,-7)])
Y <- data.matrix(train_a[7])

set.seed(1)
mod_a <- glmnet(X, Y, alpha = 1, nfolds = 10, type.measure = 'class', standardize = T, family = 'binomial')
set.seed(1)
cv <- cv.glmnet(X, Y, alpha = 1, nfolds = 10, type.measure = 'class', standardize = T, family = 'binomial')

plot(cv)


c1 <- coef(mod_a, s=cv$lambda.1se)
c2 <- coef(mod_a, s=cv$lambda.min)

# Using min model

new_x <- data.matrix(test_a[,c(-1,-7)])
new_y <- data.matrix(test_a[7])
pi_hat_3 <- predict(mod_a, newx =  new_x, type = 'response', s = cv$lambda.min)
pred3 <- prediction(pi_hat_3, test_a$Occupancy)
perf3 <- performance(pred3, 'tpr', 'fpr') # calculate probabilities for TPR/FPR for predictions 

auc_value <- performance(pred3, measure = 'auc')@y.values[[1]]

Y_hat_3 <- ifelse(pi_hat_3 >= 0.5, "1", "0")
cm <- confusionMatrix(as.factor(Y_hat_3), test_a$Occupancy)


pred <- prediction(pi_hat_3, test_a$Occupancy)
perf <- performance(pred, 'tpr', 'fpr') # calculate probabilities for TPR/FPR for predictions 

auc_value <- performance(pred, measure = 'auc')@y.values[[1]]


## Using 1se model

pi_hat_4 <- predict(mod_a, newx =  new_x, type = 'response', s = cv$lambda.1se)
pred4 <- prediction(pi_hat_4, test_a$Occupancy)
perf4 <- performance(pred4, 'tpr', 'fpr') # calculate probabilities for TPR/FPR for predictions 

auc_value <- performance(pred4, measure = 'auc')@y.values[[1]]

Y_hat_4 <- ifelse(pi_hat_4 >= 0.5, "1", "0")
cm <- confusionMatrix(as.factor(Y_hat_4), test_a$Occupancy)


pred <- prediction(pi_hat_4, test_a$Occupancy)
perf <- performance(pred, 'tpr', 'fpr') # calculate probabilities for TPR/FPR for predictions 

auc_value <- performance(pred, measure = 'auc')@y.values[[1]]


## might have to use the training set to choose the model not test set.

## Lasso Set_b

X <- data.matrix(train_b[,c(-1,-9)])
Y <- data.matrix(train_b[9])

set.seed(1)
mod2 <- glmnet(X, Y, alpha = 1, nfolds = 10, type.measure = 'class', standardize = T, family = 'binomial')
cv <- cv.glmnet(X, Y, alpha = 1, nfolds = 10, type.measure = 'class', standardize = T, family = 'binomial')


c3 <- coef(mod2, s=cv$lambda.1se)
c4 <- coef(mod2, s=cv$lambda.min)

# Using min model

new_x <- data.matrix(test_b[,c(-1,-9)])
new_y <- data.matrix(test_b[9])
pi_hat_3 <- predict(mod2, newx =  new_x, type = 'response', s = cv$lambda.min)
pred3 <- prediction(pi_hat_3, test$Occupancy)
perf3 <- performance(pred3, 'tpr', 'fpr') # calculate probabilities for TPR/FPR for predictions 

performance(pred3, measure = 'auc')@y.values[[1]]

Y_hat_3 <- ifelse(pi_hat_3 >= 0.5, "1", "0")
cm <- confusionMatrix(as.factor(Y_hat_3), test_b$Occupancy)


pred <- prediction(pi_hat_3, test_b$Occupancy)
perf <- performance(pred, 'tpr', 'fpr') # calculate probabilities for TPR/FPR for predictions 

auc_value <- performance(pred, measure = 'auc')@y.values[[1]]


## Using 1se model

pi_hat_4 <- predict(mod2, newx =  new_x, type = 'response', s = cv$lambda.1se)
pred4 <- prediction(pi_hat_4, test_b$Occupancy)
perf4 <- performance(pred4, 'tpr', 'fpr') 
# calculate probabilities for TPR/FPR for predictions 
auc_value <- performance(pred4, measure = 'auc')@y.values[[1]]

## Ridge model set_a

X <- data.matrix(train_a[,c(-1,-7)])
Y <- data.matrix(train_a[7])

mod_a <- glmnet(X, Y, alpha = 0, nfolds = 10, type.measure = 'class', standardize = T, family = 'binomial')
set.seed(1)
cv <- cv.glmnet(X, Y, alpha = 0, nfolds = 10, type.measure = 'class', standardize = T, family = 'binomial')

## Ridge Regression

c5 <- coef(mod_a, s=cv$lambda.1se)
c6 <- coef(mod_a, s=cv$lambda.min)

## Using min model
new_x <- data.matrix(test_a[,c(-1,-7)])
new_y <- data.matrix(test_a[7])
pi_hat_5 <- predict(mod_a, newx =  new_x, type = 'response', s = cv$lambda.min)
pred5 <- prediction(pi_hat_5, test_a$Occupancy)
perf5 <- performance(pred5, 'tpr', 'fpr') # calculate probabilities for TPR/FPR for predictions 

auc_value <- performance(pred5, measure = 'auc')@y.values[[1]]

Y_hat_5 <- ifelse(pi_hat_5 >= 0.5, "1", "0")
cm <- confusionMatrix(as.factor(Y_hat_5), test_a$Occupancy)

## Using 1se model

new_x <- data.matrix(test_a[,c(-1,-7)])
new_y <- data.matrix(test_a[7])
pi_hat_6 <- predict(mod_a, newx =  new_x, type = 'response', s = cv$lambda.1se)
pred6 <- prediction(pi_hat_6, test$Occupancy)
perf6 <- performance(pred6, 'tpr', 'fpr') # calculate probabilities for TPR/FPR for predictions 

auc_value <- performance(pred6, measure = 'auc')@y.values[[1]]

Y_hat_6 <- ifelse(pi_hat_6 >= 0.5, "1", "0")
cm <- confusionMatrix(as.factor(Y_hat_6), test_b$Occupancy)

## might have to use the training set to choose the model not test set.
## Ridge model set_b

X <- data.matrix(train_b[,c(-1,-9)])
Y <- data.matrix(train_b[9])

mod_b <- glmnet(X, Y, alpha = 0, nfolds = 10, type.measure = 'class', standardize = T, family = 'binomial')
set.seed(1)
cv <- cv.glmnet(X, Y, alpha = 0, nfolds = 10, type.measure = 'class', standardize = T, family = 'binomial')

c7 <- coef(mod_b, s=cv$lambda.1se)
c8 <- coef(mod_b, s=cv$lambda.min)

## Using min model
new_x <- data.matrix(test_b[,c(-1,-9)])
new_y <- data.matrix(test_b[9])
pi_hat_5 <- predict(mod_b, newx =  new_x, type = 'response', s = cv$lambda.min)
pred5 <- prediction(pi_hat_5, test_b$Occupancy)
perf5 <- performance(pred5, 'tpr', 'fpr') # calculate probabilities for TPR/FPR for predictions 

auc_value <- performance(pred5, measure = 'auc')@y.values[[1]]

Y_hat_5 <- ifelse(pi_hat_5 >= 0.5, "1", "0")
cm <- confusionMatrix(as.factor(Y_hat_5), test_b$Occupancy)

## Using 1se model

new_x <- data.matrix(test_b[,c(-1,-9)])
new_y <- data.matrix(test_b[9])
pi_hat_6 <- predict(mod_b, newx =  new_x, type = 'response', s = cv$lambda.1se)
pred6 <- prediction(pi_hat_6, test$Occupancy)
perf6 <- performance(pred6, 'tpr', 'fpr') # calculate probabilities for TPR/FPR for predictions 

auc_value <- performance(pred6, measure = 'auc')@y.values[[1]]

Y_hat_6 <- ifelse(pi_hat_6 >= 0.5, "1", "0")
cm <- confusionMatrix(as.factor(Y_hat_6), test_b$Occupancy)

## might have to use the training set to choose the model not test set.

## Conclusion

Regular <- read_excel("Regular.xlsx")

## Classification Tree without Weekday and num_sec
set.seed(1)
tree_a <- tree(Occupancy ~ .-date, data = train_a) 
s1 <- summary(tree_a) 

cv_tree_a <- cv.tree(tree_a , FUN = prune.misclass) #use classification error rate for pruning
best_t_a <- cv_tree_a$size[which.min(cv_tree_a$dev)] #The minimum CV Error

# Classification Trees

plot(cv_tree_a$size, cv_tree_a$dev, type='o', pch = 16, col = 'navy', lwd = 2,
     xlab='Number of terminal nodes', ylab='CV error')


# (3) Prune the tree
set.seed(1)
pr_tree_a <- prune.misclass(tree_a, best = best_t_a)
plot(pr_tree_a, type = 'uniform')
text(pr_tree_a, pretty = 0)

pred_a <- predict(pr_tree_a, test_a, type = 'class') #type argument nb for classification!


PredWithProbabilities <- predict(pr_tree_a, test_a, type = 'vector')
auc <- auc(test_a$Occupancy, PredWithProbabilities[,2])


# Bagging/Random Forests Set a

## Using bagging/random forests without num_sec and Weekday

data_comb <- rbind(train_a[,-1], test_a[,-1])


## Train/test split
set.seed(1)

## Fit a bagged trees model
bag1 <- randomForest(Occupancy ~ ., data = data_comb[1:6681,],
                     mtry = ncol(data_comb) - 1, #for bagging, use all predictors
                     ntree = 5000, #number of trees
                     importance = TRUE, #keep track of reduction in loss function
                     do.trace = 100)  #print out regular progress
# and more (see ?randomForest)

bag_varimp <- randomForest::importance(bag1, type=2)
bag_varimp <- bag_varimp[order(bag_varimp, decreasing=FALSE),]
barplot(bag_varimp, horiz = T, col = 'navy', las=1,
        xlab = 'Mean decrease in Gini index', cex.lab = 1.5, cex.axis = 1.2,
        main = 'Occupancy', cex.main = 1.8, cex.names = 1.2)

## Random Forest

## Fit a Random Forest
set.seed(1)
rf <- randomForest(Occupancy ~ ., data = data_comb[1:6681,],
                   ntree = 5000,
                   mtry = floor(sqrt(ncol(data_comb[,-6]))),
                   importance = TRUE,
                   do.trace = 100)
# For classification tree, default mtry = floor(sqrt(ncol(x)))
rf

rf_varimp <- randomForest::importance(rf, type=2)
rf_varimp <- rf_varimp[order(rf_varimp, decreasing=FALSE),]
barplot(rf_varimp, horiz = T, col = 'navy', las=1,
        xlab = 'Mean decrease in Gini index', cex.lab = 1.5, cex.axis = 1.2,
        main = 'Occupancy', cex.main = 1.8, cex.names = 1.2)

# compare OOB errors:
plot(rf$err.rate[, 'OOB'], type = 's', xlab = 'Number of trees', ylab = 'OOB error')
lines(bag1$err.rate[, 'OOB'], col = 'red', type = 's')
legend('topright', legend = c('Bagging', 'Random Forest'), col = c('red', 'black'), lwd = 2)

## Time to use both models for prediction

bag_pred <- predict(bag1, newdata = data_comb[6682:9346,], type = "response") #see
rf_pred <- predict(rf, newdata = data_comb[6682:9346,])

ytest <- data_comb[6682:9346,6]


PredWithProbabilities <- predict(bag1, data_comb[6682:9346,], type = 'prob')
auc <- auc(ytest$Occupancy, PredWithProbabilities[,2])



PredWithProbabilities <- predict(rf, newdata = data_comb[6682:9346,], type = 'prob')
auc <- auc(ytest$Occupancy, PredWithProbabilities[,2])


# Gradient Boosted Trees

# Boosted Forests without num_sec and Weekday

# changing the occupancy to integer

df4 <- train_a
df4$Occupancy <- as.integer(df4$Occupancy) - 1

# Using caret

d <- train_a
c <- test_a

d$Occupancy <- ifelse(d$Occupancy==1,'yes','no')
d$Occupancy <- as.factor(d$Occupancy)

c$Occupancy <- ifelse(c$Occupancy==1,'yes','no')
c$Occupancy <- as.factor(c$Occupancy)

objControl <- trainControl(method='cv', number=5, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)

gbmGrid <-  expand.grid(interaction.depth = c(1, 2, 3,4),
                        n.trees = (1:25)*20,
                        shrinkage = c(0.5, 0.1, 0.05),
                        n.minobsinnode = 10)


set.seed(1)
objModel <- train(d[,c(-1,-7)], d$Occupancy,
                  method='gbm',
                  trControl=objControl,
                  tuneGrid = gbmGrid,
                  metric = "ROC",
                  preProc = c("center", "scale"),
                  bag.fraction = 1)

plot(varImp(objModel))

#Evaluate the model
predictions <- predict(object=objModel, c[,c(-1,-7)], type='prob')
auc <- roc(ifelse(c[,7] =="yes",1,0), predictions[[2]])


## Classification Trees set b

## Classification Tree with Weekday and num_sec

set.seed(1)
tree_b <- tree(Occupancy ~ .-date, data = train_b)
summary(tree_b)

set.seed(1)
cv_tree_b <- cv.tree(tree_b , FUN = prune.misclass) #use classification error rate for pruning
best_t_b <- cv_tree_b$size[which.min(cv_tree_b$dev)] #The minimum CV Error

plot(cv_tree_b$size, cv_tree_b$dev, type='o', pch = 16, col = 'navy', lwd = 2,
     xlab='Number of terminal nodes', ylab='CV error')

# (3) Prune the tree
set.seed(1)
pr_tree_b <- prune.misclass(tree_b, best = best_t_b)

plot(pr_tree_b, type = 'uniform')
text(pr_tree_b, pretty = 0)


pred_b <- predict(pr_tree_b, test_b, type = 'class') #type argument nb for classification!

confusionMatrix(pred_b, test_b$Occupancy)

PredWithProbabilities <- predict(pr_tree_b, test_b, type = 'vector')
auc <- auc(test_b$Occupancy, PredWithProbabilities[,2])

## Using bagging/random forests without num_sec and Weekday
data_comb <- rbind(train_b[,-1], test_b[,-1])


## Train/test split
set.seed(1)

## Fit a bagged trees model
bag1 <- randomForest(Occupancy ~ ., data = data_comb[1:6681,],
                     mtry = ncol(data_comb) - 1, #for bagging, use all predictors
                     ntree = 5000, #number of trees
                     importance = TRUE, #keep track of reduction in loss function
                     do.trace = 100)  #print out regular progress
# and more (see ?randomForest)
bag1

## Choose number of trees:
head(bag1$err.rate)
plot(bag1$err.rate[, 'OOB'], type = 's', xlab = 'Number of trees', ylab = 'OOB error')


## Variable importance plot
varImpPlot(bag1, type = 2) #type=2: Reduction in gini index
# see ?importance

bag_varimp <- randomForest::importance(bag1, type=2)
bag_varimp <- bag_varimp[order(bag_varimp, decreasing=FALSE),]
barplot(bag_varimp, horiz = T, col = 'navy', las=1,
        xlab = 'Mean decrease in Gini index', cex.lab = 1.5, cex.axis = 1.2,
        main = 'Occupancy', cex.main = 1.8, cex.names = 1.2)

## Fit a Random Forest
set.seed(1)
rf <- randomForest(Occupancy ~ ., data = data_comb[1:6681,],
                   ntree = 5000,
                   mtry = floor(sqrt(ncol(data_comb[,-8]))),
                   importance = TRUE,
                   do.trace = 100)

rf

rf_varimp <- randomForest::importance(rf, type=2)
rf_varimp <- rf_varimp[order(rf_varimp, decreasing=FALSE),]
barplot(rf_varimp, horiz = T, col = 'navy', las=1,
        xlab = 'Mean decrease in Gini index', cex.lab = 1.5, cex.axis = 1.2,
        main = 'Occupancy', cex.main = 1.8, cex.names = 1.2)

# compare OOB errors:
plot(rf$err.rate[, 'OOB'], type = 's', xlab = 'Number of trees', ylab = 'OOB error')
lines(bag1$err.rate[, 'OOB'], col = 'red', type = 's')
legend('topright', legend = c('Bagging', 'Random Forest'), col = c('red', 'black'), lwd = 2)
#No need for more trees for either model

bag_pred <- predict(bag1, newdata = data_comb[6682:9346,], type = "response") #see ?predict.randomForest
rf_pred <- predict(rf, newdata = data_comb[6682:9346,])

ytest <- data_comb[6682:9346,8]

PredWithProbabilities <- predict(bag1, newdata = data_comb[6682:9346,], type = "prob") #see ?predict.randomForest
auc <- auc(test_b$Occupancy, PredWithProbabilities[,2])
auc


PredWithProbabilities <- predict(rf, newdata = data_comb[6682:9346,], type = "prob")
auc <- auc(test_b$Occupancy, PredWithProbabilities[,2])
auc


# changing the occupancy to integer

df4 <- train_b
df4$Occupancy <- as.integer(df4$Occupancy) - 1

# Using caret

d <- train_b
c <- test_b

d$Occupancy <- ifelse(d$Occupancy==1,'yes','no')
d$Occupancy <- as.factor(d$Occupancy)

c$Occupancy <- ifelse(c$Occupancy==1,'yes','no')
c$Occupancy <- as.factor(c$Occupancy)

objControl <- trainControl(method='cv', number=5, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)

gbmGrid <-  expand.grid(interaction.depth = c(1, 2, 3,4),
                        n.trees = (1:25)*20,
                        shrinkage = c(0.5, 0.1, 0.05),
                        n.minobsinnode = 10)

set.seed(1)
objModel <- train(d[,c(-1,-9)], d$Occupancy,
                  method='gbm',
                  trControl=objControl,
                  tuneGrid = gbmGrid,
                  metric = "ROC",
                  bag.fraction = 1)
summary(objModel)
objModel$finalModel

plot(varImp(objModel))

#Evaluate the model
predictions <- predict(object=objModel, c[,c(-1,-9)], type='prob')
auc <- roc(ifelse(c[,9] =="yes",1,0), predictions[[2]])



### Trees Results

Book1 <- read_excel("Book1.xlsx")
knitr::kable(Book1, digits = 5)

## Conclusion
