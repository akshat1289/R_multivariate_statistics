##------------------------Final Project----------------------##
library(rpart)
library(rpart.plot)
library(caret)
library(Hmisc)
library(caTools)
library(neuralnet)
library(gains)
library(ROCR)
library(cars)
library(forecast)
fund_df <- read.csv("fundraising.csv")
#-------------------------Target B---------------------------#
#-----Question 1-------#
#summary statistics#
summary(fund_df)
#missing values#
is.null(fund_df)
# there are no null values in the dataset #
#checking the type of variables #
str(fund_df)
# removing the unnecessary columns #
new_fund_df <- fund_df[ , -c(1:6, 24)]
dim(new_fund_df)
# checking the distribution of columns in dataset#
hist.data.frame(new_fund_df[, c(1:12, 17)])



#-----Question 2--------#
# creating the train and validation dataset #
set.seed(41)
sample <- sample.split(new_fund_df, SplitRatio = 0.6)
train_fund <- subset(new_fund_df, sample == T)
test_fund <- subset(new_fund_df, sample == F)
View(train_fund)

#----Question 3-------#
# decision tree model #
tree_model <- rpart(TARGET_B ~ ., data = train_fund, method = 'class')
rpart.plot(tree_model, extra = 1, type = 1, varlen = -10)

# prediction #
pred_train <- predict(tree_model, train_fund, type = "class")
pred_valid <- predict(tree_model, test_fund, type = "class")
typeof(pred_train)
# preparing confusion matrix #
confusionMatrix(pred_train, as.factor(train_fund$TARGET_B))
confusionMatrix(pred_valid, as.factor(test_fund$TARGET_B))

# lift chart for train#
ROCR_pred_train <- prediction(as.numeric(pred_train), train_fund$TARGET_B)
ROCR_perf_train <- performance(ROCR_pred_train, 'tpr', 'fpr')
plot(ROCR_perf_train,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))

# lift chart for test #
ROCR_pred_valid <- prediction(as.numeric(pred_valid), test_fund$TARGET_B)
ROCR_perf_valid <- performance(ROCR_pred_valid, 'tpr', 'fpr')
plot(ROCR_perf_valid,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))


# building neural network #
train_fund$MAXRAMNT = scale(train_fund$MAXRAMNT)
train_fund$RAMNTALL = scale(train_fund$RAMNTALL)
train_fund$totalmonths = scale(train_fund$totalmonths)
train_fund$WEALTH = scale(train_fund$WEALTH)
nn_fund <- neuralnet(TARGET_B ~ MAXRAMNT + RAMNTALL + totalmonths + WEALTH, train_fund, hidden = 3, rep = 1, linear.output = F)
plot(nn_fund)

pred_nn_train <- predict(nn_fund, train_fund)
pred_nn_test <- predict(nn_fund, test_fund)

pred_nn_train <- data.frame(pred_nn_train)
pred_nn_train <- ifelse(pred_nn_train >= 0.50, 1, 0)
pred_nn_test <- ifelse(pred_nn_test >= 0.50, 1, 0)

# confusion matrix for neural network #
confusionMatrix(as.factor(pred_nn_train), as.factor(train_fund$TARGET_B))
confusionMatrix(as.factor(pred_nn_test), as.factor(test_fund$TARGET_B))

# lift chart for train #
ROCR_pred_train <- prediction(as.numeric(pred_nn_train), train_fund$TARGET_B)
ROCR_perf_train <- performance(ROCR_pred_train, 'tpr', 'fpr')
plot(ROCR_perf_train,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))


# lift chart for test #
ROCR_pred_valid <- prediction(as.numeric(pred_nn_test), test_fund$TARGET_B)
ROCR_perf_valid <- performance(ROCR_pred_valid, 'tpr', 'fpr')
plot(ROCR_perf_valid,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))


#----------------------Target D-------------------------#
# building the dataset for target D #
new_d_df <- fund_df[fund_df$TARGET_B == 1,] 
new_d_df <- new_d_df[, -c(1:6, 23)]
lm_1 <- lm(TARGET_D ~ ., data = new_d_df)
summary(lm_1)

# now taking only the statistically significant variables #
new_d_df <- new_d_df[, c("MAXRAMNT", "LASTGIFT", "totalmonths", "AVGGIFT", "TARGET_D")]
set.seed(1)
sample_1 <- sample.split(new_d_df, SplitRatio = 0.7)
train_lm_df <- subset(new_d_df, sample_1 == T)
test_lm_df <- subset(new_d_df, sample_1 == F)

lm_train <- lm(TARGET_D ~ ., data = new_d_df)

pred_lm_train <- predict(lm_train, train_lm_df)
pred_lm_test <- predict(lm_train, test_lm_df)

summary(lm_train)
forecast::accuracy(pred_lm_test, test_lm_df$TARGET_D)









