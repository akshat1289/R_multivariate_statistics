getwd()
MarketingCampaign <- read.csv("marketing_campaign.csv", header = TRUE, sep = ";")
MarketingCampaign

class(MarketingCampaign)
#------------------------------- Cleaning data - START --------------------------

#Step 1: Familarizing with data: Checking the number of rows and columns the data frame has

dim(MarketingCampaign) #2240 rows and 29 columns

#Step 2: Checking for structural errors

#2.1 Correcting mislabelled variables

names(MarketingCampaign)[names(MarketingCampaign) == 'Dt_Customer'] <- 'Customer_Enrol_dt'
names(MarketingCampaign)[names(MarketingCampaign) == 'AcceptedCmp1'] <- 'Response_to_Camp1'
names(MarketingCampaign)[names(MarketingCampaign) == 'AcceptedCmp2'] <- 'Response_to_Camp2'
names(MarketingCampaign)[names(MarketingCampaign) == 'AcceptedCmp3'] <- 'Response_to_Camp3'
names(MarketingCampaign)[names(MarketingCampaign) == 'AcceptedCmp4'] <- 'Response_to_Camp4'
names(MarketingCampaign)[names(MarketingCampaign) == 'AcceptedCmp5'] <- 'Response_to_Camp5'

head(MarketingCampaign)


#2.2 Checking if all the classes are correct, 
#i.e if numeric values and character values for relevant columns makes sense

lapply(MarketingCampaign, class)

#Correcting the class

#MarketingCampaign$Year_Birth <- as.Date(as.character(MarketingCampaign$Year_Birth), format = "%Y%m%d")   
#class(MarketingCampaign$Year_Birth)

#MarketingCampaign$Customer_Enrol_dt <- as.Date(as.character(MarketingCampaign$Customer_Enrol_dt), format = "%Y%m%d")   
#class(MarketingCampaign$Customer_Enrol_dt)

is.numeric(MarketingCampaign$Income)

#2.3 Checking for duplicate IDs

MarketingCampaign[duplicated(MarketingCampaign$ID), ]
dim(MarketingCampaign)

#No duplicate IDs

#Step 3: 
#3.1 Checking for missing values in the entire dataframe

any(is.na(MarketingCampaign))

#Checking for the total number of missing values in the entire dataframe

sum(is.na(MarketingCampaign))
sum(is.na(MarketingCampaign$Income))

#Only income has missing values in data

library(tidyverse)
library("ggpubr")
ggdensity(MarketingCampaign$Income, fill = "lightgray")
#Replacing the missing income values with median income values

MarketingCampaign$Income[is.na(MarketingCampaign$Income)]<-median(MarketingCampaign$Income,na.rm=TRUE)
MarketingCampaign$Income

#3.2 Checking for outliers
head(MarketingCampaign)
# removing Customer_enroll_date as it is directly related to response
MarketingCampaign$Customer_Enrol_dt <- NULL
MarketingCampaign$..ID <- NULL

#Removing the ID variable

MarketingCampaign <- subset (MarketingCampaign, select = -1)
MarketingCampaign

#Adding Response variable
MarketingCampaign$LastResponse <- MarketingCampaign$Response
MarketingCampaign

# Creating Target variable Response which contains 1 if the customer has responded in 
#atleast one of the campigns

library(dplyr)
mutate(MarketingCampaign, Response = ifelse(MarketingCampaign$AcceptedCmp1 == 1 || MarketingCampaign$AcceptedCmp2 ==1 || MarketingCampaign$AcceptedCmp3 ==1 || MarketingCampaign$AcceptedCmp4 ==1 || MarketingCampaign$AcceptedCmp5 ==1 || MarketingCampaign$LastResponse==1, "1","0"))



#------------------------------- Cleaning data - END --------------------------#
# finding out the unique values in various columns:
unique(MarketingCampaign$Education)
unique(MarketingCampaign$Marital_Status)

MarketingCampaign$edu_cat <- as.factor(MarketingCampaign$Education)
unique(MarketingCampaign$edu_cat)

MarketingCampaign$marital_cat <- as.factor(MarketingCampaign$Marital_Status)
unique(MarketingCampaign$marital_cat)

# we will separate out effect of various campaigns run by the company #
# effect of campaign 1 #
# campaign_1 <- subset(MarketingCampaign, select = c(Response_to_Camp1, edu_cat, marital_cat, Income, Kidhome, Teenhome))
# 
# # we will try to study the response by customers to the campaign 1 #
# # taking out the training and testing data #
# 
# logic_1 <- glm(Response_to_Camp1 ~ edu_cat + marital_cat + Income + Kidhome + Teenhome, data = campaign_1, family = 'binomial')
# summary(logic_1)
# 
# campaign_2 <- subset(MarketingCampaign, select = c(Response_to_Camp2, edu_cat, marital_cat, Income, Kidhome, Teenhome))
# 
# logic_2 <- glm(Response_to_Camp2 ~ edu_cat + marital_cat + Income + Kidhome + Teenhome, data = campaign_2, family = 'binomial')
# summary(logic_2)

### box plot

boxplot(MarketingCampaign$NumWebPurchases,data=MarketingCampaign,col="light gray",
        ylab = "No of Web Purchases", xlab = "Web Purchase Data")
boxplot(MarketingCampaign$NumCatalogPurchases,data=MarketingCampaign,col="light gray",
        ylab = "No of Catalog Purchases", xlab = "Catalog Purchase Data")
boxplot(MarketingCampaign$NumStorePurchases,data=MarketingCampaign,col="light gray",
        ylab = "No of Store Purchases", xlab = "Store Purchase Data")
boxplot(MarketingCampaign$NumWebVisitsMonth,data=MarketingCampaign,col="light gray",
        ylab = "No of WebVisit Month", xlab = "Web Visit Data")
boxplot(MarketingCampaign$MntWines,data=MarketingCampaign,col="light gray",
        ylab = "No of MntWines", xlab = "Mnt Wines Data")
boxplot(MarketingCampaign$MntMeatProducts,data=MarketingCampaign,col="light gray",
        ylab = "No of Meat Products", xlab = "Meat Products Data")
boxplot(MarketingCampaign$MntFishProducts,data=MarketingCampaign,col="light gray",
        ylab = "No of Fish Products", xlab = "Fish Products Data")
boxplot(MarketingCampaign$MntSweetProducts,data=MarketingCampaign,col="light gray",
        ylab = "No of Sweet Products", xlab = "Sweet Products Data")
boxplot(MarketingCampaign$MntGoldProds,data=MarketingCampaign,col="light gray",
        ylab = "No of Gold Products", xlab = "Gold Products Data")
boxplot(MarketingCampaign$Income,data=MarketingCampaign,col="light gray",
        ylab = "Income", xlab = "Income data")

#--------------------------------------Analysis of web purchases---------------------------------------#

library(forecast)
library(lmtest)
library(sandwich)

###Assumption test for linear regression


library(lmtest)
library(sandwich)

#normality
shapiro.test(lm_web_1$residuals)  ###--- Normal

# non-constant variance in residuals
bptest(lm_web_1)

#Outlier test

outlierTest(lm_web_1)

lm_web_1 <- lm(NumWebPurchases ~ NumWebVisitsMonth, data = MarketingCampaign) 
coeftest(lm_web_1, vcov. = vcovHC(lm_web_1, type = "HC1"))

ggplot(data = MarketingCampaign) + geom_point(mapping = aes(x = NumWebVisitsMonth, y = NumWebPurchases)) +
  geom_abline(intercept = 4.42, slope = -0.06, color = 'red')

# As clearly depicted from the graph the regression line doesn't explain the relationship between the two.
# we need to add other variables #

lm_web_2 <- lm(NumWebPurchases ~ NumWebVisitsMonth + edu_cat + marital_cat + Kidhome + Teenhome + Income, data = MarketingCampaign)
coeftest(lm_web_2, vcov. = vcovHC(lm_web_2, type = "HC1"))

ggplot(data = MarketingCampaign) + geom_point(mapping = aes(x = NumWebVisitsMonth, y = NumWebPurchases)) +
  geom_abline(intercept = -0.292, slope = 0.34, color = 'red')

# we can do joint test. #

# now after adding other variables education, income, marital status; kids, teens at home #
# relation of web purchases with web visits becomes positive. And we come to know about other factors which play an important role#
# All marital status except alone, teen play an important role #

# As seen from the graph still the regression line can not  explain relationship between number of visits per month and number of web purchases.#
 
# now we will introduce quadratic, log - linear, linear - log and log - log regressions. #

### --- Assumptions of Logarthmic Regression---- ###

#1. Binary Logistic regression requires dependent variable to be binary
#2. logistic regression requires the observations to be independent of each other
#3. logistic regression requires there to be little or no multicollinearity among the independent variables
#4. logistic regression assumes linearity of independent variables and log odds
#5. logistic regression typically requires a large sample size

###---Testing assumptions


# log - linear regression#

# for using the logarithmic functions we need to take out the 0 from the columns. #
#log_data <- subset(MarketingCampaign, select = - MarketingCampaign[MarketingCampaign$NumWebPurchases == 0 | MarketingCampaign$NumWebVisitsMonth == 0, ])
log_data <- MarketingCampaign[MarketingCampaign$NumWebPurchases != 0, ]
log_data <- log_data[log_data$NumWebVisitsMonth != 0, ]

num_p_log <- log(log_data$NumWebPurchases)
num_v_log <- log(log_data$NumWebVisitsMonth)


lm_web_3 <- lm(log(NumWebPurchases) ~ NumWebVisitsMonth + edu_cat + marital_cat + Kidhome + Teenhome + Income, data = log_data)
coeftest(lm_web_3, vcov. = vcovHC(lm_web_3, type = "HC1"))

ggplot(data = log_data) + geom_point(mapping = aes(x = NumWebVisitsMonth, y = num_p_log)) +
  geom_abline(intercept = 0.088, slope = 0.088, color = 'red')

# log - log regression#

lm_web_4 <- lm(log(NumWebPurchases) ~ log(NumWebVisitsMonth) + edu_cat + marital_cat + Kidhome + Teenhome + Income, data = log_data)
coeftest(lm_web_4, vcov. = vcovHC(lm_web_4, type = "HC1"))

ggplot(data = log_data) + geom_point(mapping = aes(x = num_v_log, y = num_p_log)) +
  geom_abline(intercept = 0.064, slope = 0.37, color = 'red')

# Addition of Quadratic term  #

MarketingCampaign$num_visits_2 <- MarketingCampaign$NumWebVisitsMonth^2

lm_web_5 <- lm(NumStorePurchases ~ NumWebVisitsMonth + num_visits_2 + edu_cat + marital_cat + Kidhome + Teenhome + Income, data = MarketingCampaign)
coeftest(lm_web_5, vcov. = vcovHC(lm_web_5, type = 'HC1'))


# now we will calculate r square for every model we prepared. #
summary(lm_web_1)$r.squared
summary(lm_web_2)$r.squared
summary(lm_web_3)$r.squared
summary(lm_web_4)$r.squared
summary(lm_web_5)$r.squared

# best R square value comes out from the log log regression. 
# the meaning comes out - if there is one percent increase in number of visits online per month the number of sales online increase by 0.37 %.



##---------------------------------Response Analysis--------------------------------##

# 
# amt_fish <- sum(MarketingCampaign$MntFishProducts)
# amt_fish
# amt_meat <- sum(MarketingCampaign$MntMeatProducts)
# amt_meat
# amt_wine <- sum(MarketingCampaign$MntWines)
# amt_wine
# amt_gold <- sum(MarketingCampaign$MntGoldProds)
# amt_gold
# amt_fruit <- sum(MarketingCampaign$MntFruits)
# amt_fruit
# amt_sweet <- sum(MarketingCampaign$MntSweetProducts)
# amt_sweet
# 
# colnames(MarketingCampaign)
# 
# 
# # maximum amount spent is on wine. #
# 
# 
# # linear regression on maximum amount spent. #
# lm_max_amt <- lm(MntWines ~ LastResponse, data = MarketingCampaign)
# coeftest(lm_max_amt, vcov. = vcovHC(lm_max_amt, type = 'HC1'))
# 
# ggplot(data = MarketingCampaign) + geom_point(mapping = aes(x = LastResponse, y = MntWines)) +
#   geom_abline(intercept = 269.10, slope = 233.59, color = 'red')
# 
# lm_max_amt_2 <- lm(MntWines ~ LastResponse + NumDealsPurchases + NumCatalogPurchases + NumStorePurchases + 
#                      NumWebVisitsMonth + NumWebPurchases, data = MarketingCampaign)
# coeftest(lm_max_amt_2, vcov. = vcovHC(lm_max_amt_2, type = "HC1"))
# 
# # this shows about the hidden variable bias. #
# 
# ggplot(data = MarketingCampaign) + geom_point(mapping = aes(x = LastResponse, y = MntWines)) +
#   geom_abline(intercept = -197, slope = 128.17, color = 'red')
# 
# 
# # we can see that this linear model is unable to explain the relationship between Last response and amount spent on wine. #
# 
# 
# 
# # we will try other linear models for this #
# 
# lm_max_amt_1 <- lm(MntWines ~ NumCatalogPurchases, data = MarketingCampaign)
# coeftest(lm_max_amt_1, vcov. = vcovHC(lm_max_amt_1, type = 'HC1'))
# 
# ggplot(data = MarketingCampaign) + geom_point(mapping = aes(x = NumCatalogPurchases, y = MntWines)) +
#   geom_abline(intercept = 109.21, slope = 73.14, color = 'red')
# 
# 
# 
# 
# lm_max_amt_3 <- lm(MntWines ~ NumDealsPurchases + NumCatalogPurchases + NumStorePurchases + 
#                      NumWebVisitsMonth + NumWebPurchases + MntFishProducts + MntMeatProducts + MntGoldProds + MntFruits + 
#                      MntSweetProducts, data = MarketingCampaign)
# 
# coeftest(lm_max_amt_3, vcov. = vcovHC(lm_max_amt_3, type = "HC1"))
# 
# ggplot(data = MarketingCampaign) + geom_point(mapping = aes(x = NumCatalogPurchases, y = MntWines)) +
#   geom_abline(intercept = -194.47, slope = 38.27, color = 'red')
# 
# # we will try linear log regression #

#####-----------------------Response analysis--------------------------------####

logit_model<-glm(Response ~ MntFruits + MntWines + MntMeatProducts + MntFishProducts + MntSweetProducts + MntGoldProds, data = MarketingCampaign, family = binomial, maxit = 100) 
coeftest(logit_model, vcov. = vcovHC(logit_model, type = 'HC1'))

new_df <- subset(MarketingCampaign, select = c(Response,NumDealsPurchases, NumCatalogPurchases, NumWebPurchases, NumStorePurchases, NumWebVisitsMonth))




logit_model_1 <- glm(Response ~ NumDealsPurchases + NumWebPurchases + NumCatalogPurchases + NumStorePurchases +
                       NumWebVisitsMonth, data = new_df, family = binomial)
coeftest(logit_model_1, vcov. = vcovHC(logit_model_1, type = 'HC1'))

##--------------- now we can understand the effect of NumCatalogPurchases on response by the customer------------##
##-- we will hold all other variables at their mean values--##



new_df_p <- with(new_df, data.frame(NumDealsPurchases = mean(NumDealsPurchases), NumWebPurchases = mean(NumWebPurchases),
                                    NumStorePurchases = mean(NumStorePurchases), NumWebVisitsMonth = mean(NumWebVisitsMonth),
                                    NumCatalogPurchases = NumCatalogPurchases))

dim(new_df_p)


new_data_p_cat <- cbind(new_df_p, predict(logit_model_1, newdata = new_df_p, type = 'link', se = T))
new_data_p_cat <- within(new_data_p_cat, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

colnames(new_data_p_cat)

ggplot(data = new_data_p_cat) + geom_point(mapping = aes(x = NumCatalogPurchases, y = PredictedProb)) + 
  geom_hline(yintercept = 1, color = 'red', linetype = "solid") + geom_hline(yintercept = 0, color = 'red', linetype = "solid")


#ggplot(data = MarketingCampaign) + geom_point(mapping = aes(x = NumCatalogPurchases, y = Response)) +
  #geom_abline(intercept = -3.19, slope = 0.15, color = 'red')




#------------------------------- Linear REGRESSION ------------------------
# #MarketingCampaign
# 
# mymodel<-glm(Response ~ .-LastResponse -Response_to_Camp1 -Response_to_Camp2 -Response_to_Camp3 - Response_to_Camp4 -Response_to_Camp5,data=MarketingCampaign,family=binomial,maxit = 100)
# 
# MC1.lm <- lm(Response ~ .-LastResponse -Response_to_Camp1 -Response_to_Camp2 -Response_to_Camp3 - Response_to_Camp4 -Response_to_Camp5, data = MarketingCampaign)
# 
# summary(MC1.lm)
# summary(mymodel)
# 
# MC1.lm.step <- step(MC1.lm , direction = "both")
# summary(MC1.lm)

### -- Decision Tree model
selected.var <- c(1,5,6,9,12,13,14,15,16,17,26)
NMC<- data.frame(MarketingCampaign[,selected.var])
set.seed(1)
s<- sample(rownames(NMC), 700 ,replace=FALSE, ifelse (NMC$Response==1, 0.9, 0.01))
length(s)
FMC<-NMC[s,]
dim(FMC)
unique(FMC$Response)
dim(FMC[FMC$Response == 0,])
dim(FMC[FMC$Response == 1,])
train.rows <- sample(rownames(FMC),dim(FMC)[1]*0.7) 
valid.rows <- setdiff(rownames(FMC), train.rows) 
train.df <- FMC[train.rows, ]
valid.df <- FMC[valid.rows, ]

library(forecast)
FMC.pred.train <- predict(mymodel, data=train.df)
accuracy(FMC.pred.train, train.df$Response)

FMC.pred.valid <- predict(mymodel, data=valid.df)
accuracy(FMC.pred.valid, valid.df$Response)

# reducing dimensions with Decision tree
library(rpart)
library(rpart.plot)
set.seed(2)
Class.tree<- rpart(Response ~ ., data = FMC, 
                   control = rpart.control(), method = "class") 
##Plot tree

rpart.plot (Class.tree, type=1, extra=1, split.font=1, varlen=-10) 

rpart.rules(Class.tree)

###Prediction, and accuracy check
library(caret)
Class.tree.pred.train <- predict(Class.tree,train.df,type = "class")
Class.tree.pred.train
class (Class.tree.pred.train) # check data type
class (train.df$Response)

confusionMatrix(Class.tree.pred.train, as.factor(train.df$Response))# default positive value is 0 
confusionMatrix(Class.tree.pred.train, as.factor(train.df$Response), positive="1")

Class.tree.pred.valid <- predict(Class.tree,valid.df,type = "class")
Class.tree.pred.valid
class (Class.tree.pred.valid) # check data type
class (valid.df$Response)

confusionMatrix(Class.tree.pred.valid, as.factor(valid.df$Response))
confusionMatrix(Class.tree.pred.valid, as.factor(valid.df$Response), positive="1")

