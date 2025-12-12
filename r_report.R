################################   Introduction   ##############################

# This code was written in RStudio Version 4.5.2

# In this project we investigate the impact of certain lifestyle factors as 
# well as physical characteristics on estimating obesity levels.
#
# We use the data set 'Estimation of Obesity Levels Based On Eating Habits and
# Physical Condition' from the UCI Machine Learning Repository.
#
# We aim to investigate which lifestyle factors and physical characteristics 
# have the largest impact on a person's obesity level, in order to discover
# how to reduce the risk of being classed as obese.
#
# As well as this, we want to learn which model is best for classifying the
# data and estimating a persons obesity level.

#############################  Preliminary set up  #############################

library(readr)
obesitydata <- read_csv("data/ObesityDataSet_raw_and_data_sinthetic.csv")

library(ggplot2)
# This package is needed to plot certain graphs

library(leaps)
# This package is needed to run the regsubsets() function

library(hnp)
library(MASS)
# These packages are needed to look at logistic regression

library(glmnet)
library(Matrix)
# These packages are needed to look at ridge regression

library(tree)
# This package is needed to look at classification trees

library(randomForest)
# This package is needed to look at random forests

library(gbm)
# This package is needed to look at boosting

library(deepnet)
library(neuralnet)
# These packages are needed for deep learning

###########################   Checking the data   ##############################

View(obesitydata)
# This opens the data set. Here we can see that the data has been loaded in 
# correctly, with all 17 columns. However, the headers (variable names) do not
# represent the data well. To make it clearer what the variables in each column
# represent, we change the header names as follows:

names(obesitydata) <- c("Gender","Age","Height","Weight","Family_History",
                        "High_Calories_Frequently","Vegetables","Main_meals",
                        "Between_Meals","Smoker","Water_Intake",
                        "Monitor_Calories","Physical_Activity","Technology",
                        "Alcohol_Intake","Transportation","Weight_Grouping")

# We now have the following variables which record the responses to these 
# questions:
#
# Gender: What is your gender? This is given as 'Male'/'Female'.
#
# Age: What is your age? This is given as an integer value.
#
# Height: What is your height? This is given as a numeric value in metres.
#
# Weight: What is your weight? This is given as a numeric value in kilograms.
#
# Family_History: Has a family member suffered or do they currently suffer from
# being overweight? This is given as a 'yes'/'np' response.
#
# High_Calories_Frequently: Do you eat high caloric food frequently? This is 
# given as a 'yes'/'no' response.
#
# Vegetables: Do you usually eat vegetables in your meals? This is given as the
# responses 'never'/'sometimes'/'always'.
#
# Main_Meals: How many main meals do you have daily? This is given as a 
# numerical value between 1 and 4.
#
# Between_Meals: Do you eat any food between meals? This has responses 'No'/
# 'Sometimes'/'Frequently'/'Always'.
# 
# Smoker: Do you smoke? This has responses 'yes' and 'no'.
#
# Water_Intake: How much water do you drink daily? This is given by a numerical
# value between 1 and 3.
#
# Monitor_Calories: Do you monitor the calories you eat daily? This is 
# represented as a 'yes'/'no' response.
#
# Physical_Activity: How often do you do physical activity? This is given as 
# continuous numerical value.
# 
# Technology: How much time do you use technological devices such as cell 
# cell phones, video games, television, computer and others? This is given as an
# integer value.
#
# Alcohol_Intake: How often do you drink alcohol? This has categorical responses
# consisting of 'no' for those who do not drink, 'Sometimes', 'Frequently',
# and 'Always'.
#
# Transportation: Which transportation do you usually use? This is categorical 
# with responses such as 'Automobile', 'Motorbike', 'Bike', 
# 'Public Transportation' and 'Walking'
#
# As well as this, we have NObeyesdad which categorizes the obesity level of 
# each individual, 

dim(obesitydata)
sum(is.na(obesitydata))
# The data does not contain any missing values so we do not need to delete any
# instances.

obesitydatadummy <- obesitydata
# Here we create a new data set so that we can add a dummy variable that has 
# value '1' if an individual is classified as being obese, and has value '0'
# otherwise. We do this by appending the data set with a new column containing 
# these conditional values, grouped using the following ifelse statements:
obesitydatadummy$Is_Obese <- with(obesitydatadummy, 
     ifelse(obesitydata$Weight_Grouping == "Normal_Weight","0",
     ifelse(obesitydata$Weight_Grouping == "Overweight_Level_I","0",
     ifelse(obesitydata$Weight_Grouping == "Overweight_Level_II","0",
     ifelse(obesitydata$Weight_Grouping == "Insufficient_Weight","0",
     ifelse(obesitydata$Weight_Grouping == "Obesity_Type_I","1",
     ifelse(obesitydata$Weight_Grouping == "Obesity_Type_II","1",
 
                                                                                                                                                                     ifelse(obesitydata$Weight_Grouping == "Obesity_Type_III","1","0"))))))))
obesitydatadummy$Is_Obese <- as.numeric(obesitydatadummy$Is_Obese)
# Here we change the category from a binary operator to a numerical one.

View(obesitydatadummy)
# We can view the changes made to the data set here.

obesitydatadummy$Weight_Grouping <- NULL
# Here we remove the 'Weight_Grouping' column from the data set in order to 
# perform best subset selection. If we did not remove this column, the 
# regsubsets() function would consider it to be the most influential variable 
# when looking at whether an individual is obese or not. We would expect this to
# be the case, seeing as we have set the dummy variables based on the values in 
# the 'Weight_Grouping' column.

########################## Preliminary Observations ############################

ggplot(obesitydatadummy, aes(x=Height, y= Weight, group=Is_Obese))+
         geom_point(aes(colour=Is_Obese))
# Here we have plotted the two variables most commonly associated with obesity,
# height and weight, against each other. The points are colour-coded, with the 
# darker shade of blue representing instances where the observation has been
# classed as not obese, and the lighter shade where they have been classed as 
# obese in our data set. 
#
# We can see there is a distinct split between the observations classed as obese
# and those not classed as obese. This split occurs in the y-direction, implying
# that weight has a larger influence on the classification than height does.
#
# We can see this split again by separating the two classifications:

ggplot(data=obesitydatadummy)+geom_point(mapping=aes(x = Height, y = Weight), 
                  pch=1) +facet_wrap(~Is_Obese, nrow=2)

# We can compare our initial plot of weight against height, to consider other
# variables:

ggplot(obesitydatadummy, aes(x=Height, y= Weight, group=Transportation))+
  geom_point(aes(colour=Transportation,size=Is_Obese),pch=1)
# Here we can see that out of those classed as obese, there are fewer instances
# where active forms of transport, such as cycling and walking, are used, 
# implying that this physical activity may be beneficial in preventing
# obesity.

ggplot(obesitydatadummy,aes(x=Height, y=Weight,group=High_Calories_Frequently))+
  geom_point(aes(colour=High_Calories_Frequently,size=Is_Obese),pch=1)
# Here we can see that those do not eat high numbers of calories frequently are
# less likely to have been classified as obese.

ggplot(obesitydatadummy, aes(x=Height, y= Weight, group=Gender))+
  geom_point(aes(colour=Gender, size=Is_Obese),pch=1)
# Here we can see that there doesn't appear to be any correlation as to whether 
# an individual is male or female and if they have been classed as obese.

ggplot(obesitydatadummy, aes(x=Height, y= Weight, group=Family_History))+
  geom_point(aes(colour=Family_History, size=Is_Obese),pch=1)
# Here we can see that of the observations classified as obese, the majority
# have had a family member who struggle/struggled with obesity.

# We can also consider some box plots:

boxplot(Physical_Activity ~ Is_Obese, data=obesitydatadummy, 
        xlab="Classed as Obese", ylab="Amount of Physical Activity")
# Here we see that, on average, those who do more physical activity are less 
# likely to be classed as obese, however the median amount of activity done
# by those classed as obese and those who are not is very small. This could 
# imply that the amount of physical activity an individual does may not have 
# a very large influence on the classification.


boxplot(Age ~ Is_Obese, data=obesitydatadummy, 
        xlab="Classed as Obese", ylab="Age")
# Here we can see than, on average, younger people are less likely to be 
# classed as obese, although there are a significant amount of outliers on both
# box plots. Those not classed as obese tend to be between 14 and 32 years of
# age, and those classed as obese tend to be between 16 and 37. the outliers 
# suggest that those between 50 and 60 are less likely to be obese.

boxplot(Main_meals ~ Is_Obese, data=obesitydatadummy, 
        xlab="Classed as Obese", ylab="Number of Main Meals a Day")
# Here we can see that those classed as obese are more likely to eat 3 meals a 
# day, although there are a significant number of outlier that suggest this is 
# not always the case. This is also seen as those not classed as obese eat
# 3 meals a day, on average.

boxplot(Technology ~ Is_Obese, data=obesitydatadummy, 
        xlab="Classed as Obese", ylab="Time Spent Using Technology")
# Here we can see that there is not much difference in the time spent using 
# technology between those classed as obese and those who are not. In fact, on 
# average those not classed as obese spend a larger amount of time using 
# technology on average.

boxplot(Weight ~ Is_Obese, data=obesitydatadummy, 
        xlab="Classed as Obese", ylab="Weight(kg)")
# here we can see that those who weigh more are more likely to be classed as 
# obese. This is what would we would expect to see as obesity is often 
# determined based on BMI, which uses weight in its calculations.

boxplot(Height~ Is_Obese, data=obesitydatadummy, 
        xlab="Classed as Obese", ylab="Height(m)")
# Here we can see that those who are taller are slightly more likely to be 
# categorized as obese. Taller people carry more mass due to their height, so 
# once again this is what we would expect if the classification is based on BMI.


###########################  Best Subset Selection  ############################

regfit <- regsubsets(obesitydatadummy$Is_Obese~.,obesitydatadummy)
summary(regfit)

# The regsubsets() function indicates which variables have the most influence on
# whether an individual is classed as being obese. The more asterisks each 
# variable has, the more influential it is. We can see that these variables are
# Height, Weight, High_Calories_Frequently, Between_Meals and Alcohol_Intake.
# This suggests that the best two-variable model contains only the variables 
# weight and height.

regfitsummary <- summary(regfit)
names(regfitsummary)
# This returns some of the models we can consider when trying to select the best
# model.

regfitsummary$rsq
# The R^2 statistic increases as more variables are included, from approximately
# 63% when only one variable is included, to almost 72% when 7 variables are 
# included.

# We can plot the models returned by the names() function and investigate these
# as potential fits for the data.

############################ Investigating Models ##############################

# Linear regression:
set.seed(1) #Set the seed so the results can be replicated 
attach(obesitydatadummy)

train <- sample(2111,1055)
# Here we create a training set for the data, splitting the observations in 
# half by selecting a random subset of size 1055 out of the 2111 original
# observations. Later, we use -train, the observations that are not included in
# the training set.

obesitydata_lm <- lm(Is_Obese ~ Height + Weight + High_Calories_Frequently + 
                       Between_Meals + Alcohol_Intake, subset=train)

mean((Is_Obese-predict(obesitydata_lm, obesitydatadummy))[-train]^2)
# Thus the estimated mean test error for the linear regression fit containing
# the variables 'Height', 'Weight', 'High_Calories_Frequently', 'Between_Meals'
# and 'Alcohol_Intake' is 0.07221 (4s.f.).

coef(obesitydata_lm)
# Here we see the coefficients of the variables in the linear regression model.

plot(obesitydata_lm)

# Earlier we saw that the data could be modeled using Cp, BIC and adjusted 
# R-squared. We now explore these models:

# RSS:
plot(regfitsummary$rss, xlab="Number of Variables",ylab="RSS", type="l")

plot(regfit, scale="r2")

# Adjusted R^2:
plot(regfitsummary$adjr2, xlab="Number of Variables", ylab="Adjusted R^2",
     type="l")
which.max(regfitsummary$adjr2)
points(8,regfitsummary$adjr2[8], col="green 4", cex=2, pch=20)
# The which.max() function returns the maximum of the adjusted R^2 statistic and
# is represented by the green point on the plot. For the adjusted R^2 statistic,
# the maximum is found when the model has 8 variables.
#
# For the adjusted R^2 statistic, the model with the largest adjusted R^2 value
# of 0.72 includes the variables 'GenderMale', 'Height', 'Weight',
# 'High_Calories_Frequentlyyes', 'Vegetables', 'Between_Mealsno', 'Technology'
# and 'Alcohol_Intakeno'. This is indicated by the shaded cells in the plot, 
# meaning that a variable is included, where each row represents a different
#model.
plot(regfit, scale="adjr2")
# Here we can see the coefficients of this 8 variable model:
coef(regfit,8)


# Cp:
plot(regfitsummary$cp, xlab="Number of Variables", ylab="Cp", type="l")
which.min(regfitsummary$cp)
points(8,regfitsummary$cp[8], col="orange", cex=2, pch=20)
# The which.min() function returns the number of variables for which the Cp
# statistic is minimized. We can see that here, the minimum of is found when the 
# model has 8 variables.
#
# Here we can see that the model with the minimum Cp statistic of 7.1 contains
# variables 'GenderMale', 'Height', 'Weight', 'High_Calories_Frequentlyyes', 
# 'Vegetables', 'Between_Mealsno', 'Technology' and 'Alcohol_Intakeno', as in 
# the 8 variable model for the adjusted R^2 statistic.
plot(regfit, scale="Cp")
# Here we can the coefficients of this 8 variable model:
coef(regfit,8)


# BIC:
plot(regfitsummary$bic, xlab="Number of Variables", ylab="BIC", type="l")
which.min(regfitsummary$bic)
points(6,regfitsummary$bic[6], col="cornflowerblue", cex=2, pch=20)
# The which.min() functions returns the number of variables for which the BIC
# statistic is minimized. Here, the minimum is found when the model has only 6 
# variables.
#
# The model with the minimum BIC statistic of -2600 contains the variables 
# 'GenderMale', 'Height', 'Weight', 'High_Calories_Frequentlyyes', 
# 'Between_Mealsno' and 'Alcohol_Intakeno'.
plot(regfit, scale="bic")
# Here we can see the coefficients of this 6 variable model:
coef(regfit,6)

###########################  Logistic Regression  ##############################

obesitydatadummy <- as.data.frame(lapply(obesitydatadummy,
                        function(x) if(is.character(x)) as.factor(x) else x))
obesitydata_glm <- glm(Is_Obese~ Gender + Age + Height + Family_History + 
                         High_Calories_Frequently + Vegetables + Main_meals + 
                         Between_Meals + Smoker + Water_Intake + 
                         Monitor_Calories + Physical_Activity + Technology + 
                         Alcohol_Intake + Transportation, data=obesitydatadummy,
                       family= binomial)
# Here we apply the generalized linear model (GLM) to the obesity data set,
# using family=binomial to apply the logistic model.
#
# With this data I found it was necessary to omit the 'Weight' variable in order
# to use the glm() function on the data set. However, this leads to a distorted
# linear regression model as we have seen that the 'Weight' variable is 
# highly influential in this data set.

summary(obesitydata_glm)
# In summarizing our logistic model we can see that the variables with the 
# smallest p-values are the following: 'Age', 'Family_History', 'Vegetables', 
# 'Transportation' and 'High_Calories_Frequently'. 'Between_Meals', 
# 'Monitor_Calories' and 'Physical_Activity' also have relatively small 
# p-values.
#
# These variables all have a p-value less than 0.001, indicating there is 
# evidence of a strong association between these variables and whether or not
# an individual is likely to be classed as obese.
#
# To look at the p-values only we can use the following:
summary(obesitydata_glm)$coef[,4]

coef(obesitydata_glm)
# Here we can see the coefficients of the variables for this logistic regression
# 
# We can reduce the number of variables in the logistic regression, using only
# only those that returned small p-values in our initial model:
obesitydata_glm2 <- glm(Is_Obese~Age + Family_History+Vegetables+Transportation+
                          High_Calories_Frequently, data=obesitydatadummy, 
                        family=binomial)
summary(obesitydata_glm2)
coef(obesitydata_glm2)

plot(obesitydata_glm)
plot(obesitydata_glm2)

# We can also look at the logistic model with one variable:

# 'Age' variable:
glm_age <- glm(Is_Obese~Age, data=obesitydatadummy)
summary(glm_age)
exp(summary(glm_age)$coef[2,1])
# This tells us that the higher a person's age, they more likely they are to
# be obese in the one variable model.

# 'Family_History' variable:
glm_family_history <- glm(Is_Obese~Family_History, data=obesitydatadummy)
summary(glm_family_history)
exp(summary(glm_family_history)$coef[2,1])
# This indicates that a person with a family history of obesity is 1.712 (4s.f.)
# times more likely to be an obese than a person without a family history of 
# obesity in the one variable model.

# 'Transportation' variable:
glm_transportation <- glm(Is_Obese~ Transportation, data=obesitydatadummy)
summary(glm_transportation)
exp(summary(glm_transportation)$coef[2:5,1])
# This suggests that those who use public transport and cycle are less likely to
# be obese than those who use other methods of transportation in the one 
# variable model.

#'High_Calories_Frequently' variable:
glm_high_calories <- glm(Is_Obese~High_Calories_Frequently, 
                         data=obesitydatadummy)
summary(glm_high_calories)
exp(summary(glm_high_calories)$coef[2,1])
# This implies that those who frequently consume high calories are 1.542 (4s.f)
# times more likely to be obese than those who do not frequently consume high
# calories in the one variable model.

# Conclusion on logistic regression model:
# 
# The logistic model we have used is not very accurate due to the omission of
# the weight variable. We have seen using linear regression, adjusted R^2, Cp 
# and BIC that the weight variable is one of the most influential variables in 
# the model, hence its omission will cause bias in favour of the other 
# variables. Therefore, the logistic regression model is not the most suitable
# method of investigating and drawing conclusions from this data set.

####################   Classification and Random Forests   #####################

# Classification:

tree.obesity <- tree(Is_Obese~., obesitydatadummy)
summary(tree.obesity)
# This tree has small deviance, implying that the tree provides a good fit to
# the data. Here the deviance is 34.27 and the residual mean deviance is 
# 0.01628 which is calculated by dividing the deviance by n-|T_0|, where |T_0| 
# is the number of terminal nodes, and n is the number of observations.
#
# Here we can see that this classification tree has 6 terminal nodes.
#
# We can also see, both from the summary and the plot of the classification 
# tree, that 'Weight' and 'Height' are the variables used in the construction of
# the tree.
plot(tree.obesity)
text(tree.obesity, pretty=0)

tree.obesity
# Here we can see the split criteria for each branch of the tree, as well as the 
# deviance and number of observations in that branch.

set.seed(1)
# Recall we have generated a training set 'train' consisting of a random sample
# of half of the obesity data set.

obesity.test <- obesitydatadummy[-train,]
Is_Obese.test <- Is_Obese[-train]
# Now we apply the classification tree to test data in order to calculate
# the test error. We use '-train' as this is the data not included in the 
# training data i.e. the test data.
tree.obesity2 <- tree(as.factor(Is_Obese)~., obesitydatadummy, subset=train)

tree_predict <- predict(tree.obesity2, obesity.test, type="class")
plot(tree.obesity2)
text(tree.obesity2, pretty=0)
# This tree has 10 terminal node and introduces the variable 'Technology'.

table(tree_predict,Is_Obese.test)
((552+489)/1055)*100
# We can see that this tree has lead to correct predictions for approximately 
# 98.7% of the test data.

# Next we want to investigate if pruning the tree will reduce the test error.
obesity.cv <- cv.tree(tree.obesity2, FUN=prune.misclass)
names(obesity.cv)
obesity.cv
# We can see here that the cv.tree() function returns the number of terminal 
# nodes for each of the considered tree as well as the associated error rate
# and a parameter k, the cost-complexity parameter.
#
# We can plot the error rates for these functions:
plot(obesity.cv$size, obesity.cv$dev, type="b")
# Here we can see that the lowest cross-validation error occurs when the tree
# has terminal nodes. The tree with 10 terminal nodes has 23 cross-validation
# errors.
plot(obesity.cv$k, obesity.cv$dev, type="b")

# As the tree we already have has 10 terminal nodes, there is no need to prune 
# the tree to try and further improve the model. Therefore, the tree with 10 
# terminal nodes has the highest classification accuracy, with approximately
# 98.7% of observations classified correctly.
summary(tree.obesity2)

# Random Forests:

set.seed(1) 
train <- sample(2111,1055)

obesity.bag <-randomForest(as.factor(Is_Obese)~., data=obesitydatadummy, mtry=5, 
                           subset=train,importance=TRUE)
obesity.bag
# Here we use the randomForest() function with m approximately equal to the 
# square root of the predictors (here m=5). The out-of-bag (OOB) error is low 
# here, at 0.76%.
((578+469)/1055)*100
# We can see that, compared to the classification tree, the random forest with
# m=5 has a higher number of observation correctly classified, with 
# approximately 99.2% of observations being classified correctly.

importance(obesity.bag)
varImpPlot(obesity.bag, sort=TRUE, main=" Variable Importance Plot")
# Here we can see the weight of the importance of each variable, with a mean
# decrease accuracy and a mean decrease Gini plot. The mean decrease accuracy
# plot demonstrates how much accuracy of the model is lost when a variable is 
# excluded. In our plot, we see that the 'Weight' and 'Height' variables being
# excluded would result in a less accurate classification. 
#
# Similarly in the Gini plot, the mean decrease in Gini coefficient measures 
# each variables contribution to the random forest, with the higher coefficients
# representing a higher importance of that variable in the model. Here we have 
# the variable 'Weight' with the largest coefficient, highlighting its 
# importance in the classification model.
     
# Boosting:

set.seed(1)

obesity.boost <- gbm(Is_Obese~., data= obesitydatadummy[train,], 
                     distribution = "bernoulli", n.trees=5000, 
                     interaction.depth=5)
# As we have a binary classification problem, we use distribution = "bernoulli"
# when fitting our boosted regression tree.

summary(obesity.boost)
# Once again, we can see, both in the relative influence plot and statistics 
# that 'Weight' and 'Height' are the most influential variables, followed by 
# 'Age'.

# Partial dependence plots:
plot(obesity.boost, i="Weight")
plot(obesity.boost, i="Height")
# These plots demonstrate the effect of these variables on whether an individual
# is classed as being obese or not after integrating out the other variables
# in the model. As we would expect, the likelihood of being classed as obese
# increases with weight and decreases with height.

##############################   Deep Learning  ################################

# Due to issues using the keras3 package, here we will use the deepnet and
# neuralnet packages to investigate fitting a neural network to our data set.

set.seed(1)
  
y <- as.matrix(obesitydatadummy$Is_Obese)
y <- as.numeric(y)
x <- scale(model.matrix(Is_Obese~.-1, data=obesitydatadummy))

obesity.neural <- nn.train(x,y)
yy <- nn.predict(obesity.neural,x)
print(head(yy))

yhat = matrix(0, length(yy),1)
yhat[which(yy > mean(yy))] =1
yhat[which(yy<=mean(yy))] = 0
obesity.confusion = table(y,yhat)
print(obesity.confusion)
# Here we can see that the leading diagonal of the confusion matrix contains
# most of the observations.
print(sum(diag(obesity.confusion))/sum(obesity.confusion)*100)
# This suggests that the neural network does a fairly good job of classifying
# the data.

obesity.df<- data.frame(cbind(x,y))
obesity.neural2 <- neuralnet(y~., data=obesity.df, hidden=5)
yy2 <- obesity.neural2$net.result[[1]]
yhat2 <- matrix(0,length(y),1)
yhat2[which(yy2> mean(yy2))]=1
yhat2[which(yy2<= mean(yy2))]=0

obesity.confusion2 <- table(y,yhat2)
obesity.confusion2
# This time we can see that the neural network performs better on this data set
# as the leading diagonal contains more of the observations.
sum(diag(obesity.confusion2))/sum(obesity.confusion2)*100

plot(obesity.neural2)      
# We can also plot the graph for this neural network

##############################    Conclusion    ################################

# Throughout our research we have consistently seen that weight has the largest 
# impact on the classification models when estimating a persons obesity levels.
# 
#
# We saw that the best adjusted-R^2 model contained 8 variables and had an 
# adjusted-R^2 statistic of 0.72  and includes the variables 'GenderMale', 
# 'Height', 'Weight', 'High_Calories_Frequentlyyes', 'Vegetables', 
# 'Between_Mealsno', 'Technology' and 'Alcohol_Intakeno'.
# 
# We also saw that the model with the minimum Cp statistic of 7.1 contains
# variables 'GenderMale', 'Height', 'Weight', 'High_Calories_Frequentlyyes', 
# 'Vegetables', 'Between_Mealsno', 'Technology' and 'Alcohol_Intakeno', as in 
# the 8 variable model for the adjusted R^2 statistic.
#
# As well as this, The model with the minimum BIC statistic of -2600 contains 
# the variables 'GenderMale', 'Height', 'Weight', 'High_Calories_Frequentlyyes', 
# 'Between_Mealsno' and 'Alcohol_Intakeno'.
#
# From these models, and the linear regression model, we can see that the 
# variables with the largest influence on the models tends to be 'Weight',
# 'Height', 'High_Calories_Frequently', 'Alcohol_Intake', 'Between_Meals'.
#
#
# Next we investigated the logistic regression model, however due to the large
# influence of the weight variable on the classification, the variable needed to
# be omitted from the model. This led to a less accurate model, suggesting that
# logistic regression is not as suitable for modelling this data compared to the
# aforementioned methods.
#
# From the logistic model we saw that 'Age', 'Family_History', 'Vegetables', 
# 'Transportation' and 'High_Calories_Frequently' were the most influential 
# variables. Interestingly, the 'Height' variable was not considered one of the 
# most influential variables. This likely due to the omission of the 'Weight' 
# variable leading to bias, as weight and height are linked and both are used in 
# the calculation of BMI.
#
#
# When looking at classification trees, we initially looked at the tree with 6
# terminal nodes which had mean deviance 0.01628, as well as 'Weight' and 
# 'Height' being the two classification variables as the split criteria.
#
# We then went on to look for the optimal classification tree by splitting the 
# data into a test and training set. We found that the tree with 10 terminal 
# nodes leads to correct predictions for approximately 98.7% of the test data.
#
# This tree is the optimal choice so no further pruning needed to be carried 
# out. As it correctly classified 98.7% of the data, the classification tree
# with 10 terminal nodes is a good choice for estimating a person's obesity
# level. The classification tree with 10 terminal modes has the split criteria
# variables 'Weight', 'Height' and 'Technology'.
#
# Looking at random forests, we found that the random tree with m=5 has a low 
# OOB error of 0.076%. This model correctly classified approximately 99.2%
# of the observations, making it a more accurate model than the classification
# tree with 10 terminal nodes. Once again, we saw that 'Weight' has the largest
# influence on classification, particularly in the mean decrease Gini plot,
# where the coefficient of the 'Weight' variable was far larger than the 
# coefficients of the other variables, highlighting how influential the 'Weight'
# variable is in the classification. The mean decrease accuracy plot showed that
# excluding the 'Weight' and 'Height' variables would lead to a less accurate
# classification model. This further shows that the logistic regression model
# is not ideal for modelling this data set as it requires the exclusion of the
# 'Weight' variable.
#
# Applying a boosting method showed yet again that 'Weight' and 'Height' were 
# the most influential variables, followed by 'Age'. We were able to plot 
# graphs that showed that the likelihood of being classed as obese increases 
# with weight and decreases with height.
#
# 
# Looking at neural networks, we can see that they have a high accuracy when it
# it comes to classifying the data set, with our second neural network having a
# higher level of accuracy, with the leading diagonal of the confusion matrix
# containing most of the observations.
#
#
# Overall, we have seen that classification trees, random forests and neural 
# networks are all suitable for classifying the obesity data set, with random
# forests and neural networks having the highest accuracy in classifying the 
# data. However, visually, neural networks are more complex, making them harder
# to interpret with a high number of variables. Therefore, classification trees
# and random forests are more suitable for visually seeing the influence each 
# variable has on the model.
#
# We have found that 'Weight' and 'Height' are the most influential variables
# when it comes to classification, which we expected due to obesity levels 
# often being linked to BMI, which is calculated using these two variables.

# We also saw that frequently consuming high numbers of calories and having a
# family history of obesity were linked to an observation being more likely to 
# be classified as obese. As well as this, observations where an individual 
# used a physical transport method such as walking were less likely to be 
# classed as obese.
#
# These findings are in line with what one might expect, however we must 
# remember that factors such as weight and transport lack context as weight does
# not necessarily refer to fat distribution as individuals with large muscle 
# mass can be classed as overweight. Similarly, those who use exercise-based 
# means of transport may only have short distances to travel, and those who 
# drive or use public transport may do more other physical activity, so we must
# be aware that this could have an impact on our findings.
#
# Whilst having a family history of obesity is not a variable that can be 
# changed, our findings suggest that those at risk of obesity could eat fewer 
# high calorific foods and use means of transport that involve exercise to 
# reduce the chances of being classed as obese, and therefore reducing the 
# likelihood of developing further health problems.


