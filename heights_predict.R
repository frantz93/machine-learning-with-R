library(dslabs)
b <- as.data.frame(heights)

#In this exercise, we are going to predict the sex of students based on their heights

#required libraries
library(tidyverse)
library(ggplot2)
library(caret)

#recall on certain useful functions from other libraries
sample(x = 10, size = 7)  #display a list of 7 random values from 0 to 10
sample(c("M", "F"), 30, replace = TRUE) #display a list of 30 random values of either M or F
levels(heights$sex) #check the level of a variable
typeof(b$sex)  #check the type or dimension of a variable
typeof(b$height)
head(b)

#Now let's start...

#setting the test index, training set, and test set
set.seed(2, sample.kind = "Rounding")
test_index <- createDataPartition(b$sex, times = 1, p = 0.5, list = FALSE)
train_set <- b[-test_index, ]
test_set <- b[test_index, ]


#evaluating a simple algorithm based on guessing the sex of students
set.seed(2, sample.kind = "Rounding")
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>% factor(levels = levels(test_set$sex))  #predicted values based on guessing
mean(y_hat==test_set$sex) #we see that about 50,8% of the predicted values are correct when just guessing the sex. 

#let's see if we can do better based on the heights
#checking the statistics of heights by groups (male and female)
b %>% group_by(sex) %>% summarise(mean_height = mean(height), sd_height = sd(height)) #we see that males are slightly taller than female
#predict male if height is within two sd from the average male
69.3-2*3.61  #we will predict male if height is > 62
y_hat <- ifelse(train_set$height > 62, "Male", "Female") %>% factor(levels = levels(test_set$sex))
mean(y_hat == train_set$sex)
mean(y_hat == test_set$sex)    #NB. preventing overfitting require to evaluate the algorithm using the test_set, not the training set
                               #so we get an average of 75% righ prediction
#now using different cutoff of height for prediction
cutoff <- seq(61, 70, 1) #generate a vector of 10 cutoff values from 61 to 70

accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
}) #here we compute different prediction for different cutoff of heights

#we will now plot a graph to link each accuracy level to the respective cutoff
library(ggplot2)
ggplot(data.frame(), aes(x = cutoff, y = accuracy)) + geom_point() + geom_line()   ggtitle("The set of accuracy") + 
  theme(plot.title.position = "plot", panel.background = element_rect(fill = "lightblue"))
as.data.frame(rbind(cutoff, accuracy)) #best accuracy[4]=83.6% is reached with cutoff = 64

#evaluating the accuracy using the test set
y_hat <- ifelse(test_set$height > 64, "Male", "Female") %>% factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)  #we got an accuracy of 81,7%

#Now we are going to check the accuracy obtained with an econometric logistic regression model
reg <- glm(sex ~ height, family = binomial(link = "logit"), data = b)
b$predict_sex <- reg$fitted.values
summary(b$predict_sex)
b$predict_sex <- ifelse(b$predict_sex > 0.5, "Male", "Female") %>% factor(levels = levels(b$sex))
mean(b$predict_sex == b$sex)  #80.7% correct prediction in the whole sample

reg2 <- glm(sex ~ height, family = binomial(link = "logit"), data = train_set)
test_set$reg_psex <- predict(reg2, test_set, type = 'response')
summary(test_set$reg_psex)
test_set$reg_psex <- ifelse(test_set$reg_psex > 0.5, "Male", "Female") %>% factor(levels = levels(b$sex))
mean(test_set$reg_psex == test_set$sex)  #80.2% correct prediction in the test set

#checking the confusion matrix for the predicted sex with height > 64 = "Male" rule and logit reg estimates
table(y_hat)
table(test_set$sex)
table(predicted = y_hat, actual = train_set$sex)

test_set %>% mutate(y_hat = y_hat) %>% group_by(sex) %>% summarise(accuracy_r64 = mean(y_hat == sex), accuracy_logit = mean(reg_psex == sex))

confusionMatrix(data = y_hat, reference = test_set$sex)
confusionMatrix(data = test_set$reg_psex, reference = test_set$sex)
mean(c(0.31092,0.94581)) #correspond to the balanced average of sensitivity and specificity

#Calculating the F_1 score with the F_meas function from the caret package
F_meas(data = y_hat, reference = factor(test_set$sex)) #correspond to the harmonic average of sensitivity(TPR) and precision
2/((1/0.6891)+(1/0.5540541))
#we can also check for the cutoff that maximize the F1 score 
cutoff <- seq(61, 70, 1) #generate a vector of 10 cutoff values from 61 to 70
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
}) #here we compute different level of F1_score for predictions based on different cutoff of heights
max(F_1)
cutoff[which.max(F_1)]   #Max F1 score is 61.4% at the cutoff point 66.


#Comparing accuracy and F1-score : the Receiver Operating Characteristic curve (ROC)
#The ROC plots the sensibility(True positive rate) versus 1-specificity (False positive rate)
#Let's have the ROC for guessing the sex method and compare to the height cutoff based method
p <- 0.9
sample(c("Male", "Female"), length =length(train_set$sex),  replace = TRUE) %>% factor(levels = levels(train_set$sex))


#Other tips
t <- data.frame(nom = c("Frantz", "Betty", "Roberto", "Nhelissa"), sex = factor(c("M", "F", "M", "F"), levels = c("M", "F")))
View(t)
levels(t$sex)
summary(t$sex)
typeof(t$sex)

heights %>% group_by(sex) %>% summarise(mean(height), sd(height))
69.3-2*3.61
69.3+2*3.61

64.9-2*3.76
64.9+2*3.76
seq(61,70)

set.seed(3, sample.kind = "Rounding")
sample(x = 10, size = 5)
mnist <- as.data.frame(read_mnist())
typeof(mnist$train.labels)
head(mnist$train.images.1)
str(mnist)
list(mnist)
R.version


#Comprehensive check (PART 1) : basic algorithm
library(dslabs)
library(dplyr)
library(lubridate)
d <- as.data.frame(reported_heights)
head(d)

dat <- mutate(d, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)
head(dat)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

table(y, x)
round(prop.table(table(y, x), margin = 2), 2)  #proportion of female and male in each type of class

#we predict sex base on type of class;
#we assume that all inclass students are female and all online students are male based on the prevalence of sex in each type
y_hat <- ifelse(dat$type == "inclass", "Female", "Male") %>% factor(levels = c("Female", "Male"))
round(mean(y_hat == y),2)

table(y_hat, y)  #confusion matrix between y_hat and y
#now we calculate the sensitivity between y_hat and y
library(caret)
round(sensitivity(data = y_hat, reference = y), 2)  #report the sensitivity of the prediction
round(specificity(data = y_hat, reference = y), 2)  #report the specificity of the prediction
round(prop.table(table(y_hat, y), margin = 2), 2) #report the sensitivity and specificity of the prediction
round(mean(dat$sex == "Female"), 2)   #report the prevalance of female in the sample

#Comprehensive check (PART 2) : machine learning algorithm
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species
head(y)
#First we create the test and train partition
set.seed(2, sample.kind = "Rounding")
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
length(test_index)
test <- iris[test_index, ]
train <- iris[-test_index, ]
#now we look for the singular feature in the dataset that offer the highest overall accuracy to predict species categories
head(train)
summary(train)
train %>% group_by(Species) %>% summarise(sl = mean(Sepal.Length), sw = mean(Sepal.Width), pl = mean(Petal.Length),
                                          pw = mean(Petal.Width))
x_sl <- seq(5, 7.9, 0.1)
acc_seplen <- map_dbl(x_sl, function(x){
  y_hat <- ifelse(train$Sepal.Length > x, "virginica", "versicolor") %>% factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})
x_sw <- seq(2, 3.8, 0.1)
acc_sepw <- map_dbl(x_sw, function(x){
  y_hat <- ifelse(train$Sepal.Width > x, "virginica", "versicolor") %>% factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})
x_pl <- seq(3, 6.9, 0.1)
acc_petlen <- map_dbl(x_pl, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>% factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})
x_pw <- seq(1, 2.5, 0.1)
acc_petw <- map_dbl(x_pw, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>% factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})
m <- c(m_sl = max(acc_seplen), m_sw = max(acc_sepw), m_pl = max(acc_petlen), m_pw = max(acc_petw))
m   #the 'Petal.Length' feature bears the highest overall accuracy which is 96%
x_pl[which.max(acc_petlen)]   #the precedent accuracy corresponds to a cutoff of 4.7

#let's check the overall accuracy in the test dataset for the precedent best result
y_hat <- ifelse(test$Petal.Length > 4.7, "virginica", "versicolor") %>% factor(levels = levels(test$Species))
mean(y_hat == test$Species) #the accuracy is 90%

#testing if overtrain case
#we will repeat the same operations for determining accuracy levels but using the test dataset
x_sl <- seq(5, 7.9, 0.1)
acc_seplen <- map_dbl(x_sl, function(x){
  y_hat <- ifelse(test$Sepal.Length > x, "virginica", "versicolor") %>% factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
})
x_sw <- seq(2, 3.8, 0.1)
acc_sepw <- map_dbl(x_sw, function(x){
  y_hat <- ifelse(test$Sepal.Width > x, "virginica", "versicolor") %>% factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
})
x_pl <- seq(3, 6.9, 0.1)
acc_petlen <- map_dbl(x_pl, function(x){
  y_hat <- ifelse(test$Petal.Length > x, "virginica", "versicolor") %>% factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
})
x_pw <- seq(1, 2.5, 0.1)
acc_petw <- map_dbl(x_pw, function(x){
  y_hat <- ifelse(test$Petal.Width > x, "virginica", "versicolor") %>% factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
})
m <- c(m_sl = max(acc_seplen), m_sw = max(acc_sepw), m_pl = max(acc_petlen), m_pw = max(acc_petw))
m  #now, the 'Petal.Width' feature bears the highest overall accuracy which is 94%. Because the best feature has changed,.. 
    #in comparison to when we use the train dataset, we can conclude that using only one feature causes overtraining the algorithm.
x_pw[which.max(acc_petw)]   #this level of accuracy corresponds to a cutoff of 1.6

#now let's perform some exploratory analysis on the dataset
plot(iris, pch=21, bg=iris$Species)
head(iris)
summary(iris)
str(iris)

x_pl <- seq(3, 6.9, 0.1)
acc_petlen <- map_dbl(x_pl, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>% factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})
x_pw <- seq(1, 2.5, 0.1)
acc_petw <- map_dbl(x_pw, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>% factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})
m <- c(m_sl = max(acc_seplen), m_sw = max(acc_sepw), m_pl = max(acc_petlen), m_pw = max(acc_petw))
m   #the 'Petal.Length' and 'Petal.Width' features bear the 96% and 94% accuracy respectively
x_pl[which.max(acc_petlen)]   #best accuracy corresponds to a cutoff of 4.7 for "Petal.Length"
x_pw[which.max(acc_petw)]     #best accuracy corresponds to a cutoff of 1.5 for "Petal.Width"

#let's check the overall accuracy in the test dataset for the precedent best results combined together
y_hat <- ifelse(test$Petal.Length > 4.7 | test$Petal.Width > 1.5, "virginica", "versicolor") %>% 
  factor(levels = levels(test$Species))
mean(y_hat == test$Species) #the new accuracy is 88%.

#Comprehensive check (PART 3) : conditionnal probabilities 1
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))
round(mean(test == 1), 2)
caret::confusionMatrix(data = as.factor(test), reference = as.factor(disease), positive = "1")
table(test, disease)
16853/(16853+3065)
882426/(882426+97656)
16853/(16853+97656)
3065/(3065+882426)
mean(disease == 1)
mean(disease[test==0])
mean(disease[test==1])

#Comprehensive check (PART 3) : conditionnal probabilities 2
data(heights)
head(heights)
heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>%
  qplot(height, p, data =.)
heights[heights$height==50,]


ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)

Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
plot(dat)

ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  qplot(x, y, data =.)
xyg <- dat %>% mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE))
View(xyg)
