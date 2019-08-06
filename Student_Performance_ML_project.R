#Machine Learning project: Student Performance in Exams
#############################
# Students Performance
#############################
# Rita Levine
# Marcus Miles
# Rafael Nunez
#############################
library(car)
library(MASS)
library(ggplot2)
library(caTools)
library(fastDummies)
library(tidyr)
library(tidyverse)
library(psych)
library(expss)
library(rattle)
library(sjlabelled)

stud_perf <- read.csv(file.choose())

##### Descriptives #####
names(stud_perf)
summary(stud_perf)
describe(stud_perf)
str(stud_perf)

#Create dummy variables for each of the factors in the dataset
stud_perf_new <- dummy_cols(stud_perf,select_columns = c("gender","race.ethnicity","parental.level.of.education","lunch","test.preparation.course"),
                            remove_first_dummy  = TRUE)

View(stud_perf_new)

##### Quantitative Variables Plots #####
plot(stud_perf)
hist(stud_perf$math.score)
hist(stud_perf$reading.score)     
hist(stud_perf$writing.score)

##### GGPlots #####

ggplot(stud_perf, aes(x=gender, y=math.score, fill = gender))+
  geom_boxplot() + labs(title = "Math scores by Gender")

ggplot(stud_perf, aes(x=gender, y=reading.score, fill = gender))+
  geom_boxplot() + labs(title = "Reading scores by Gender")

ggplot(stud_perf, aes(x=gender, y=writing.score, fill = gender))+
  geom_boxplot() + labs(title = "Writing scores by Gender")

ggplot(stud_perf, aes(x=parental.level.of.education, y=math.score, fill = gender))+
  geom_boxplot() + labs(title = "Math scores by Parent's Level of Education")

ggplot(stud_perf, aes(x=parental.level.of.education, y=reading.score, fill = gender))+
  geom_boxplot() + labs(title = "Reading scores by Parent's Level of Education")

ggplot(stud_perf, aes(x=parental.level.of.education, y=writing.score, fill = gender))+
  geom_boxplot() + labs(title = "Writing scores by Parent's Level of Education")

ggplot(stud_perf, aes(x=race.ethnicity, y=math.score, fill = gender))+
  geom_boxplot() + labs(title = "Math scores by Race/Ethnicity")

ggplot(stud_perf, aes(x=race.ethnicity, y=reading.score, fill = gender))+
  geom_boxplot() + labs(title = "Reading scores by Race/Ethnicity")

ggplot(stud_perf, aes(x=race.ethnicity, y=writing.score, fill = gender))+
  geom_boxplot() + labs(title = "Writing scores by Race/Ethnicity")

ggplot(stud_perf, aes(x=lunch, y=math.score, fill = gender))+
  geom_boxplot() + labs(title = "Math scores by Lunch")

ggplot(stud_perf, aes(x=lunch, y=reading.score, fill = gender))+
  geom_boxplot() + labs(title = "Reading scores by Lunch")

ggplot(stud_perf, aes(x=lunch, y=writing.score, fill = gender))+
  geom_boxplot() + labs(title = "Writing scores by Lunch")

ggplot(stud_perf, aes(x=test.preparation.course, y=math.score, fill = gender))+
  geom_boxplot() + labs(title = "Math scores by Test Prep Course Completion")

ggplot(stud_perf, aes(x=test.preparation.course, y=reading.score, fill = gender))+
  geom_boxplot() + labs(title = "Reading scores by Test Prep Course Completion")

ggplot(stud_perf, aes(x=test.preparation.course, y=writing.score, fill = gender))+
  geom_boxplot() + labs(title = "Writing scores by Test Prep Course Completion")

#Create train and test sets for regression
stud_perf_new[c(-1,-2,-3,-4,-5)] %>% dplyr::sample_frac(.8) -> train
stud_perf_new[c(-1,-2,-3,-4,-5)] %>% dplyr::anti_join(train) -> test

#linear regression for predicting student perfomance for each subject
#Used backward elimination to choose the best independent variables for the models
math_fit <- lm(math.score ~ . - `parental.level.of.education_master's degree` 
               - `race.ethnicity_group C` - `parental.level.of.education_some college` - `parental.level.of.education_associate's degree` -
                 `race.ethnicity_group A` - `race.ethnicity_group D` - 
                 `parental.level.of.education_some high school` - `parental.level.of.education_high school`, data = train)
summary(math_fit)

math_pred <- predict(math_fit, newdata = test)
math_pred
summary(math_pred)

actual_pred <- data.frame(cbind(actuals=test$math.score, predicteds=math_pred))
corr_accuracy <- cor(actual_pred) #93.1% accuracy
head(actual_pred)

read_fit <- lm(reading.score ~. - `race.ethnicity_group C` - `race.ethnicity_group E` -
                 `parental.level.of.education_master's degree` - `race.ethnicity_group A` -
                 gender_male - `parental.level.of.education_some college` - 
                 `parental.level.of.education_associate's degree`, data = train)
summary(read_fit)

read_pred <- predict(read_fit, newdata = test)
summary(read_pred)

actual_pred <- data.frame(cbind(actuals=test$reading.score, predicteds=read_pred))
corr_accuracy <- cor(actual_pred) #96.3% accuracy
head(actual_pred)

write_fit <- lm(writing.score ~. - `race.ethnicity_group A` - `parental.level.of.education_master's degree` 
                - `race.ethnicity_group C` - `lunch_free/reduced`, data = train)
summary(write_fit)

write_pred <- predict(write_fit, newdata = test)
summary(write_pred)

actual_pred <- data.frame(cbind(actuals=test$writing.score, predicteds=write_pred))
corr_accuracy <- cor(actual_pred) #97.7% accuracy
head(actual_pred)

write_pred2 <- predict(write_fit, newdata = test, interval = "prediction", level=0.95)
math_pred2 <- predict(math_fit, newdata = test, interval = "prediction", level=0.95)
read_pred2 <- predict(read_fit, newdata = test, interval = "prediction", level=0.95)
head(write_pred2)

#Create Regression model plot; Visualization at 95% confidence and prediction intervals.
pred_data <- cbind(test, write_pred2)

ggplot(pred_data, aes(fit, writing.score)) +
  geom_point() +
  stat_smooth(method = lm) + 
  geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed") +
  xlab("Predicted Scores") + ylab("Test Scores") +
  labs(title = "95% conf and pred intervals for Writing Scores")

pred_data <- cbind(test, math_pred2)

ggplot(pred_data, aes(fit, math.score)) +
  geom_point() +
  stat_smooth(method = lm) + 
  geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed") +
  xlab("Predicted Scores") + ylab("Test Scores") +
  labs(title = "95% conf and pred intervals for Math Scores")

pred_data <- cbind(test, read_pred2)

ggplot(pred_data, aes(fit, reading.score)) +
  geom_point() +
  stat_smooth(method = lm) + 
  geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed") +
  xlab("Predicted Scores") + ylab("Test Scores") +
  labs(title = "95% conf and pred intervals for Reading Scores")

#----------------------------------------------------------------------------
#Create a model that classifies students on whether they get accepted or not based on the 
#average of the three test scores being above 80
stud_perf_new$average.score <- rowMeans(stud_perf_new[c(6,7,8)])
stud_perf_new$accepted <- ifelse(stud_perf_new$average.score >= 80, 1,0)

#Create Train/Test sets for logistic regression model
stud_perf_new[c(-1,-2,-3,-4,-5,-6,-7,-8)] %>% dplyr::sample_frac(.8) -> train
stud_perf_new[c(-1,-2,-3,-4,-5,-6,-7,-8)] %>% dplyr::anti_join(train) -> test

#Create logistic Regression model
train$accepted <- as.factor(train$accepted)
str(train)
view(train)
glm_control <- caret::trainControl(method = 'cv', number = 5, returnResamp = "final")

x <- train[,-14]
y <- train$accepted

#build the model 5-fold Cross Validation model for Logistic Regression
glm_fit <- caret::train(x,y, method = 'glm', 
                        trControl = glm_control,trace = FALSE)

glm_fit$results #Training model produced 99.1% accuracy with a KAPPA stat of 0.972

glm_pred <- predict(glm_fit, test, type = 'prob')
colnames(glm_pred) <- c('zero','one')

InformationValue::confusionMatrix(test$accepted, glm_pred$one)

accuracy <- (149 + 46)/(1+0+149+46)
accuracy #99.5% accuracy

error_rate <- 1 - accuracy
error_rate#0.01% error rate 

#The model correctly classified 99.5% of the test data 

#Produces ROC plot
InformationValue::plotROC(test$accepted, glm_pred$one)
