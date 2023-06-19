# A

# read file into the R
diabetes <- read.csv("diabetes.csv", header = TRUE)

# get an overview of the data
head(diabetes)

# number of rows and columns in the dataset
dim(diabetes)

# determine the data types of the variables in the data set
library(dplyr)
glimpse(diabetes)

# check if there are any missing values present in form of NA
anyNA(diabetes)




# B - Missing Values

#find out how many missing values are in the dataset
sum(diabetes %>% select("Pregnancies": "Age") ==0)

# Discard the Pregnancies variable
clean_diabetes = select(diabetes, -Pregnancies)


#Replace missing entries with NA
install.packages("naniar")
library(naniar)

clean_diabetes = replace_with_na_at(clean_diabetes, .vars = c("Glucose", "BloodPressure", "SkinThickness", "Insulin","BMI", "DiabetesPedigreeFunction", "Age"), ~.x == 0)






# C - Summary Statistics and Visualizations


attach(clean_diabetes)

#visualize the distribution of the variables
par(mfrow = c(3,3))
hist(Glucose)
hist(BloodPressure)
hist(SkinThickness)
hist(Insulin)
hist(BMI)
hist(DiabetesPedigreeFunction)
hist(Age)
hist(Outcome)
barplot(table(Outcome), main = "Barplot of Outcome Variable", xlab = "Outcome", ylab = "Number of Individuals")

#provide the summary statistic
summary(clean_diabetes)






# D - Statistical test for differences of all the factors


#subset data according to outcome
diabetic <- subset(clean_diabetes, Outcome == 1, select = Glucose: Outcome)
non_diabetic = subset(clean_diabetes, Outcome == 0, select = Glucose : Outcome)

#statistical test for differences for glucose levels
par(mfrow = c(1,2))
qqnorm(diabetic$Glucose, main = "Glucose levels for diabetic patients") 
qqline(diabetic$Glucose)
qqnorm(non_diabetic$Glucose, main = "Glucose levels for non-diabetic patients")
qqline(non_diabetic$Glucose)

ks.test(diabetic$Glucose, "pnorm", mean = mean(diabetic$Glucose, na.rm = TRUE), sd = sd(diabetic$Glucose, na.rm = TRUE))
ks.test(non_diabetic$Glucose, "pnorm", mean = mean(non_diabetic$Glucose, na.rm = TRUE), sd = sd(non_diabetic$Glucose, na.rm = TRUE))

wilcox.test(diabetic$Glucose, non_diabetic$Glucose, alternative = "two.sided")



#statistical test for differences for blood pressure
par(mfrow = c(1,2))
qqnorm(diabetic$BloodPressure, main = "Blood pressure for diabetic patients") 
qqline(diabetic$BloodPressure)
qqnorm(non_diabetic$BloodPressure, main = "Blood pressure for non_diabetic patients") 
qqline(non_diabetic$BloodPressure)

ks.test(diabetic$BloodPressure, "pnorm", mean = mean(diabetic$BloodPressure, na.rm = TRUE), sd = sd(diabetic$BloodPressure, na.rm = TRUE))
ks.test(non_diabetic$BloodPressure, "pnorm", mean = mean(non_diabetic$BloodPressure, na.rm = TRUE), sd = sd(non_diabetic$BloodPressure, na.rm = TRUE))

t.test(diabetic$BloodPressure, non_diabetic$BloodPressure, paired = FALSE)



#statistical test for differences for skin thickness
par(mfrow = c(1,2))
qqnorm(diabetic$SkinThickness, main = "Skin thickness for diabetic patients") 
qqline(diabetic$SkinThickness)
qqnorm(non_diabetic$SkinThickness, main = "Skin thickness for non-diabetic patients") 
qqline(non_diabetic$SkinThickness)

ks.test(diabetic$SkinThickness, "pnorm", mean = mean(diabetic$SkinThickness, na.rm = TRUE), sd = sd(diabetic$SkinThickness, na.rm = TRUE))
ks.test(non_diabetic$SkinThickness, "pnorm", mean = mean(non_diabetic$SkinThickness, na.rm = TRUE), sd = sd(non_diabetic$SkinThickness, na.rm = TRUE))

t.test(diabetic$SkinThickness, non_diabetic$SkinThickness, paired = FALSE)



#statistical test for differences for Insulin
par(mfrow = c(1,2))
qqnorm(diabetic$Insulin, main = "Insulin for diabetic patients") 
qqline(diabetic$Insulin)
qqnorm(non_diabetic$Insulin, main = "Insulin for non-diabetic patients") 
qqline(non_diabetic$Insulin)

ks.test(diabetic$Insulin, "pnorm", mean = mean(diabetic$Insulin, na.rm = TRUE), sd = sd(diabetic$Insulin, na.rm = TRUE))
ks.test(non_diabetic$Insulin, "pnorm", mean = mean(non_diabetic$Insulin, na.rm = TRUE), sd = sd(non_diabetic$Insulin, na.rm = TRUE))

wilcox.test(diabetic$Insulin, non_diabetic$Insulin, alternative = "two.sided")



#statistical test for differences for BMI
par(mfrow = c(1,2))
qqnorm(diabetic$BMI, main = "BMI for diabetic patients") 
qqline(diabetic$BMI)
qqnorm(non_diabetic$BMI, main = "BMI for non-diabetic patients") 
qqline(non_diabetic$BMI)

ks.test(diabetic$BMI, "pnorm", mean = mean(diabetic$BMI, na.rm = TRUE), sd = sd(diabetic$BMI, na.rm = TRUE))
ks.test(non_diabetic$BMI, "pnorm", mean = mean(non_diabetic$BMI, na.rm = TRUE), sd = sd(non_diabetic$BMI, na.rm = TRUE))

wilcox.test(diabetic$BMI, non_diabetic$BMI, alternative = "two.sided")



#statistical test for differences for Diabetes Pedigree Function
par(mfrow = c(1,2))
qqnorm(diabetic$DiabetesPedigreeFunction, main = "DPF for diabetic patients") 
qqline(diabetic$DiabetesPedigreeFunction)
qqnorm(non_diabetic$DiabetesPedigreeFunction, main = "DPF for non-diabetic patients") 
qqline(non_diabetic$DiabetesPedigreeFunction)

ks.test(diabetic$DiabetesPedigreeFunction, "pnorm", mean = mean(diabetic$DiabetesPedigreeFunction, na.rm = TRUE), sd = sd(diabetic$DiabetesPedigreeFunction, na.rm = TRUE))
ks.test(non_diabetic$DiabetesPedigreeFunction, "pnorm", mean = mean(non_diabetic$DiabetesPedigreeFunction, na.rm = TRUE), sd = sd(non_diabetic$DiabetesPedigreeFunction, na.rm = TRUE))

wilcox.test(diabetic$DiabetesPedigreeFunction, non_diabetic$DiabetesPedigreeFunction, alternative = "two.sided")



#statistical test for differences for Diabetes Pedigree Function
par(mfrow = c(1,2))
qqnorm(diabetic$Age, main = "Age for diabetic patients")
qqline(diabetic$Age)
qqnorm(non_diabetic$Age, main = "Age for non-diabetic patients") 
qqline(non_diabetic$Age)

ks.test(diabetic$Age, "pnorm", mean = mean(diabetic$Age, na.rm = TRUE), sd = sd(diabetic$Age, na.rm = TRUE))
ks.test(non_diabetic$Age, "pnorm", mean = mean(non_diabetic$Age, na.rm = TRUE), sd = sd(non_diabetic$Age, na.rm = TRUE))

wilcox.test(diabetic$Age, non_diabetic$Age, alternative = "two.sided")


# Boxplot to visualize differences in central tendencies

par(mfrow = c(2,4))
boxplot(diabetic$Glucose, non_diabetic$Glucose, main = "Glucose", xlab = "Diabetic  |  Non-diabetic")
boxplot(diabetic$BloodPressure, non_diabetic$BloodPressure, main = "Blood Pressure", xlab = "Diabetic  |  Non-diabetic")
boxplot(diabetic$SkinThickness, non_diabetic$SkinThickness, main = "Skin Thickness", xlab = "Diabetic  |  Non-diabetic")
boxplot(diabetic$Insulin, non_diabetic$Insulin, main = "Insulin", xlab = "Diabetic  |  Non-diabetic")
boxplot(diabetic$BMI, non_diabetic$BMI, main = "BMI", xlab = "Diabetic  |  Non-diabetic")
boxplot(diabetic$DiabetesPedigreeFunction, non_diabetic$DiabetesPedigreeFunction, main = "DPF", xlab = "Diabetic  |  Non-diabetic")
boxplot(diabetic$Age, non_diabetic$Age, main = "Age", xlab = "Diabetic  |  Non-diabetic")





# E -  Checking for correlation

## Visualising the Correlation

install.packages("psych")
library(psych)
pairs.panels(clean_diabetes[, -8], method = "spearman", hist.col = "blue", density = TRUE)

# Test for Correlation
install.packages("easystats")
library(correlation)
library(easystats)
#easystats::install_latest()
#install.packages("correlation", repos = "https://easystats.r-universe.dev")

corr_matrix = correlation(clean_diabetes,method = "spearman" )
summary(corr_matrix)
j <- corr_matrix[,4]
j
corr_matrix

install.packages("Hmisc")
library(Hmisc)
f <- rcorr(as.matrix(clean_diabetes[,-8]), type = "spearman")
f
f[["P"]]








# F

# Binary Logistic Regression
check <- glm(formula = Outcome ~ Glucose + BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age, family =  "binomial", data = clean_diabetes)
summary(check)

# Assumption checks for Binary Logistic Regression

## Variance Inflation Factor to check for multicolinearity assumption
install.packages("car")
library(car)
vif(check)
vif_values <- vif(check)

## Visual test for multicolinearity
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")
abline(v = 5, lwd = 3, lty = 2)

## Visaulisation for linearity assumption check
library(tidyverse)
library(broom)
install.packages("regplot")
library(regplot)
library(gridExtra)

par(mfrow = c (3,3))
logplot1 <- ggplot(clean_diabetes, aes(x = Glucose, y = Outcome)) + geom_point(alpha = .5) + stat_smooth(method = "glm", se = FALSE, method.args = list(family = binomial))
logplot2 <- ggplot(clean_diabetes, aes(x = BloodPressure, y = Outcome)) + geom_point(alpha = .5) + stat_smooth(method = "glm", se = FALSE, method.args = list(family = binomial))
logplot3 <- ggplot(clean_diabetes, aes(x = SkinThickness, y = Outcome)) + geom_point(alpha = .5) + stat_smooth(method = "glm", se = FALSE, method.args = list(family = binomial))
logplot4 <- ggplot(clean_diabetes, aes(x = Insulin, y = Outcome)) + geom_point(alpha = .5) + stat_smooth(method = "glm", se = FALSE, method.args = list(family = binomial))
logplot5 <- ggplot(clean_diabetes, aes(x = BMI, y = Outcome)) + geom_point(alpha = .5) + stat_smooth(method = "glm", se = FALSE, method.args = list(family = binomial))
logplot6 <- ggplot(clean_diabetes, aes(x = DiabetesPedigreeFunction, y = Outcome)) + geom_point(alpha = .5) + stat_smooth(method = "glm", se = FALSE, method.args = list(family = binomial))
logplot7 <- ggplot(clean_diabetes, aes(x = Age, y = Outcome)) + geom_point(alpha = .5) + stat_smooth(method = "glm", se = FALSE, method.args = list(family = binomial))

logplots <- list(logplot1, logplot2, logplot3, logplot4, logplot5, logplot6, logplot7)
grid.arrange(grobs = logplots, top = "")


## Checking for Influential Outliers
install.packages("olsrr")
library(olsrr)

plot(check, which = 4, id.n = 5)

cooksd <- cooks.distance(check)
sample_size <- nrow(clean_diabetes)
plot(cooksd, pch="*", cex=2, main="Influential Outliers by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line


## Test for Goodness of fit
install.packages("ResourceSelection")
library(ResourceSelection)

fitness_test <- hoslem.test(check$y, fitted(check), g = 10)
fitness_test

cbind(fitness_test$expected, fitness_test$observed)








# G


### predictions for glucose in r


## Prediction for Missing Values of Glucose

glucose_fit <- lm(Glucose~Age)
summary(glucose_fit)
predic <- predict(glucose_fit, subset(clean_diabetes, is.na(Glucose)), type="response")

# visualization for linear regression model assumptions
par(mfrow=c(2,2))

#check for linearity
plot(Age, Glucose, main = "Scatterplot of Glucose vs Age")
abline(lm(Glucose ~ Age))


# normality of residuals
gluc_residuals = residuals(glucose_fit)
hist(gluc_residuals, main = "Distribution of Residuals", xlab = "Residuals")

gluc_residuals

qqnorm(gluc_residuals, main = "Q-Q Plot of Residuals")
qqline(gluc_residuals)

# check for homoscedasticity
fitted_values= fitted.values(glucose_fit)
plot(fitted_values, gluc_residuals, main = "Scatterplot of Residuals over Fitted Values", ylab = "Residuals")
abline(h = 0)

