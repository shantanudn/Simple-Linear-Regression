# Load wc_at.csv dataset
library(readr)
calories_consumed <- read.csv("C:/Training/ExcelR/Simple Linear Regression/calories_consumed.csv")
View(calories_consumed)

# Exploratory data analysis
summary(calories_consumed)

#Attaching dataset
attach(calories_consumed)

#Exploratory Data Analysis
boxplot(Weight.gained..grams.,Calories.Consumed)
hist(Weight.gained..grams.)
hist(Calories.Consumed)


#Scatter plot
plot(Calories.Consumed,Weight.gained..grams.)  # plot(X,Y)
    #In the above plot weight gained is the dependent variable, hence it is Y)


#Correlation Coefficient (r)
cor(Calories.Consumed,Weight.gained..grams.)             # cor(X,Y)

# Simple Linear Regression model
model1 <- lm(Calories.Consumed ~ Weight.gained..grams.) # lm(Y ~ X)

summary(model1)



####################

# Logrithamic Model

# x = log(Waist); y = AT

plot(log(Calories.Consumed),Weight.gained..grams.)
cor(log(Calories.Consumed),Weight.gained..grams.)

model1_log <- lm(Calories.Consumed ~ log(Weight.gained..grams.))   # lm(Y ~ X)

summary(model1_log)

######################

# Exponential Model

# x = Waist and y = log(AT)

plot(Calories.Consumed, log(Weight.gained..grams.))

cor(Calories.Consumed, log(Weight.gained..grams.))

model1_exp <- lm(log(Calories.Consumed) ~ Weight.gained..grams.)  #lm(log(Y) ~ X)

summary(model1_exp)



##############################
# Polynomial model with 2 degree (quadratic model)

plot(Calories.Consumed,Weight.gained..grams.)
plot(Calories.Consumed*Calories.Consumed,Weight.gained..grams.)

cor(Calories.Consumed,Weight.gained..grams.,Weight.gained..grams.)

plot(Calories.Consumed*Calories.Consumed, log(Weight.gained..grams.))

cor(Calories.Consumed, log(Weight.gained..grams.))
cor(Calories.Consumed*Calories.Consumed, log(Weight.gained..grams.))

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

model12degree <- lm(log(Weight.gained..grams.) ~ Calories.Consumed + I(Calories.Consumed*Calories.Consumed))

summary(model12degree)




##############################
#  Polynomial model with 3 degree

reg3degree<-lm(log(Weight.gained..grams.)~Calories.Consumed + I(Calories.Consumed*Calories.Consumed) + I(Calories.Consumed*Calories.Consumed*Calories.Consumed))

summary(model13degree)
logpol3 <- predict(model13degree)
expy3 <- exp(logpol3)


# visualization
ggplot(data = wc_at, aes(x = Waist + I(Waist^2) + I(Waist^3), y = AT)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = wc_at, aes(x=Waist+I(Waist^2)+I(Waist^3), y=expy3))

################################
