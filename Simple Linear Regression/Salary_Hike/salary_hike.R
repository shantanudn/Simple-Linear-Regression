# Load salary_hike.csv dataset
library(readr)
salary_data <- read_csv("C:/Training/ExcelR/Simple Linear Regression/Salary_Data.csv")
View(salary_data)

# Exploratory data analysis
summary(salary_data)

attach(salary_data)

#Scatter plot
plot(YearsExperience, Salary)  # plot(X,Y)



#Correlation Coefficient (r)
cor(YearsExperience, Salary)             # cor(X,Y)

# Simple Linear Regression model
reg <- lm(Salary ~ YearsExperience) # lm(Y ~ X)

summary(reg)


####################

# Logrithamic Model

# x = log(YearsExperience); y = Salary

plot(log(YearsExperience), Salary)
cor(log(YearsExperience), Salary)

reg_log <- lm(Salary ~ log(YearsExperience))   # lm(Y ~ X)

summary(reg_log)


######################

# Exponential Model

# x = Salary and y = log(YearsExperience)

plot(YearsExperience, log(Salary))

cor(YearsExperience, log(Salary))

reg_exp <- lm(log(Salary) ~ YearsExperience)  #lm(log(Salary) ~ YearsExperience)

summary(reg_exp)



##############################
# Polynomial model with 2 degree (quadratic model)

plot(YearsExperience, Salary)
plot(YearsExperience*YearsExperience, Salary)

cor(YearsExperience*YearsExperience, Salary)

plot(YearsExperience*YearsExperience, log(Salary))

cor(YearsExperience, log(Salary))
cor(YearsExperience*YearsExperience, log(Salary))

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg2degree <- lm(log(Salary) ~ YearsExperience+ I(YearsExperience*YearsExperience))

summary(reg2degree)




##############################
#  Polynomial model with 3 degree

reg3degree<-lm(log(Salary)~YearsExperience+ I(YearsExperience*YearsExperience) + I(YearsExperience*YearsExperience*YearsExperience))

summary(reg3degree)
logpol3 <- predict(reg3degree)
expy3 <- exp(logpol3)


################################
