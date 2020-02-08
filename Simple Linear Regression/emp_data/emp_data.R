# Load wc_at.csv dataset
library(readr)
emp_data <- read_csv("C:/Training/ExcelR/Simple Linear Regression/emp_data.csv")
View(emp_data)

# Exploratory data analysis
summary(emp_data)

attach(emp_data)

#Scatter plot
plot(Salary_hike, Churn_out_rate)  # plot(X,Y)



#Correlation Coefficient (r)
cor(Salary_hike, Churn_out_rate)             # cor(X,Y)

# Simple Linear Regression model
reg <- lm(Churn_out_rate ~ Salary_hike) # lm(Y ~ X)

summary(reg)


####################

# Logrithamic Model

# x = log(Salary_hike); y = Churn_out_rate

plot(log(Salary_hike), Churn_out_rate)
cor(log(Salary_hike), Churn_out_rate)

reg_log <- lm(Churn_out_rate ~ log(Salary_hike))   # lm(Y ~ X)

summary(reg_log)


######################

# Exponential Model

# x = Churn_out_rate and y = log(Salary_hike)

plot(Salary_hike, log(Churn_out_rate))

cor(Salary_hike, log(Churn_out_rate))

reg_exp <- lm(log(Churn_out_rate) ~ Salary_hike)  #lm(log(Churn_out_rate) ~ Salary_hike)

summary(reg_exp)



##############################
# Polynomial model with 2 degree (quadratic model)

plot(Salary_hike, Churn_out_rate)
plot(Salary_hike*Salary_hike, Churn_out_rate)

cor(Salary_hike*Salary_hike, Churn_out_rate)

plot(Salary_hike*Salary_hike, log(Churn_out_rate))

cor(Salary_hike, log(Churn_out_rate))
cor(Salary_hike*Salary_hike, log(Churn_out_rate))

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg2degree <- lm(log(Churn_out_rate) ~ Salary_hike+ I(Salary_hike*Salary_hike))

summary(reg2degree)




##############################
#  Polynomial model with 3 degree

reg3degree<-lm(log(Churn_out_rate)~Salary_hike+ I(Salary_hike*Salary_hike) + I(Salary_hike*Salary_hike*Salary_hike))

summary(reg3degree)
logpol3 <- predict(reg3degree)
expy3 <- exp(logpol3)


################################
