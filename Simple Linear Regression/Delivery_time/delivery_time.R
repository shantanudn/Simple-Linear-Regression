# Load delivery_time.csv dataset
library(readr)
delivery_time <- read_csv("C:/Training/ExcelR/Simple Linear Regression/delivery_time.csv")
View(delivery_time)

# Exploratory data analysis
summary(delivery_time)

attach(delivery_time)

#Scatter plot
plot(sorting_time,delivery_time1)  # plot(X,Y)



#Correlation Coefficient (r)
cor(sorting_time,delivery_time1)             # cor(X,Y)

# Simple Linear Regression model
reg <- lm(delivery_time1 ~ sorting_time) # lm(Y ~ X)

summary(reg)


####################

# Logrithamic Model

# x = log(sorting_time); y = delivery_time1

plot(log(sorting_time), delivery_time1)
cor(log(sorting_time), delivery_time1)

reg_log <- lm(delivery_time1 ~ log(sorting_time))   # lm(Y ~ X)

summary(reg_log)


######################

# Exponential Model

# x = sorting_time and y = log(delivery_time1)

plot(sorting_time, log(delivery_time1))

cor(sorting_time, log(delivery_time1))

reg_exp <- lm(log(delivery_time1) ~ sorting_time)  

summary(reg_exp)



##############################
# Polynomial model with 2 degree (quadratic model)

plot(sorting_time, delivery_time1)
plot(sorting_time*sorting_time, delivery_time1)

cor(sorting_time*sorting_time, delivery_time1)

plot(sorting_time*sorting_time, log(delivery_time1))

cor(sorting_time, log(delivery_time1))
cor(sorting_time*sorting_time, log(delivery_time1))

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg2degree <- lm(log(delivery_time1) ~ sorting_time+ I(sorting_time*sorting_time))

summary(reg2degree)




##############################
#  Polynomial model with 3 degree

reg3degree<-lm(log(delivery_time1)~sorting_time+ I(sorting_time*sorting_time) + I(sorting_time*sorting_time*sorting_time))

summary(reg3degree)
logpol3 <- predict(reg3degree)
expy3 <- exp(logpol3)


################################
