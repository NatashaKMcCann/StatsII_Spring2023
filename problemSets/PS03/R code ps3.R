data <- gdpChange
data2 <- MexicoMuniData
#1 
install.packages("MASS")
library(MASS)
library(nnet)
library(ggplot2)
library(stargazer)
gdpChange$GDPWdiffMOD <- cut(gdpChange$GDPWdiff,
                             breaks = c(-100000, -1,1,10000),
                             labels = c("positive",
                                        "negative",
                                        "no change"))

model <- multinom(gdpChange$GDPWdiffMOD ~ ., data = data)
summary(model) # model runs for around 20 seconds then an error says that there are too many weights and im not sure how to fix this. I tried googling it and I couldnt find the answer.

# 2

model2 <- polr(gdpChange$GDPWdiffMOD ~ ., data = data, method = "logistic")
summary(model2) #I have tried running this a few times and R has crashed each time, im not sure if its an issue with my laptop or if i've done something wrong but it will not run a summary. It runs, the program freezes and then crashes. 


# Question 2 
#1 
mod.ps <- glm(data2$competitive.district ~ ., data = data2, family = poisson)
summary(mod.ps)
 
test_statistic <- (778.15 - 542.72)/ 2401
test_statistic # [1] 0.09805498

p_value <- pchisq(test_statistic * 2401,5, lower.tail = FALSE)
p_value # [1] 7.330925e-49

#2 
# Marginality 06 - coefficient is -0.05708, this means that for a one unit increase on marginality 06 there is an expected decrease in the response variable of 0.05708.
# PAN.govenor.06 - coefficient is 0.03499, this means that for a one unit increase on marginality 06 there is an expected increase in the response variable of 0.03499.

#3
input_data <- data.frame(
  competitive.district = 1,
  marginality.06 = 0,
  PAN.governor.06 = 1,
  pan.vote.09 = mean(data$pan.vote.09),
  PAN.visits.06 = mean(data$PAN.visits.06),
  MunicipCode = mean(data$MunicipCode)
)

mean_visits <- predict(mod.ps, newdata = input_data, type = "response")
mean_visits


