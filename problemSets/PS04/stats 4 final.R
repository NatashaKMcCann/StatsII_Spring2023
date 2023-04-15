# get the dataset 
library(survival)
install.packages("eha")
library(eha)
library(ggplot2)
install.packages("survminer")
library(survminer)
library(stargazer)
data <- child


# Cox Proportional Hazard Model 
child$time <- as.numeric(child$time)

surv_obj <- Surv(child$time, child$event)

model <- coxph(surv_obj ~ m.age + sex, data = child)

model

mod1 <- stargazer(model, title = "Survival Analysis Results", 
                  dep.var.caption = "Dependent variable", 
                  covariate.labels = c("Age", "Sex"), 
                  column.labels = c("Coefficient", "Standard Error"), 
                  type = "text")
mod1 <- stargazer(model)
mod1
# plot 
med_mage <- median(child$m.age)
new_data <- data.frame(m.age = med_mage, sex = c("male", "female"))
new_data$pred_prob <- predict(coxph_obj, newdata = new_data, type = "survival")
coxph_obj <- coxph(Surv(child$time, child$event) ~ m.age + sex, data = child)
plot_data <- survfit(as.formula(paste("Surv(child$time, child$event) ~ child$sex")), newdata = new_data)
plot_data <- data.frame(time = plot_data$time, n.event = plot_data$n.event, n.risk = plot_data$n.risk, surv_prob = plot_data$surv)









plot(model)

med_mage <- median(child$m.age)
new_data <- data.frame(m.age = med_mage, sex = child$sex)
coxph_obj <- coxph(Surv(child$time, child$event) ~ m.age + sex, data = child)

ggsurvplot(survfit(coxph_obj, newdata = new_data), data = new_data,
           pval = TRUE, conf.int = TRUE, risk.table = TRUE, 
           legend.title = "Infant Gender", xlab = "Time (days)", 
           ylab = "Survival Probability", 
           ggtheme = theme_bw())
