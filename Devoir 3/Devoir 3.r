data <- read.table("~/cheminVers/kidney.txt", header=TRUE)

library(survival)

groupe_0_weib <- survreg(Surv(time, delta) ~ 1, data = data[data$type == 2,], dist = "weibull")
summary(groupe_0_weib)

groupe_1_weib <- survreg(Surv(time, delta) ~ 1, data = data[data$type == 1,], dist = "weibull")
summary(groupe_1_weib)


groupe_0_log_logistique <- survreg(Surv(time, delta) ~ 1, data = data[data$type == 2,], dist = "loglogistic")
summary(groupe_0_log_logistique)

groupe_1_log_logistique <- survreg(Surv(time, delta) ~ 1, data = data[data$type == 1,], dist = "loglogistic")
summary(groupe_1_log_logistique)
