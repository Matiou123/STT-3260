# a
données <- read.table("tongue.txt", header = T)
Z <- données$type == 1
données["Z"] <- Z

library(survival)
modèle = coxph(Surv(time,delta)~Z, ties = "breslow", data=données)

summary_1 <- summary(modèle)
summary_1

beta_chap <- summary_1$coefficients["ZTRUE", "coef"]
beta_chap
SE_beta  <- summary_1$coefficients["ZTRUE", "se(coef)"]
SE_beta

IC_beta <- exp(beta_chap + c(-1, 1) * 1.96 * SE_beta)
IC_beta

