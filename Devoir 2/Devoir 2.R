# Exercice 1
# a)
data <- data.frame(
  c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21),
  c(1, 22, 3, 12, 8, 17, 2, 11, 8, 12, 2, 5, 4, 15, 8, 23, 5, 11, 4, 1, 9)
)
colnames(data) <- c("Temps", "Nombre de rechutes")

n <- sum(data$`Nombre de rechutes`)
EVM_mu_lognormale <- sum(log(data$Temps) * data$`Nombre de rechutes`) / n

EVM_sigma_lognormale <- sqrt(sum( ((log(data$Temps) - EVM_mu_lognormale)^2) 
                                  * data$`Nombre de rechutes`)/n)

# Fonction à résoudre 
fonction_beta <- function(beta) {
  log(n * beta / sum(data$Temps * data$`Nombre de rechutes`)) + 
    sum(log(data$Temps)* data$`Nombre de rechutes`)/n - 
    digamma(beta)
}

EVM_beta_gamma <- uniroot(fonction_beta,
                          interval = c(0.001, 100))$root
EVM_lambda_gamma <- n * EVM_beta_gamma / (sum(data$Temps * data$`Nombre de rechutes`))

# b)
library(flexsurv)
d <- read.table("CheminVers/donneesRechute.txt", header=TRUE)
modele_gengamma <- flexsurvreg(Surv(Temps) ~ 1,data= d ,dist = "gengamma")
modele_gengamma$loglik
modele_gengamma$coefficients


# c)
fonction_repartion_empirique <- function(t, data){
  return( sum((data <= t)) / length(data) )
}
fonction_survie_empirique <- function(t, data){
  return(1-  fonction_repartion_empirique(t, data) )
}

t <- seq(0, max(d$Temps) , 1)
s <- numeric(max(d$Temps) + 1)
for (i in 1 :max(d$Temps)){
  s[i] <- fonction_survie_empirique(i - 1, d$Temps)
}
rm(i)
plot(t, s, pch=19, col = "blue", main="Fonction de survie empirique", 
     ylab = "Fonction de survie empirique", 
     xlab="Temps", type="l", lwd=2)

# d)

plot(t, s, pch=19, col = "blue", main="Fonctions de survie pour les données", 
     ylab = "Fonction de survie", 
     xlab="Temps", type="l", lwd=2)

# Log-normale
survie_log_norm <- 1 - pnorm((log(t) - EVM_mu_lognormale) / EVM_sigma_lognormale)
lines(t, survie_log_norm, lwd =2, col="red" , lty=2)

# Gamma
survie_gamma <- 1 - pgamma(t, shape = EVM_beta_gamma, rate = EVM_lambda_gamma)
lines(t, survie_gamma, col="magenta", lty=3, lwd = 2)

legend("topright",
       legend=c("Empirique", "Log-normale", "Gamma"),
       col=c("blue","red","magenta"), lty=c(1,2,3), lwd=2)

# e)

plot(t, s, pch=19, col = "blue", main="Fonctions de survie pour les données", 
     ylab = "Fonction de survie", 
     xlab="Temps", type="l", lwd=2)

# Normale
lines(t, survie_log_norm, lwd =2, col="red" , lty=2)

# Gamma
lines(t, survie_gamma, col="magenta", lty=3, lwd = 2)

# Gamma-généralisé
#        mu     sigma         Q 
#    2.850828 -1.384425  3.437482

survie_gengamma <- 1 - pgengamma(t, mu = 2.850828, sigma = exp(-1.384425), Q = 3.437482)
lines(t, survie_gengamma, col="black", lty= 4, lwd=2)

legend("topright",
       legend=c("Empirique", "Log-normale", "Gamma","Gamma-généralisé"),
       col=c("blue","red","magenta","black"), lty=c(1,2,3,4), lwd=2)

# f)

modele_log_normale <- flexsurvreg(Surv(Temps) ~ 1,data= d ,dist = "lnorm")
modele_log_normale$loglik

stat_vraisemblance <- -2*(modele_log_normale$loglik - modele_gengamma$loglik)

qchisq(0.99,1)
stat_vraisemblance > qchisq(0.99,1)
