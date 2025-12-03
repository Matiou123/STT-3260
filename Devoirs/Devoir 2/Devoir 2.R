# Exercice 1
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
