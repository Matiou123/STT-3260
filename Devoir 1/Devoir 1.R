# Survie lognormale
lognormale_survie <- function(t, mu, sigma) {
  return (1 - pnorm( (log(t) - mu)/sigma ))
}


# Densité lognormale
lognormale_densite <- function(t, mu , sigma) {
  return (dnorm( (log(t) - mu)/sigma ) /(t*sigma))
}

# Risque lognormale
lognormale_risque <- function(t, mu, sigma) {
  return(lognormale_densite(t,mu,sigma)/ lognormale_survie(t,mu,sigma))
}

# Temps jusqu'à 300 jours
t <- seq(0.00001,300, 0.1)

mu = 3.177
sigma = 2.084

plot(t, lognormale_densite(t,mu,sigma), type="l", lwd=2, col="magenta", ylab="Taux de risque",
     main="Taux de risque de décès suite à une greffe", xlab="Temps en jours")

