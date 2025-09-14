fonction_cumul <- function(loi, t, ...) {
  return(loi(t, ...))
}

plot_fonction_cumul <- function(loi, intervalle, type = "l", ...) {
  # Récupérer les paramètres supplémentaires
  params <- list(...)
  
  # Calcul de la fonction de répartition
  F <- fonction_cumul(loi, intervalle, ...)
  
  # Construire le titre dynamique
  titre <- paste("Fonction de répartition de",
                 deparse(substitute(loi)),
                 if (length(params) > 0) paste0("(",
                                                paste(names(params), params, sep = "=", collapse = ", "),
                                                ")"))
  
  # Tracé
  plot(intervalle, F, type = type, col = "magenta", lwd = 3,
       main = titre,
       xlab = "t", ylab = "F(t)", ylim = c(0, 1))
}


fonction_survie <- function(loi, t, ...) {
  return(1 - fonction_cumul(loi, t, ...))
}

plot_fonction_survie <- function(loi, intervalle, type = "l", ...) {
  # Récupère les paramètres supplémentaires
  params <- list(...)
  
  # Calcule la survie
  S <- fonction_survie(loi, intervalle, ...)
  
  # Construit le titre
  titre <- paste("Fonction de survie de", 
                 deparse(substitute(loi)),
                 if (length(params) > 0) paste0("(", 
                                                paste(names(params), params, sep="=", collapse=", "), 
                                                ")"))
  
  # Trace le graphe
  plot(intervalle, S, type = type, col = "magenta", lwd = 3,
       main = titre,
       xlab = "t", ylab = "S(t)", ylim = c(0, 1))
}


# Fonction cumul d'une binomial(4,0.5)
t <- seq(0, 4, 1)
plot_fonction_cumul(pbinom, t,type="S", size = 4, prob = 0.5)
plot_fonction_survie(pbinom, t,type="S", size = 4, prob = 0.5)

# Loi exponentielle de paramètre 1
t <- seq(0, 5, 0.001)
plot_fonction_cumul(pexp, t)
plot_fonction_survie(pexp, t)

# Loi Weibull shape = alpha, scale = lambda
t <- seq(0,2,0.01)
plot_fonction_survie(pweibull,t, shape=1, scale=0.1)



# Loi normale centrée réduite
t <- seq(-3, 3, 0.01)
plot_fonction_survie(pnorm, t)

# Loi exponentielle λ=2
t <- seq(0, 20, 0.01)
plot_fonction_survie(pexp, t, rate = 1)

# Loi Weibull α=1, λ=0.1
t <- seq(0, 2, 0.01)
plot_fonction_survie(pweibull, t, shape = 1, scale = 0.1)

# Loi Weibull α=0.5, λ=0.27
t <- seq(0, 2, 0.01)
plot_fonction_survie(pweibull, t, shape = 1, scale = 0.27)

# Loi Weibull α=3, λ=0.002
t <- seq(0, 20, 0.001)
plot_fonction_survie(dweibull, t, shape = 3, scale = 0.002)

plot(t, exp(-0.002*t^3),type="l")



weib_survie <- function(t, shape = 1, scale = 1){
  return (exp(-scale*t^shape))
}
weib_répartion <- function(t, shape = 1, scale = 1){
  return (1 - weib_survie(t, shape, scale))
}

plot_fonction_survie(weib_survie, t, shape = 3, scale = 0.002)
plot_fonction_survie(weib_survie, t, shape = 1, scale = 0.1)
plot_fonction_survie(weib_survie, t, shape = 0.5, scale = 0.27)
