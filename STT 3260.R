fonction_cumul <- function(loi, t) {
  return(loi(t))
}

plot_fonction_cumul <- function(loi, intervalle,type="l") {
  F <- fonction_cumul(loi, intervalle)
  
  plot(intervalle, F, type = type, col = "magenta", lwd = 3,
       main = paste("Fonction de répartition de", deparse(substitute(loi))),
       xlab = "t", ylab = "F(t)", ylim=c(0,1))
}

fonction_survie <- function(loi, t){
  return (1 - fonction_cumul(loi,t))
}

plot_fonction_cumul <- function(loi, intervalle,type="l") {
  S <- fonction_survie(loi, intervalle)
  
  plot(intervalle, S, type = type, col = "magenta", lwd = 3,
       main = paste("Fonction de survie de", deparse(substitute(loi))),
       xlab = "t", ylab = "S(t)", ylim=c(0,1))
}


# Loi normale centrée réduite
t <- seq(0, 4, 1)
plot_fonction_cumul(function(x) pbinom(x, size = 4, prob = 0.5), t,type="S")

# Loi exponentielle de paramètre 1
t <- seq(0, 5, 0.1)
plot_fonction_cumul(pexp, t)

