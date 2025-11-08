library( survival)
shunt.data = read.table( "shunt.txt", header=T)
shunt.fit= survfit( Surv( time= shunt.data$time, event=shunt.data$event ) ~ shunt.data$age, data=
                      shunt.data,  error="greenwood", conf.type="log", conf.int=0.95 )
plot( shunt.fit, lty= 2:3, lwd=3, xlab="temps de survie (court−circuit)", ylab="fonction de survi
 e", cex=2, cex.axis=1.5, cex.lab=2  )


estimation_risque_cumule_NA <- function(data, groupe = 0) {
  data_type <- data[data$age == groupe, ]
  data_delta <- data_type[data_type$event == 1, ]
  
  temps_uniques <- unique(data_delta$time)
  
  i <- 1
  cumul <- 0
  
  #######################Sortie##########################
  risque_cumul <- numeric(length = length(temps_uniques))  
  nb_à_risque <- numeric(length = length(temps_uniques))  
  nb_de_décès <- numeric(length = length(temps_uniques))   
  ratio <- numeric(length = length(temps_uniques))         
  survie_est <- numeric(length = length(temps_uniques))    
  écart_type <- numeric(length = length(temps_uniques))    
  #######################Sortie##########################
  
  while (i < length(temps_uniques) + 1) {                 
    t_i <- temps_uniques[i]                                
    d_i <- nrow(data_delta[data_delta$time == t_i, ])
    nb_de_décès[i] <- d_i
    y_i <- nrow(data_type[data_type$time >= t_i, ])
    nb_à_risque[i] <- y_i
    ratio[i] <- d_i / y_i
    cumul <- cumul + d_i / y_i
    risque_cumul[i] <- cumul 
    survie_est[i] <- exp(-risque_cumul[i])
    écart_type[i] <- sqrt(survie_est[i]^2 * sum(ratio[1:i] / nb_à_risque[1:i]))
    
    i <- i + 1
  }
  
  temps <- temps_uniques                                   
  return(data.frame(temps, nb_à_risque, nb_de_décès, ratio, 
                    risque_cumul, survie_est, écart_type))
}


risque_cumul <- estimation_risque_cumule_NA(shunt.data, groupe=1)

