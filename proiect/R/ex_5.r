# Definirea funcției g(x) specificată de utilizator
g <- function(x) {
  return(x^2)
}

# Definirea funcției de densitate de probabilitate a lui X
densitate_x <- function(x) {
  return(dnorm(x, mean = 0, sd = 1))
}

# Funtie pentru calcularea mediei si dispersiei variabilei aleatoare g(X)qq
ex_5 <- function(g, densitate_x) {
  # Calcularea mediei variabilei aleatoare g(X)
  mean_gx <- integrate(function(x) g(x) * densitate_x(x), lower = -Inf, upper = Inf)$value

  # Calcularea dispersiei variabilei aleatoare g(X)
  var_gx <- integrate(function(x) (g(x) - mean_gx)^2 * densitate_x(x), lower = -Inf, upper = Inf)$value

  # Afișarea rezultatelor
  print(paste("Media variabilei aleatoare g(X):", mean_gx))
  print(paste("Dispersia variabilei aleatoare g(X):", var_gx))
}

ex_5(g, densitate_x)
