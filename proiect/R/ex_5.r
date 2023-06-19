# Definirea funcției g(x) specificată de utilizator
g <- function(x) {
  # Definiți aici funcția g(x)
  # Exemplu: g(x) = x^2
  return(x^2)
}

# Definirea funcției de densitate de probabilitate a lui X
densitate_x <- function(x) {
  # Definiți aici funcția de densitate de probabilitate a lui X
  # Exemplu: densitatea de probabilitate a unei variabile aleatoare normale standard
  return(dnorm(x, mean = 0, sd = 1))
}

ex_5 <- function(g, densitate_x) {
  # Calcularea mediei variabilei aleatoare g(X)
  mean_gx <- integrate(function(x) g(x) * densitate_x(x), lower = -Inf, upper = Inf)$value

  # Calcularea dispersiei variabilei aleatoare g(X)
  var_gx <- integrate(function(x) (g(x) - mean_gx)^2 * densitate_x(x), lower = -Inf, upper = Inf)$value

  # Afișarea rezultatelor
  print(paste("Media variabilei aleatoare g(X):", mean_gx))
  print(paste("Dispersia variabilei aleatoare g(X):", var_gx))
}
