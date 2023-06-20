
# Functie pentru calculul probabilitatii marginale sau conditionate
P <- function(X, condition = NULL) {
  if (is.null(condition)) {
    # Calculul probabilității marginale
    return(sum(X) / length(X))
  } else {
    # Calculul probabilității condiționate
    return(sum(X[condition]) / sum(condition))
  }
}

# Calculul probabilității marginale pentru o variabilă aleatoare discretă

# Definirea variabilei aleatoare X (ex. o variabilă binomială)
X <- rbinom(1000, size = 10, prob = 0.5)

# Calculul probabilității marginale pentru variabila X
prob_marginal <- P(X)
print(prob_marginal)

# Calculul probabilității marginale pentru o variabilă aleatoare continuă

# Definirea variabilei aleatoare X (ex. o variabilă normală)
X <- rnorm(1000, mean = 0, sd = 1)

# Calculul probabilității marginale pentru variabila X
prob_marginal <- P(X)
print(prob_marginal)

# Calculul probabilității condiționate pentru o variabilă aleatoare discretă

# Definirea variabilei aleatoare X (ex. o variabilă Poisson)
X <- rpois(1000, lambda = 3)

# Calculul probabilității condiționate pentru variabila X, dat fiind X >= 2
prob_conditioned <- P(X, condition = X >= 2)
print(prob_conditioned)

# Calculul probabilității condiționate pentru o variabilă aleatoare continuă

# Definirea variabilei aleatoare X (ex. o variabilă exponențială)
X <- rexp(1000, rate = 0.5)

# Calculul probabilității condiționate pentru variabila X, dat fiind X > 2
prob_conditioned <- P(X, condition = X > 2)
print(prob_conditioned)

