setwd("C:\\Users\\robst\\Desktop\\Robert\\Programming\\VSCode\\Fac\\An 2\\Sem2\\PS\\proiect\\R")
source("functie_exemplu.r")

ex_4 <- function(user_function) {
  # Calculați media
  mean_value <- integrate(function(x) x * user_function(x), lower = -Inf, upper = Inf)$value

  # Calculați dispersia
  variance <- integrate(function(x) (x - mean_value)^2 * user_function(x), lower = -Inf, upper = Inf)$value

  # Calculați momentele inițiale până la ordinul 4
  moment_1 <- integrate(function(x) (x - mean_value)^1 * user_function(x), lower = -Inf, upper = Inf)$value
  moment_2 <- integrate(function(x) (x - mean_value)^2 * user_function(x), lower = -Inf, upper = Inf)$value
  moment_3 <- integrate(function(x) (x - mean_value)^3 * user_function(x), lower = -Inf, upper = Inf)$value
  moment_4 <- integrate(function(x) (x - mean_value)^4 * user_function(x), lower = -Inf, upper = Inf)$value

  # Calculați momentele centrate până la ordinul 4
  central_moment_1 <- moment_1
  central_moment_2 <- moment_2
  central_moment_3 <- moment_3
  central_moment_4 <- moment_4 - 3 * variance^2

  # Afișați rezultatele
  if (!is.finite(moment_1)) {
    print("Momentul inițial de ordinul 1 nu există.")
  } else {
    print(paste("Momentul inițial de ordinul 1:", moment_1))
  }

  if (!is.finite(moment_2)) {
    print("Momentul inițial de ordinul 2 nu există.")
  } else {
    print(paste("Momentul inițial de ordinul 2:", moment_2))
  }

  if (!is.finite(moment_3)) {
    print("Momentul inițial de ordinul 3 nu există.")
  } else {
    print(paste("Momentul inițial de ordinul 3:", moment_3))
  }

  if (!is.finite(moment_4)) {
    print("Momentul inițial de ordinul 4 nu există.")
  } else {
    print(paste("Momentul inițial de ordinul 4:", moment_4))
  }

  if (!is.finite(central_moment_1)) {
    print("Momentul centrat de ordinul 1 nu există.")
  } else {
    print(paste("Momentul centrat de ordinul 1:", central_moment_1))
  }

  if (!is.finite(central_moment_2)) {
    print("Momentul centrat de ordinul 2 nu există.")
  } else {
    print(paste("Momentul centrat de ordinul 2:", central_moment_2))
  }

  if (!is.finite(central_moment_3)) {
    print("Momentul centrat de ordinul 3 nu există.")
  } else {
    print(paste("Momentul centrat de ordinul 3:", central_moment_3))
  }

  if (!is.finite(central_moment_4)) {
    print("Momentul centrat de ordinul 4 nu există.")
  } else {
    print(paste("Momentul centrat de ordinul 4:", central_moment_4))
  }
}


ex_4(density_function)
