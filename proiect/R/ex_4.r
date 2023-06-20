source("functie_exemplu.r")
source("ex_1.r")

ex_4_mass <- function(user_function) {
  x <- seq(-10, 10, length.out = 1000)

  # Calculați media
  mean_value <- sum(x^1 * user_function(x))

  variance <- sum((x - mean_value)^2 * user_function(x))

  # Calculați momentele inițiale până la ordinul 4
  moment_1 <- sum(x^1 * user_function(x))
  moment_2 <- sum(x^2 * user_function(x))
  moment_3 <- sum(x^3 * user_function(x))
  moment_4 <- sum(x^4 * user_function(x))

  # Calculați momentele centrate până la ordinul 4
  central_moment_1 <- sum((x - mean_value)^1 * user_function(x))
  central_moment_2 <- sum((x - mean_value)^2 * user_function(x))
  central_moment_3 <- sum((x - mean_value)^3 * user_function(x))
  central_moment_4 <- sum((x - mean_value)^4 * user_function(x))

  print(paste("Media:", mean_value))
  print(paste("Dispersia:", variance))

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

ex_4_density <- function(user_function) {
  # Calculați media
  mean_value <- integrate(function(x) x * user_function(x), lower = -Inf, upper = Inf)$value

  # Calculați dispersia
  variance <- integrate(function(x) (x - mean_value)^2 * user_function(x), lower = -Inf, upper = Inf)$value

  # Calculați momentele inițiale până la ordinul 4
  moment_1 <- integrate(function(x) x^1 * user_function(x), lower = -Inf, upper = Inf)$value
  moment_2 <- integrate(function(x) x^2 * user_function(x), lower = -Inf, upper = Inf)$value
  moment_3 <- integrate(function(x) x^3 * user_function(x), lower = -Inf, upper = Inf)$value
  moment_4 <- integrate(function(x) x^4 * user_function(x), lower = -Inf, upper = Inf)$value

  # Calculați momentele centrate până la ordinul 4
  central_moment_1 <- integrate(function(x) (x - mean_value)^1 * user_function(x), lower = -Inf, upper = Inf)$value
  central_moment_2 <- integrate(function(x) (x - mean_value)^2 * user_function(x), lower = -Inf, upper = Inf)$value
  central_moment_3 <- integrate(function(x) (x - mean_value)^3 * user_function(x), lower = -Inf, upper = Inf)$value
  central_moment_4 <- integrate(function(x) (x - mean_value)^4 * user_function(x), lower = -Inf, upper = Inf)$value

  print(paste("Media:", mean_value))
  print(paste("Dispersia:", variance))

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

#' @export
ex_4 <- function(user_function) {
  fct <- density_or_mass(user_function)

  if (fct == 1) {
    ex_4_mass(user_function)
  } else if (fct == 2){
    ex_4_density(user_function)
  }
}

ex_4(mass_function)
ex_4(density_function)
