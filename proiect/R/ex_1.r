source("functie_exemplu.r")

density_or_mass <- function(user_function) {
  # Verificați dacă funcția este o funcție de masă de probabilitate sau o funcție de densitate de probabilitate

  # Verificați dacă funcția are valori non-negative
  has_nonnegative_values <- all(user_function(1:10) >= 0)

  # Verificați suma (pentru funcții de masă de probabilitate) sau integrala (pentru funcții de densitate de probabilitate)

  sum <- sum(user_function(1:10)) # pentru funcție de masă de probabilitate (suma)
  # sau
  integral <- integrate(user_function, lower = -Inf, upper = Inf)$value # pentru funcție de densitate de probabilitate (integrala)

  # Verificați dacă suma sau integrala este aproape de 1, utilizând o toleranță
  is_mass_function <- abs(sum - 1) < 0.0001
  # sau
  is_density_function <- abs(integral - 1) < 0.0001

  # Afișați rezultatul
  if (has_nonnegative_values && is_mass_function) {
    return(1)
  } else if (has_nonnegative_values && is_density_function) {
    return(2)
  } else {
    return(0)
  }
}

print_density_or_mass <- function(user_function) {
  result <- density_or_mass(user_function)
  if (result == 1) {
    print("Funcția introdusă de utilizator este o funcție de masă de probabilitate.")
  } else if (result == 2) {
    print("Funcția introdusă de utilizator este o funcție de densitate de probabilitate.")
  } else {
    print("Funcția introdusă de utilizator nu este o funcție de masă de probabilitate și nici o funcție de densitate de probabilitate.")
  }
}

print_density_or_mass(mass_function)
print_density_or_mass(density_function)
