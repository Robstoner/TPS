source("functie_exemplu.r")


normalization_function <- function(user_function) {
  # Calculați constanta de normalizare k
  normalization_constant <- integrate(user_function, lower = -Inf, upper = Inf)$value

  tolerance <- 0.0001
  # Verificați dacă constanta de normalizare există și determinați dacă funcția este o funcție de masă sau o densitate de probabilitate
  if (is.finite(normalization_constant)) {
    if ((1 / normalization_constant - 1) < tolerance) {
      print("Funcția introdusă de utilizator este o functie de densitate de probabilitate.")
    } else {
      print("Funcția introdusă de utilizator este o funcție de masă de probabilitate.")
    }
    print(paste("Constanta de normalizare k =", 1 / normalization_constant))
  } else {
    print("Nu există o constantă de normalizare pentru funcția introdusă de utilizator.")
  }
}

normalization_function(mass_function)
