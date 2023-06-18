density_function <- function(x) {
  # Exemplu: funcție de densitate de probabilitate pentru o variabilă aleatoare continuă
  dnorm(x, mean = 0, sd = 1)
}

mass_function <- function(x) {
  # Exemplu: funcție de masă de probabilitate pentru o variabilă aleatoare discretă
  ifelse(x %in% c(1, 2, 3), 1 / 3, 0)
}