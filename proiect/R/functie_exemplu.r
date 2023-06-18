# Funcție de densitate de probabilitate pentru o variabilă aleatoare continuă
density_function <- function(x) {
  dnorm(x, mean = 0, sd = 1)
}

# Funcție de masă de probabilitate pentru o variabilă aleatoare discretă
mass_function <- function(x) {
  ifelse(x %in% c(1, 2, 3), 1 / 3, 0)
}

# Functie de masa comuna pentru doua variabile aleatoare discrete
pmf <- function(x, y = NULL) {
  if (identical(x, 1) && identical(y, 1)) {
    return(0.2)
  } else if (identical(x, 1) && identical(y, 2)) {
    return(0.1)
  } else if (identical(x, 2) && identical(y, 1)) {
    return(0.3)
  } else if (identical(x, 2) && identical(y, 2)) {
    return(0.4)
  } else {
    return(0)
  }
}

# Functie de densitate comuna pentru doua variabile aleatoare discrete
pdf <- function(x = NULL, y = NULL) {
  # Verificăm dacă argumentul y este furnizat
  if (missing(y)) {
    if (x >= 0 && x <= 1) {
      return(1)
    } else {
      return(0)
    }
  } else {
    if (missing(x)) {
      if (y >= 0 && y <= 1) {
        return(1)
      } else {
        return(0)
      }
    } else {
      ifelse(x >= 0 & x <= 1 & y >= 0 & y <= 1, 1, 0)
    }
  }
}
