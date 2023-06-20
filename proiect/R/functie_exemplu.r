# Funcție de densitate de probabilitate pentru o variabilă aleatoare continuă
density_function <- function(x) {
  dnorm(x, mean = 0, sd = 1)
}

# Funcție de masă de probabilitate pentru o variabilă aleatoare discretă
mass_function <- function(x) {
  ifelse(x %in% c(1, 2, 3), 1 / 3, 0)
}

# Functie de masa comuna pentru doua variabile aleatoare discrete
f_pmf <- function(x, y = NULL) {
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
f_pdf <- function(x = NULL, y = NULL) {
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


set_a <- c(7, 4, 2, 11, 2, 1, 2, 1, 6, 6, 0, 1, 3, 9, 7, 0, 1, 14, 0, 5, 1, 5, 2, 4, 3, 1, 0, 0, 26, 1)
set_b <- c(-1.91, -0.97, 4.59, 2.19, -0.86, -0.74, -0.60, -1.29, 0.93, 1.42, 2.14, -2.01, 2.60, 1.45, 2.60, -3.32, -3.62, 3.09, 2.91, 3.60, -0.83, -0.27, 1.82, -1.38, -1.76, 1.43, -0.59, -1.34, 2.07, 1.02)
set_c <- c(0.90, 8.91, 0.06, 1.85, 1.61, 6.50, 0.26, 0.04, 0.62, 1.01, 3.42, 1.45, 3.44, 0.46, 0.55, 0.09, 2.22, 0.65, 0.61, 6.45, 0.27, 4.81, 2.27, 0.34, 4.51, 0.42, 3.71, 2.59, 0.42, 11.18)
set_d <- c(4.83, 4.37, 5.57, 4.22, 5.96, 5.11, 5.52, 4.81, 5.19, 4.19, 4.73, 5.92, 5.63, 4.53, 4.67, 4.84, 5.25, 5.06, 5.98, 5.25, 4.60, 4.11, 4.32, 5.09, 5.25, 5.10, 4.36, 5.40, 5.33, 4.65)
set_e <- c(11, 11, 10, 10, 10, 6, 5, 9, 11, 10, 14, 8, 11, 6, 13, 9, 14, 16, 14, 10, 7, 7, 11, 12, 9, 5, 12, 15, 9, 12)
