source("functie_exemplu.r")

# Funcție pentru calcularea funcției de masă marginală a variabilei X
marginal_X <- function(pmf, x) {
  sum(pmf(x, seq_along(pmf(x))))
}

# Funcție pentru calcularea funcției de masă condiționată a variabilei Y dată variabila X
conditional_Y_given_X <- function(pmf, x, y) {
  pmf(x, y) / marginal_X(pmf, x)
}

# Funcție pentru construirea funcțiilor de masă/densități marginale și condiționate
build_marginal_and_conditional <- function(pmf) {
  # Funcție de masă marginală a variabilei X
  marginal_X_values <- sapply(seq_along(pmf), function(x) marginal_X(pmf, x))

  # Funcții de masă condiționate ale variabilei Y dată variabila X
  conditional_Y_given_X_values <- outer(seq_along(pmf), seq_along(pmf), function(x, y) conditional_Y_given_X(pmf, x, y))

  # Returnează rezultatele ca o listă
  list(
    marginal_X = marginal_X_values,
    conditional_Y_given_X = conditional_Y_given_X_values
  )
}

build_marginal_and_conditional(pmf)

# Funcție pentru calcularea densității marginale a variabilei X
marginal_X_pdf <- function(pdf, x) {
  integrate(pdf, lower = -Inf, upper = Inf, y = x)$value
}

# Funcție pentru calcularea densității condiționate a variabilei Y dată variabila X
conditional_Y_given_X_pdf <- function(pdf, x, y) {
  pdf_xy <- function(y) {
    pdf(x, y)
  }
  pdf(x = NULL, y) / marginal_X_pdf(pdf_xy, x)
}

# Funcție pentru construirea funcțiilor de densitate comună, densități marginale și densități condiționate
build_joint_marginal_conditional <- function(pdf) {
  # Funcție pentru densitatea marginală a variabilei X
  marginal_X_values <- sapply(seq_along(pdf), function(x) marginal_X_pdf(pdf, x))

  # Funcții pentru densitățile condiționate ale variabilei Y dată variabila X
  conditional_Y_given_X_values <- outer(seq_along(pdf), seq_along(pdf), function(x, y) conditional_Y_given_X_pdf()(pdf, x, y))

  # Returnează rezultatele ca o listă
  list(
    marginal_X = marginal_X_values,
    conditional_Y_given_X = conditional_Y_given_X_values
  )
}

build_marginal_and_conditional(f_pmf)
build_joint_marginal_conditional(f_pdf)
