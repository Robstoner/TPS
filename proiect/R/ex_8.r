# Definirea funcției de masă comună (pentru variabile discrete)
joint <- function(x, y) {
  # Definirea tabelei de probabilități conjuncte (poți ajusta valorile în funcție de situația ta)
  probabilities <- matrix(c(0.1, 0.2, 0.15, 0.25, 0.05, 0.1), nrow = 2, byrow = TRUE)

  # Obținerea valorilor unice ale variabilelor X și Y
  x_values <- unique(x)
  y_values <- unique(y)

  # Crearea unui data frame pentru asocierea valorilor variabilelor X și Y cu probabilitățile corespunzătoare
  data <- expand.grid(X = x_values, Y = y_values)
  data$Prob <- as.vector(probabilities)

  # Returnarea probabilității corespunzătoare combinației X și Y
  data$Prob[match(data$X, x) & match(data$Y, y)]
}

# Definirea valorilor aleatoare pentru variabilele X și Y
x_values <- c(1, 2, 3) # Valorile posibile ale variabilei X
y_values <- c(1, 2, 3) # Valorile posibile ale variabilei Y

# Funcția de masă marginală pentru variabila X
marginal_X <- function(x) {
  joint_probs <- joint(x_values, y_values)
  marginal_probs <- rowSums(joint_probs)
  marginal_probs[match(x, x_values)]
}

# Funcția de masă marginală pentru variabila Y
marginal_Y <- function(y) {
  joint_probs <- joint(x_values, y_values)
  marginal_probs <- colSums(joint_probs)
  marginal_probs[match(y, y_values)]
}

# Funcția de masă condiționată pentru variabila X dat fiind Y
conditional_X_given_Y <- function(x, y) {
  joint_probs <- joint(x_values, y_values)
  marginal_probs_Y <- marginal_Y[y]
  conditional_probs <- joint_probs[, y] / marginal_probs_Y
  conditional_probs[match(x, x_values)]
}

# Funcția de masă condiționată pentru variabila Y dat fiind X
conditional_Y_given_X <- function(y, x) {
  joint_probs <- joint(x_values, y_values)
  marginal_probs_X <- marginal_X[x]
  conditional_probs <- joint_probs[x, ] / marginal_probs_X
  conditional_probs[match(y, y_values)]
}

prob_conjuncta <- joint(1, 2) # Probabilitatea conjunctă pentru X = 1 și Y = 2


# Exemplu de calcul al probabilității condiționate P(X=1 | Y=2)
prob_conditional_X_given_Y <- conditional_X_given_Y(1, 2)
print(prob_conditional_X_given_Y)

# Exemplu de calcul al probabilității condiționate P(Y=2 | X=1)
prob_conditional_Y_given_X <- conditional_Y_given_X(2, 1)
print(prob_conditional_Y_given_X)
