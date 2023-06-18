# Seturile de valori
set_a <- c(7, 4, 2, 11, 2, 1, 2, 1, 6, 6, 0, 1, 3, 9, 7, 0, 1, 14, 0, 5, 1, 5, 2, 4, 3, 1, 0, 0, 26, 1)
set_b <- c(-1.91, -0.97, 4.59, 2.19, -0.86, -0.74, -0.60, -1.29, 0.93, 1.42, 2.14, -2.01, 2.60, 1.45, 2.60, -3.32, -3.62, 3.09, 2.91, 3.60, -0.83, -0.27, 1.82, -1.38, -1.76, 1.43, -0.59, -1.34, 2.07, 1.02)
set_c <- c(0.90, 8.91, 0.06, 1.85, 1.61, 6.50, 0.26, 0.04, 0.62, 1.01, 3.42, 1.45, 3.44, 0.46, 0.55, 0.09, 2.22, 0.65, 0.61, 6.45, 0.27, 4.81, 2.27, 0.34, 4.51, 0.42, 3.71, 2.59, 0.42, 11.18)
set_d <- c(4.83, 4.37, 5.57, 4.22, 5.96, 5.11, 5.52, 4.81, 5.19, 4.19, 4.73, 5.92, 5.63, 4.53, 4.67, 4.84, 5.25, 5.06, 5.98, 5.25, 4.60, 4.11, 4.32, 5.09, 5.25, 5.10, 4.36, 5.40, 5.33, 4.65)
set_e <- c(11, 11, 10, 10, 10, 6, 5, 9, 11, 10, 14, 8, 11, 6, 13, 9, 14, 16, 14, 10, 7, 7, 11, 12, 9, 5, 12, 15, 9, 12)

#9.1
# Funția pentru afișarea histogramelor și valorilor
afisare_histograma_si_statistici <- function(set_valori) {
  # Calculați mediana, media și deviația standard
  mediana <- median(set_valori)
  media <- mean(set_valori)
  deviatia_standard <- sd(set_valori)
  
  # Afișați valorile calculate
  cat("Mediana:", mediana, "\n")
  cat("Media:", media, "\n")
  cat("Deviația standard:", deviatia_standard, "\n")
  
  # Afișați histograma
  hist(set_valori, main = "Histograma", xlab = "Valori", ylab = "Frecvență", col = "lightblue")
  
  # Adăugați linii verticale pentru mediana, media și deviația standard
  abline(v = mediana, col = "red", lwd = 2)
  abline(v = media, col = "blue", lwd = 2)
  abline(v = c(media - deviatia_standard, media + deviatia_standard), col = "green", lwd = 2, lty = 2)
}

# Afișați histograma și valorile statistice pentru fiecare set de valori
afisare_histograma_si_statistici(set_a)
afisare_histograma_si_statistici(set_b)
afisare_histograma_si_statistici(set_c)
afisare_histograma_si_statistici(set_d)
afisare_histograma_si_statistici(set_e)

#9.2
# Identificarea posibilelor repartiții pentru setul de valori a)
mean_a <- mean(set_a)
sd_a <- sd(set_a)
median_a <- median(set_a)

# Justificarea alegerii: 
# - Deoarece valorile din setul a) pot fi considerate discrete și nu există valori negative, putem verifica dacă setul se potrivește cu o repartiție Poisson sau binomială.
# - Având în vedere că valoarea medie și deviația standard sunt relativ apropiate, acest lucru sugerează că setul a) poate fi aproximativ distribuit într-o manieră Poisson sau binomială.
# - Mediana poate oferi și o indicație, deoarece într-o repartiție Poisson mediana și media sunt aproximativ egale.

# Compararea cu repartițiile posibile
poisson_lambda <- mean_a
binomial_n <- round(mean_a / (sd_a^2))
binomial_p <- mean_a / binomial_n

# Testarea repartițiilor Poisson și binomial
poisson_test <- chisq.test(table(round(set_a)), p = dpois(0:max(set_a), lambda = poisson_lambda))
binomial_test <- chisq.test(table(set_a), p = dbinom(0:max(set_a), size = binomial_n, prob = binomial_p))

if (poisson_test$p.value > binomial_test$p.value) {
  cat("Setul de valori a) pare să fie generat dintr-o repartiție Poisson cu parametrul lambda =", poisson_lambda)
} else {
  cat("Setul de valori a) pare să fie generat dintr-o repartiție binomială cu parametrii n =", binomial_n, "și probabilitatea de succes p =", binomial_p)
}