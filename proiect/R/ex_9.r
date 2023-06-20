# Seturile de valori
set_a <- c(7, 4, 2, 11, 2, 1, 2, 1, 6, 6, 0, 1, 3, 9, 7, 0, 1, 14, 0, 5, 1, 5, 2, 4, 3, 1, 0, 0, 26, 1)
set_b <- c(-1.91, -0.97, 4.59, 2.19, -0.86, -0.74, -0.60, -1.29, 0.93, 1.42, 2.14, -2.01, 2.60, 1.45, 2.60, -3.32, -3.62, 3.09, 2.91, 3.60, -0.83, -0.27, 1.82, -1.38, -1.76, 1.43, -0.59, -1.34, 2.07, 1.02)
set_c <- c(0.90, 8.91, 0.06, 1.85, 1.61, 6.50, 0.26, 0.04, 0.62, 1.01, 3.42, 1.45, 3.44, 0.46, 0.55, 0.09, 2.22, 0.65, 0.61, 6.45, 0.27, 4.81, 2.27, 0.34, 4.51, 0.42, 3.71, 2.59, 0.42, 11.18)
set_d <- c(4.83, 4.37, 5.57, 4.22, 5.96, 5.11, 5.52, 4.81, 5.19, 4.19, 4.73, 5.92, 5.63, 4.53, 4.67, 4.84, 5.25, 5.06, 5.98, 5.25, 4.60, 4.11, 4.32, 5.09, 5.25, 5.10, 4.36, 5.40, 5.33, 4.65)
set_e <- c(11, 11, 10, 10, 10, 6, 5, 9, 11, 10, 14, 8, 11, 6, 13, 9, 14, 16, 14, 10, 7, 7, 11, 12, 9, 5, 12, 15, 9, 12)

# 9.1
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
  hist(set_valori, main = "Histograma", xlab = "Valori", ylab = "Frecventa", col = "lightblue", breaks = "FD")

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

# 9.2
# install.packages("MASS")
# library(MASS)

ex_9_2 <- function(set) {
  # Ajustarea distribuției normale
  fit_norm <- MASS::fitdistr(set[set > 0], "normal")
  AIC_norm <- AIC(fit_norm)
  BIC_norm <- BIC(fit_norm)

  # Ajustarea distribuției Poisson
  fit_pois <- MASS::fitdistr(set[set > 0], "Poisson")
  AIC_pois <- AIC(fit_pois)
  BIC_pois <- BIC(fit_pois)

  # Ajustarea distribuției exponențiale
  fit_exp <- MASS::fitdistr(set[set > 0], "exponential")
  AIC_exp <- AIC(fit_exp)
  BIC_exp <- BIC(fit_exp)

  # Ajustarea distribuției gamma
  fit_gamma <- MASS::fitdistr(set[set > 0], "gamma")
  AIC_gamma <- AIC(fit_gamma)
  BIC_gamma <- BIC(fit_gamma)

  # Compararea criteriilor AIC și BIC
  criteria <- data.frame(
    Distributie = c("Normala", "Poisson", "Exponentiala", "Gamma"),
    AIC = c(AIC_norm, AIC_pois, AIC_exp, AIC_gamma),
    BIC = c(BIC_norm, BIC_pois, BIC_exp, BIC_gamma)
  )
  criteria

  # Alegerea distribuției cu cele mai mici criterii AIC și BIC
  distributie_AIC <- criteria$Distributie[which.min(criteria$AIC)]
  distributie_BIC <- criteria$Distributie[which.min(criteria$BIC)]

  print(paste("Distributia potrivita conform criteriului AIC este:", distributie_AIC))
  print(paste("Distributia potrivita conform criteriului BIC este:", distributie_BIC))
}

ex_9_2(set_a)
ex_9_2(set_b)
ex_9_2(set_c)
ex_9_2(set_d)
ex_9_2(set_e)

# 9.3

# Estimarea prin metoda verosimilității maxime
mle_a <- MASS::fitdistr(set_a, "normal")$estimate
mle_b <- MASS::fitdistr(set_b, "normal")$estimate
mle_c <- MASS::fitdistr(set_c, "normal")$estimate
mle_d <- MASS::fitdistr(set_d, "normal")$estimate
mle_e <- MASS::fitdistr(set_e, "normal")$estimate

# Estimarea prin metoda momentelor
moment_a <- c(mean(set_a), sd(set_a))
moment_b <- c(mean(set_b), sd(set_b))
moment_c <- c(mean(set_c), sd(set_c))
moment_d <- c(mean(set_d), sd(set_d))
moment_e <- c(mean(set_e), sd(set_e))

# Compararea rezultatelor
comparatie <- data.frame(
  Set = c("a_mean", "a_sd", "b_mean", "b_sd", "c_mean", "c_sd", "d_mean", "d_sd", "e_mean", "e_sd"),
  Parametri_MLE = c(mle_a, mle_b, mle_c, mle_d, mle_e),
  Parametri_Moment = c(moment_a, moment_b, moment_c, moment_d, moment_e)
)
comparatie

# 9.4

test_a <- shapiro.test(set_a)
test_b <- shapiro.test(set_b)
test_c <- shapiro.test(set_c)
test_d <- shapiro.test(set_d)
test_e <- shapiro.test(set_e)

rezultate <- data.frame(
  Set = c("a", "b", "c", "d", "e"),
  P_Value = c(test_a$p.value, test_b$p.value, test_c$p.value, test_d$p.value, test_e$p.value),
  Normala = NA
)

rezultate$Normala[rezultate$P_Value > 0.05] <- "Da"
rezultate$Normala[rezultate$P_Value <= 0.05] <- "Nu"

rezultate
