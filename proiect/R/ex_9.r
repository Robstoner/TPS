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

# 9.2
# install.packages("fitdistrplus")
# library(fitdistrplus)

# Seturile de valori
set_a <- c(7, 4, 2, 11, 2, 1, 2, 1, 6, 6, 0, 1, 3, 9, 7, 0, 1, 14, 0, 5, 1, 5, 2, 4, 3, 1, 0, 0, 26, 1)
set_b <- c(-1.91, -0.97, 4.59, 2.19, -0.86, -0.74, -0.60, -1.29, 0.93, 1.42, 2.14, -2.01, 2.60, 1.45, 2.60, -3.32, -3.62, 3.09, 2.91, 3.60, -0.83, -0.27, 1.82, -1.38, -1.76, 1.43, -0.59, -1.34, 2.07, 1.02)
set_c <- c(0.90, 8.91, 0.06, 1.85, 1.61, 6.50, 0.26, 0.04, 0.62, 1.01, 3.42, 1.45, 3.44, 0.46, 0.55, 0.09, 2.22, 0.65, 0.61, 6.45, 0.27, 4.81, 2.27, 0.34, 4.51, 0.42, 3.71, 2.59, 0.42, 11.18)
set_d <- c(4.83, 4.37, 5.57, 4.22, 5.96, 5.11, 5.52, 4.81, 5.19, 4.19, 4.73, 5.92, 5.63, 4.53, 4.67, 4.84, 5.25, 5.06, 5.98, 5.25, 4.60, 4.11, 4.32, 5.09, 5.25, 5.10, 4.36, 5.40, 5.33, 4.65)
set_e <- c(11, 11, 10, 10, 10, 6, 5, 9, 11, 10, 14, 8, 11, 6, 13, 9, 14, 16, 14, 10, 7, 7, 11, 12, 9, 5, 12, 15, 9, 12)

# Setul a
# Ajustarea distribuției normale
fit_norm <- fitdist(set_a, "norm")
AIC_norm <- AIC(fit_norm)
BIC_norm <- BIC(fit_norm)

# Ajustarea distribuției Poisson
fit_pois <- fitdist(set_a, "pois")
AIC_pois <- AIC(fit_pois)
BIC_pois <- BIC(fit_pois)

# Ajustarea distribuției exponențiale
fit_exp <- fitdist(set_a, "exp")
AIC_exp <- AIC(fit_exp)
BIC_exp <- BIC(fit_exp)

# Ajustarea distribuției gamma
fit_gamma <- fitdist(set_a, "gamma")
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

# Setul b
# Ajustarea distribuției normale
fit_norm <- fitdist(set_b, "norm")
AIC_norm <- AIC(fit_norm)
BIC_norm <- BIC(fit_norm)

# Ajustarea distribuției Poisson
fit_pois <- fitdist(set_b, "pois")
AIC_pois <- AIC(fit_pois)
BIC_pois <- BIC(fit_pois)

# Ajustarea distribuției exponențiale
fit_exp <- fitdist(set_b, "exp")
AIC_exp <- AIC(fit_exp)
BIC_exp <- BIC(fit_exp)

# Ajustarea distribuției gamma
fit_gamma <- fitdist(set_b, "gamma")
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

# Setul c
# Ajustarea distribuției normale
fit_norm <- fitdist(set_c, "norm")
AIC_norm <- AIC(fit_norm)
BIC_norm <- BIC(fit_norm)

# Ajustarea distribuției Poisson
fit_pois <- fitdist(set_c, "pois")
AIC_pois <- AIC(fit_pois)
BIC_pois <- BIC(fit_pois)

# Ajustarea distribuției exponențiale
fit_exp <- fitdist(set_c, "exp")
AIC_exp <- AIC(fit_exp)
BIC_exp <- BIC(fit_exp)

# Ajustarea distribuției gamma
fit_gamma <- fitdist(set_c, "gamma")
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

# Setul d
# Ajustarea distribuției normale
fit_norm <- fitdist(set_d, "norm")
AIC_norm <- AIC(fit_norm)
BIC_norm <- BIC(fit_norm)

# Ajustarea distribuției Poisson
fit_pois <- fitdist(set_d, "pois")
AIC_pois <- AIC(fit_pois)
BIC_pois <- BIC(fit_pois)

# Ajustarea distribuției exponențiale
fit_exp <- fitdist(set_d, "exp")
AIC_exp <- AIC(fit_exp)
BIC_exp <- BIC(fit_exp)

# Ajustarea distribuției gamma
fit_gamma <- fitdist(set_d, "gamma")
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

# Setul e
# Ajustarea distribuției normale
fit_norm <- fitdist(set_e, "norm")
AIC_norm <- AIC(fit_norm)
BIC_norm <- BIC(fit_norm)

# Ajustarea distribuției Poisson
fit_pois <- fitdist(set_e, "pois")
AIC_pois <- AIC(fit_pois)
BIC_pois <- BIC(fit_pois)

# Ajustarea distribuției exponențiale
fit_exp <- fitdist(set_e, "exp")
AIC_exp <- AIC(fit_exp)
BIC_exp <- BIC(fit_exp)

# Ajustarea distribuției gamma
fit_gamma <- fitdist(set_e, "gamma")
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

# 9.3
# install.packages("moments")
# library(moments)

set_a <- c(7, 4, 2, 11, 2, 1, 2, 1, 6, 6, 0, 1, 3, 9, 7, 0, 1, 14, 0, 5, 1, 5, 2, 4, 3, 1, 0, 0, 26, 1)
set_b <- c(-1.91, -0.97, 4.59, 2.19, -0.86, -0.74, -0.60, -1.29, 0.93, 1.42, 2.14, -2.01, 2.60, 1.45, 2.60, -3.32, -3.62, 3.09, 2.91, 3.60, -0.83, -0.27, 1.82, -1.38, -1.76, 1.43, -0.59, -1.34, 2.07, 1.02)
set_c <- c(0.90, 8.91, 0.06, 1.85, 1.61, 6.50, 0.26, 0.04, 0.62, 1.01, 3.42, 1.45, 3.44, 0.46, 0.55, 0.09, 2.22, 0.65, 0.61, 6.45, 0.27, 4.81, 2.27, 0.34, 4.51, 0.42, 3.71, 2.59, 0.42, 11.18)
set_d <- c(4.83, 4.37, 5.57, 4.22, 5.96, 5.11, 5.52, 4.81, 5.19, 4.19, 4.73, 5.92, 5.63, 4.53, 4.67, 4.84, 5.25, 5.06, 5.98, 5.25, 4.60, 4.11, 4.32, 5.09, 5.25, 5.10, 4.36, 5.40, 5.33, 4.65)
set_e <- c(11, 11, 10, 10, 10, 6, 5, 9, 11, 10, 14, 8, 11, 6, 13, 9, 14, 16, 14, 10, 7, 7, 11, 12, 9, 5, 12, 15, 9, 12)

# Estimarea prin metoda verosimilității maxime
mle_a <- fitdistr(set_a, "normal")$estimate
mle_b <- fitdistr(set_b, "normal")$estimate
mle_c <- fitdistr(set_c, "normal")$estimate
mle_d <- fitdistr(set_d, "normal")$estimate
mle_e <- fitdistr(set_e, "normal")$estimate

# Estimarea prin metoda momentelor
moment_a <- mnormt::mleMoment(set_a, "mnorm")$param
moment_b <- mnormt::mleMoment(set_b, "mnorm")$param
moment_c <- mnormt::mleMoment(set_c, "mnorm")$param
moment_d <- mnormt::mleMoment(set_d, "mnorm")$param
moment_e <- mnormt::mleMoment(set_e, "mnorm")$param

# Compararea rezultatelor
comparatie <- data.frame(
  Set = c("a", "b", "c", "d", "e"),
  Parametri_MLE = c(mle_a, mle_b, mle_c, mle_d, mle_e),
  Parametri_Moment = c(moment_a, moment_b, moment_c, moment_d, moment_e)
)
comparatie

# 9.4

set_a <- c(7, 4, 2, 11, 2, 1, 2, 1, 6, 6, 0, 1, 3, 9, 7, 0, 1, 14, 0, 5, 1, 5, 2, 4, 3, 1, 0, 0, 26, 1)
set_b <- c(-1.91, -0.97, 4.59, 2.19, -0.86, -0.74, -0.60, -1.29, 0.93, 1.42, 2.14, -2.01, 2.60, 1.45, 2.60, -3.32, -3.62, 3.09, 2.91, 3.60, -0.83, -0.27, 1.82, -1.38, -1.76, 1.43, -0.59, -1.34, 2.07, 1.02)
set_c <- c(0.90, 8.91, 0.06, 1.85, 1.61, 6.50, 0.26, 0.04, 0.62, 1.01, 3.42, 1.45, 3.44, 0.46, 0.55, 0.09, 2.22, 0.65, 0.61, 6.45, 0.27, 4.81, 2.27, 0.34, 4.51, 0.42, 3.71, 2.59, 0.42, 11.18)
set_d <- c(4.83, 4.37, 5.57, 4.22, 5.96, 5.11, 5.52, 4.81, 5.19, 4.19, 4.73, 5.92, 5.63, 4.53, 4.67, 4.84, 5.25, 5.06, 5.98, 5.25, 4.60, 4.11, 4.32, 5.09, 5.25, 5.10, 4.36, 5.40, 5.33, 4.65)
set_e <- c(11, 11, 10, 10, 10, 6, 5, 9, 11, 10, 14, 8, 11, 6, 13, 9, 14, 16, 14, 10, 7, 7, 11, 12, 9, 5, 12, 15, 9, 12)

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
