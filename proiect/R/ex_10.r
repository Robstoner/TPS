source("functie_exemplu.r")

ex_1 <- function(set_valori) {
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

ex_2 <- function(set) {
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

ex_3 <- function(set) {
  mle <- MASS::fitdistr(set, "normal")$estimate

  moment <- c(mean(set), sd(set))

  comparatie <- data.frame(
    Parametri = c("Media", "Deviatia standard"),
    MLE = mle,
    Moment = moment
  )

  print(comparatie)
}

ex_4 <- function(set) {
  test <- shapiro.test(set)

  rezultate <- data.frame(
    P_Value = test$p.value,
    Normala = NA
  )

  rezultate$Normala[rezultate$P_Value > 0.05] <- "Da"
  rezultate$Normala[rezultate$P_Value <= 0.05] <- "Nu"

  print(rezultate)
}

main <- function() {
  # Citirea setului de valori
  set_valori <- readline(prompt = "Introduceti setul de valori (separate prin virgula): ")
  set_valori <- as.numeric(strsplit(set_valori, ",")[[1]])

  # Meniu interactiv
  while (TRUE) {
    cat("\nAlegeti optiunea dorita:")
    cat("\n1. 9.1")
    cat("\n2. 9.2")
    cat("\n3. 9.3")
    cat("\n4. 9.4")
    cat("\n0. Iesire\n")

    optiune <- readline(prompt = "Optiune: ")

    if (optiune == "0") {
      break
    }

    if (optiune == "1") {
      ex_1(set_valori)
    } else if (optiune == "2") {
      ex_2(set_valori)
    } else if (optiune == "3") {
      ex_3(set_valori)
    } else if (optiune == "4") {
      ex_4(set_valori)
    } else {
      cat("Optiune invalida! Va rugam sa alegeti o optiune valida.\n")
    }
  }
}

main()
