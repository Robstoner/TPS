source("functie_exemplu.r")

calcul_cov_corel_functii <- function(f_pmf, f_pdf) {
    # Definirea setului de valori pentru variabilele X și Y
    set_valori_X <- c(1, 2, 2, 2)
    set_valori_Y <- c(1, 2, 1, 2)

    # Calcularea covarianței utilizând funcția de masă comună
    covarianta <- sum((set_valori_X - mean(set_valori_X)) * (set_valori_Y - mean(set_valori_Y)) * f_pmf(set_valori_X, set_valori_Y))

    # Calcularea deviației standard a variabilei X
    dev_std_X <- sqrt(sum((set_valori_X - mean(set_valori_X))^2 * f_pmf(set_valori_X, set_valori_Y)))

    # Calcularea deviației standard a variabilei Y
    dev_std_Y <- sqrt(sum((set_valori_Y - mean(set_valori_Y))^2 * f_pmf(set_valori_X, set_valori_Y)))

    # Calcularea coeficientului de corelație
    coef_corel <- covarianta / (dev_std_X * dev_std_Y)

    # Returnarea covarianței și coeficientului de corelație
    return(list(covarianta = covarianta, coeficient_corelatie = coef_corel))
}

# Apelarea funcției pentru calcularea covarianței și coeficientului de corelație
rezultate <- calcul_cov_corel_functii(f_pmf, f_pdf)
print(paste(rezultate$covarianta))
print(paste(rezultate$coeficient_corelatie))
