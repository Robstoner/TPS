setwd("./proiect/R")
source("functie_exemplu.r")

graphic_representation <- function(user_function) {
  # Definiți intervalul de valori pentru x
  x <- seq(-20, 10, length.out = 100)

  # Calculați valorile densității de probabilitate/funcției de masă pentru x
  density <- user_function(x)

  # Calculați valorile funcției de repartiție pentru x
  cumulative <- cumsum(density)

  # Creați un obiect de tipul data.frame pentru a stoca valorile
  # Creați graficul pentru densitatea de probabilitate/funcția de masă
  jpeg("../../density_plot.jpg", width = 350, height = 350)
  plot(x, density,
    type = "l", xlab = "x", ylab = "Probabilitate",
    main = "Densitatea de probabilitate/Functia de masa"
  )
  dev.off()

  # Creați graficul pentru funcția de repartiție
  jpeg("../../repartition_plot.jpg", width = 350, height = 350)
  plot(x, cumulative,
    type = "l", xlab = "x", ylab = "Probabilitate",
    main = "Functia de repartitie"
  )
  dev.off()
}

graphic_representation(density_function)
