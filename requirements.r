# Lista de paquetes requeridos
required_packages <- c(
  "shiny",
  "shinythemes",
  "ggplot2",
  "DT",
  "car",
  "corrplot",
  "bslib",
  "MASS",
  "reshape2",
  "lmtest"
)

# Instalar si no están instalados
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

# Instalar todos los paquetes
lapply(required_packages, install_if_missing)