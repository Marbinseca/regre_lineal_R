# Análisis de Regresión Lineal Interactivo

Una aplicación web interactiva desarrollada con **R** y **Shiny** para realizar análisis de regresión lineal completos, desde la exploración de datos hasta el diagnóstico del modelo, sin necesidad de escribir código.

## 📋 Descripción

Esta herramienta está diseñada para facilitar el aprendizaje y la aplicación de modelos de regresión lineal. Permite a estudiantes, investigadores y analistas de datos:

- Explorar visualmente sus datasets.
- Ajustar modelos lineales simples y múltiples de forma dinámica.
- Verificar los supuestos estadísticos mediante diagnósticos visuales y pruebas formales.
- Realizar predicciones interactivas.

## ✨ Características Principales

### 1. Gestión de Datos Flexible

- **Datasets Integrados**: Acceso inmediato a `mtcars`, `iris`, `swiss` y `USArrests`.
- **Carga de Datos Propios**: Soporte para archivos CSV con configuración de separadores (coma, punto y coma, tabulación) y encabezados.

### 2. Análisis Exploratorio de Datos (EDA)

- **Resumen Estadístico**: Tabla interactiva con media, desviación estándar, valores únicos y conteo de NAs.
- **Visualización**:
  - Histogramas con curvas de densidad.
  - Boxplots para detección de outliers.
  - Matrices de correlación y gráficos de dispersión (pairs plots).

### 3. Modelado

- Selección dinámica de la variable dependiente ($Y$) e independientes ($X$).
- Ajuste automático del modelo de regresión lineal (`lm`).

### 4. Diagnóstico Completo

- **Resumen del Modelo**: Coeficientes, $R^2$, estadístico F y p-values.
- **Gráficos de Diagnóstico**:
  - Residuos vs Ajustados.
  - Q-Q Plot (Normalidad).
  - Escala-Localización.
  - Residuos vs Leverage.
- **Pruebas Estadísticas**:
  - **Normalidad**: Test de Shapiro-Wilk.
  - **Homocedasticidad**: Test de Breusch-Pagan.
  - **Multicolinealidad**: Factor de Inflación de la Varianza (VIF).

### 5. Predicciones

- Tabla comparativa de valores reales vs. predichos.
- Gráfico de dispersión para evaluar la calidad de la predicción.

## 🛠 Requisitos del Sistema

- **R** (versión reciente recomendada).
- **RStudio** (recomendado para una mejor experiencia de usuario).

### Paquetes R Necesarios

La aplicación utiliza las siguientes librerías:

| Paquete       | Propósito                                         |
| ------------- | ------------------------------------------------- |
| `shiny`       | Framework de aplicación web                       |
| `shinythemes` | Temas visuales (Flatly)                           |
| `ggplot2`     | Gráficos avanzados                                |
| `DT`          | Tablas interactivas                               |
| `car`         | Cálculo del VIF                                   |
| `corrplot`    | Matriz de correlación visual                      |
| `bslib`       | Personalización de temas Bootstrap                |
| `lmtest`      | Test de Breusch-Pagan (Opcional pero recomendado) |

## 🚀 Instalación

1.  **Clonar o Descargar** el repositorio en tu equipo local.
2.  **Instalar Dependencias**: Abre R o RStudio y ejecuta el siguiente comando:

    ```r
    pkg_list <- c("shiny", "shinythemes", "ggplot2", "DT", "car", "corrplot", "bslib", "lmtest")
    install.packages(pkg_list)
    ```

## 💻 Cómo Ejecutar la Aplicación

### Opción 1: Desde RStudio (Recomendado)

1.  Abre el archivo `app.r` en RStudio.
2.  Haz clic en el botón verde **Run App** ubicado en la parte superior del editor de scripts.

### Opción 2: Desde la Consola de R

Asegúrate de que tu directorio de trabajo sea la carpeta del proyecto y ejecuta:

```r
library(shiny)
runApp("app.r")
```

O directamente usando el namespace de shiny:

```r
shiny::runApp("app.r")
```

## 📁 Estructura para Datasets Personalizados

Si deseas cargar tus propios datos, asegúrate de que tu archivo CSV cumpla con lo siguiente:

- **Formato**: Valores separados por comas (`,`), punto y coma (`;`) o tabulaciones.
- **Encabezados**: La primera fila debe contener los nombres de las variables.
- **Variables Numéricas**: Asegúrate de que las columnas que usarás para la regresión contengan solo números. Evita símbolos de moneda o texto en estas columnas.
- **Nombres Limpios**: Evita caracteres especiales o espacios en los nombres de las columnas para facilitar su lectura.

## 📚 Ejemplos de Uso

### Ejemplo 1: Consumo de Combustible (mtcars)

- **Objetivo**: Predecir `mpg` (millas por galón).
- **Configuración**:
  - Variable Dependiente: `mpg`
  - Variables Independientes: `hp` (potencia), `wt` (peso), `cyl` (cilindros).
- **Interpretación**: Revisa el $R^2$ ajustado para ver la calidad del ajuste y el test de Shapiro-Wilk para confirmar la normalidad de los residuos.

### Ejemplo 2: Morfología de Flores (iris)

- **Objetivo**: Analizar la relación entre dimensiones del sépalo.
- **Configuración**:
  - Variable Dependiente: `Sepal.Length`
  - Variables Independientes: `Sepal.Width`, `Petal.Length`.
- **Nota**: Útil para explorar la correlación entre variables biológicas.

## 📂 Estructura del Proyecto

```text
.
├── app.r       # Código fuente principal (UI y Server)
└── README.md   # Documentación del proyecto
```

---

Creado con ❤️ usando R Shiny.
