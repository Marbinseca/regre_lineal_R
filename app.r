# Instalar paquetes necesarios
# install.packages(c("shiny", "shinythemes", "ggplot2", "DT", "car", "corrplot", "bslib"))

library(shiny)
library(shinythemes)
library(ggplot2)
library(DT)
library(car)
library(corrplot)
library(bslib)



# Función alternativa para matriz de dispersión
simple_pairs_plot <- function(data) {
  if (ncol(data) < 2) return(NULL)
  
  # Limitar a 4 variables para no sobrecargar
  if (ncol(data) > 4) {
    data <- data[, 1:4]
  }
  
  # Crear matriz de gráficos manualmente
  var_names <- names(data)
  n_vars <- length(var_names)
  
  # Configurar el layout
  par(mfrow = c(n_vars, n_vars), mar = c(2, 2, 2, 2))
  
  for (i in 1:n_vars) {
    for (j in 1:n_vars) {
      if (i == j) {
        # Diagonal: histograma
        hist(data[[i]], main = "", xlab = "", ylab = "",
             col = "steelblue", border = "white", probability = TRUE)
        lines(density(data[[i]]), col = "red", lwd = 2)
        title(main = var_names[i], line = 0.5)
      } else {
        # Fuera de diagonal: scatter plot
        plot(data[[j]], data[[i]], 
             pch = 16, col = alpha("steelblue", 0.5),
             xlab = "", ylab = "")
        
        # Agregar línea de regresión
        if (i > j) {
          abline(lm(data[[i]] ~ data[[j]]), col = "red", lwd = 2)
        }
        
        # Agregar correlación
        if (i < j) {
          cor_val <- round(cor(data[[i]], data[[j]], use = "complete.obs"), 2)
          legend("topright", legend = paste("r =", cor_val), 
                 bty = "n", cex = 1.2)
        }
      }
    }
  }
  par(mfrow = c(1, 1))
}

# Definir UI
ui <- fluidPage(
  withMathJax(),
  theme = shinytheme("flatly"),
  tags$head(tags$link(rel="icon", type="image/png", href="icon.png")),
  titlePanel("Analisis de Regresion Lineal Interactivo"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Configuracion del Modelo"),
      
      selectInput("dataset", 
                  label = h5("Seleccionar Dataset:"),
                  choices = c("mtcars", "iris", "swiss", "USArrests", 
                             "Seleccionar archivo" = "upload"),
                  selected = "mtcars"),
      
      # Para subir archivo propio
      conditionalPanel(
        condition = "input.dataset == 'upload'",
        fileInput("file", "Subir archivo CSV",
                  accept = c(".csv", ".txt")),
        checkboxInput("header", "Encabezados", TRUE),
        selectInput("sep", "Separador",
                   choices = c(Coma = ",", PuntoYComa = ";", Tab = "\t"),
                   selected = ",")
      ),
      
      uiOutput("dataset_info"),
      br(),
      uiOutput("dep_var_selector"),
      uiOutput("indep_var_selector"),
      
      actionButton("run_model", 
                   label = "Ejecutar Modelo",
                   class = "btn-primary",
                   width = "100%"),
      
      hr(),
      helpText("Selecciona un dataset y las variables para el modelo."),
      br(), # Espaciado
      div(style = "text-align: center;",
          img(src = "icon.png", height = "200px", width = "auto")
      )
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Resumen de Datos",
                 br(),
                 DTOutput("data_summary_table"),
                 br(),
                 h4("Vista Previa"),
                 DTOutput("data_preview")
        ),
        
        tabPanel("Histogramas",
                 br(),
                 uiOutput("histogram_selector"),
                 plotOutput("histograms", height = "500px")
        ),
        
        tabPanel("Boxplots",
                 br(),
                 uiOutput("boxplot_selector"),
                 plotOutput("boxplots", height = "500px")
        ),
        
        tabPanel("Correlaciones",
                 br(),
                 h4("Matriz de Correlacion"),
                 plotOutput("corr_plot", height = "400px"),
                 br(),
                 h4("Matriz de Dispersion"),
                 plotOutput("scatter_matrix", height = "500px")
        ),
        
        tabPanel("Diagnostico del Modelo",
                 br(),
                 h4("Resumen del Modelo"),
                 verbatimTextOutput("model_summary"),
                 br(),
                 h4("Graficos de Diagnostico"),
                 selectInput("diagnostic_plot",
                             label = "Seleccionar grafico:",
                             choices = c("Residuos vs Ajustados",
                                         "Q-Q Plot",
                                         "Escala-Localizacion",
                                         "Residuos vs Leverage")),
                 plotOutput("diagnostic_plots", height = "400px"),
                 br(),
                 h4("Pruebas de Supuestos"),
                 verbatimTextOutput("assumption_tests")
        ),
        
        tabPanel("Predicciones",
                 br(),
                 h4("Predicciones del Modelo"),
                 DTOutput("predictions_table"),
                 br(),
                 h4("Grafico de Prediccion vs Real"),
                 plotOutput("prediction_plot", height = "400px")
        ),
        
        tabPanel("Teoria de Regresion",
                 br(),
                 h3("Teoria de la Regresion Lineal"),
                 p("La regresion lineal es un metodo estadistico que modela la relacion entre una variable dependiente (Y) y una o mas variables independientes (X). El objetivo es encontrar la mejor linea (o hiperplano) que explique los datos."),
                 
                 hr(),
                 
                 h4("1. Modelo Matematico"),
                 p("El modelo de regresion lineal multiple se expresa como:"),
                 p("$$Y = \\beta_0 + \\beta_1X_1 + \\beta_2X_2 + ... + \\beta_kX_k + \\epsilon$$"),
                 tags$ul(
                   tags$li(strong("Y:"), "Variable dependiente (respuesta)."),
                   tags$li(strong("X:"), "Variables independientes (predictores)."),
                   tags$li(strong("\\(\\beta_0\\):"), "Intercepto (valor esperado de Y cuando todas las X son 0)."),
                   tags$li(strong("\\(\\beta_i\\):"), "Coeficientes (cambio promedio en Y por cada unidad que aumenta \\(X_i\\), manteniendo las demas constantes)."),
                   tags$li(strong("\\(\\epsilon\\):"), "Error aleatorio (residual), representa la variabilidad no explicada por el modelo.")
                 ),
                 
                 br(),
                 h4("2. Supuestos del Modelo"),
                 p("Para que los estimadores sean insesgados y eficientes (Teorema de Gauss-Markov), y para realizar inferencia valida, se deben cumplir los siguientes supuestos:"),
                 tags$ol(
                   tags$li(strong("Linealidad:"), "La relacion entre las variables independientes y la media de la variable dependiente es lineal. Si no se cumple, las predicciones seran sesgadas."),
                   tags$li(strong("Independencia:"), "Los residuos (errores) deben ser independientes entre si. La violacion de este supuesto (autocorrelacion) afecta los errores estandar y los valores p."),
                   tags$li(strong("Homocedasticidad:"), "La varianza de los residuos debe ser constante en todos los niveles de las variables predictoras. Si la varianza cambia (heterocedasticidad), los errores estandar seran incorrectos."),
                   tags$li(strong("Normalidad:"), "Los residuos deben seguir una distribucion normal (para muestras pequeñas). Esto es crucial para la validez de los intervalos de confianza y las pruebas de hipotesis."),
                   tags$li(strong("No Multicolinealidad:"), "Las variables independientes no deben estar altamente correlacionadas entre si. La multicolinealidad infla la varianza de los coeficientes, haciendolos inestables.")
                 ),
                 
                 br(),
                 h4("3. Pruebas de Diagnostico e Interpretacion"),
                 p("Esta aplicacion incluye pruebas estadisticas para validar los supuestos anteriores:"),
                 tags$dl(
                   tags$dt("Normalidad de Residuos (Test de Shapiro-Wilk)"),
                   tags$dd("Evalua si los residuos siguen una distribucion normal.",
                           tags$ul(
                             tags$li(em("Hipotesis Nula (H0):"), "Los datos siguen una distribucion normal."),
                             tags$li(em("Interpretacion:"), "Si el p-value > 0.05, no se rechaza H0 (asumimos normalidad). Si p-value < 0.05, se rechaza H0 (no hay normalidad).")
                           )),
                   br(),
                   tags$dt("Homocedasticidad (Test de Breusch-Pagan)"),
                   tags$dd("Evalua si la varianza de los errores es constante (homocedasticidad) o si varia (heterocedasticidad).",
                           tags$ul(
                             tags$li(em("Hipotesis Nula (H0):"), "La varianza es constante (homocedasticidad)."),
                             tags$li(em("Interpretacion:"), "Si el p-value > 0.05, asumimos homocedasticidad. Si p-value < 0.05, hay evidencia de heterocedasticidad.")
                           )),
                   br(),
                   tags$dt("Multicolinealidad (VIF - Factor de Inflacion de la Varianza)"),
                   tags$dd("Mide cuanto aumenta la varianza de un coeficiente debido a la correlacion con otras variables.",
                           tags$ul(
                             tags$li("VIF = 1: No hay correlacion."),
                             tags$li("1 < VIF < 5: Correlacion moderada (generalmente aceptable)."),
                             tags$li("VIF > 5 o 10: Alta multicolinealidad (preocupante, considere eliminar variables).")
                           ))
                 ),
                 
                 br(),
                 h4("4. Interpretacion de Graficos de Diagnostico"),
                 tags$ul(
                   tags$li(strong("Residuos vs Ajustados:"), "Se usa para verificar linealidad y homocedasticidad. Idealmente, los puntos deben distribuirse aleatoriamente alrededor de la linea horizontal 0, sin patrones claros (como forma de embudo o 'U')."),
                   tags$li(strong("Q-Q Plot (Normal Q-Q):"), "Verifica la normalidad. Los puntos (residuos estandarizados) deben alinearse sobre la linea diagonal punteada. Desviaciones en los extremos indican colas pesadas o asimetria."),
                   tags$li(strong("Escala-Localizacion:"), "Similar al primero, verifica la homocedasticidad usando la raiz cuadrada de los residuos. La linea roja debe ser aproximadamente horizontal."),
                   tags$li(strong("Residuos vs Leverage:"), "Identifica valores influyentes (outliers). Puntos fuera de las lineas de 'Distancia de Cook' pueden tener un impacto desproporcionado en el modelo.")
                 ),
                 br()
        )
      )
    )
  )
)

# Servidor
server <- function(input, output, session) {
  
  # Datos reactivos
  dataset_input <- reactive({
    if (input$dataset == "upload") {
      req(input$file)
      tryCatch({
        df <- read.csv(input$file$datapath,
                      header = input$header,
                      sep = input$sep,
                      stringsAsFactors = TRUE)
        return(df)
      }, error = function(e) {
        showNotification("Error al leer el archivo", type = "error")
        return(mtcars)
      })
    } else {
      switch(input$dataset,
             "mtcars" = mtcars,
             "iris" = iris,
             "swiss" = swiss,
             "USArrests" = USArrests,
             mtcars)
    }
  })
  
  # Info del dataset
  output$dataset_info <- renderUI({
    data <- dataset_input()
    tagList(
      p(strong("Filas:"), nrow(data)),
      p(strong("Columnas:"), ncol(data)),
      p(strong("Variables numericas:"), 
        sum(sapply(data, is.numeric)))
    )
  })
  
  # Selector de variable dependiente
  output$dep_var_selector <- renderUI({
    data <- dataset_input()
    numeric_cols <- names(data)[sapply(data, is.numeric)]
    
    if (length(numeric_cols) == 0) {
      return(helpText("No hay variables numericas en el dataset"))
    }
    
    selectInput("dep_var",
                label = h5("Variable Dependiente (Y):"),
                choices = numeric_cols,
                selected = numeric_cols[1])
  })
  
  # Selector de variables independientes
  output$indep_var_selector <- renderUI({
    data <- dataset_input()
    numeric_cols <- names(data)[sapply(data, is.numeric)]
    
    if (!is.null(input$dep_var)) {
      numeric_cols <- setdiff(numeric_cols, input$dep_var)
    }
    
    if (length(numeric_cols) == 0) {
      return(helpText("No hay variables independientes disponibles"))
    }
    
    n_vars <- min(3, length(numeric_cols))
    selected_vars <- numeric_cols[1:n_vars]
    
    selectInput("indep_vars",
                label = h5("Variables Independientes (X):"),
                choices = numeric_cols,
                multiple = TRUE,
                selected = selected_vars)
  })
  
  # Modelo de regresion
  regression_model <- eventReactive(input$run_model, {
    req(input$dep_var, input$indep_vars)
    data <- dataset_input()
    
    formula_str <- paste(input$dep_var, "~", 
                        paste(input$indep_vars, collapse = " + "))
    formula_obj <- as.formula(formula_str)
    
    model <- lm(formula_obj, data = data)
    return(model)
  })
  
  # Pestaña 1: Resumen
  output$data_summary_table <- renderDT({
    data <- dataset_input()
    
    summary_data <- data.frame(
      Variable = names(data),
      Tipo = sapply(data, class),
      Unicos = sapply(data, function(x) length(unique(x))),
      NAs = sapply(data, function(x) sum(is.na(x))),
      stringsAsFactors = FALSE
    )
    
    # Agregar estadisticas para variables numericas
    if (any(sapply(data, is.numeric))) {
      summary_data$Media <- sapply(data, function(x) 
        ifelse(is.numeric(x), round(mean(x, na.rm = TRUE), 3), NA))
      summary_data$Desviacion <- sapply(data, function(x) 
        ifelse(is.numeric(x), round(sd(x, na.rm = TRUE), 3), NA))
    }
    
    datatable(summary_data, 
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })
  
  output$data_preview <- renderDT({
    datatable(dataset_input(), 
              options = list(pageLength = 5, scrollX = TRUE))
  })
  
  # Pestaña 2: Histogramas
  output$histogram_selector <- renderUI({
    data <- dataset_input()
    numeric_cols <- names(data)[sapply(data, is.numeric)]
    
    if (length(numeric_cols) > 4) {
      selectInput("hist_vars",
                  label = "Seleccionar variables:",
                  choices = numeric_cols,
                  multiple = TRUE,
                  selected = numeric_cols[1:4])
    }
  })
  
  output$histograms <- renderPlot({
    data <- dataset_input()
    numeric_cols <- names(data)[sapply(data, is.numeric)]
    
    if (length(numeric_cols) == 0) {
      plot(1, 1, type = "n", main = "No hay variables numericas")
      return()
    }
    
    if (!is.null(input$hist_vars) && length(input$hist_vars) > 0) {
      show_vars <- input$hist_vars
    } else {
      show_vars <- numeric_cols[1:min(4, length(numeric_cols))]
    }
    
    # Convertir a formato largo para ggplot
    plot_data <- data[, show_vars, drop = FALSE]
    plot_data_long <- data.frame(
      value = unlist(plot_data),
      variable = rep(names(plot_data), each = nrow(plot_data))
    )
    
    ggplot(plot_data_long, aes(x = value)) +
      geom_histogram(aes(y = ..density..), 
                    bins = 20, fill = "steelblue", alpha = 0.7) +
      geom_density(color = "red", size = 1) +
      facet_wrap(~variable, scales = "free") +
      theme_minimal() +
      labs(title = "Histogramas", x = "Valor", y = "Densidad")
  })
  
  # Pestaña 3: Boxplots
  output$boxplot_selector <- renderUI({
    data <- dataset_input()
    numeric_cols <- names(data)[sapply(data, is.numeric)]
    
    if (length(numeric_cols) > 4) {
      selectInput("box_vars",
                  label = "Seleccionar variables:",
                  choices = numeric_cols,
                  multiple = TRUE,
                  selected = numeric_cols[1:4])
    }
  })
  
  output$boxplots <- renderPlot({
    data <- dataset_input()
    numeric_cols <- names(data)[sapply(data, is.numeric)]
    
    if (length(numeric_cols) == 0) {
      plot(1, 1, type = "n", main = "No hay variables numericas")
      return()
    }
    
    if (!is.null(input$box_vars) && length(input$box_vars) > 0) {
      show_vars <- input$box_vars
    } else {
      show_vars <- numeric_cols[1:min(4, length(numeric_cols))]
    }
    
    plot_data <- data[, show_vars, drop = FALSE]
    plot_data_long <- data.frame(
      value = unlist(plot_data),
      variable = rep(names(plot_data), each = nrow(plot_data))
    )
    
    ggplot(plot_data_long, aes(x = variable, y = value, fill = variable)) +
      geom_boxplot(alpha = 0.7) +
      stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "yellow") +
      theme_minimal() +
      labs(title = "Boxplots", x = "", y = "Valor") +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Pestaña 4: Correlaciones
  output$corr_plot <- renderPlot({
    data <- dataset_input()
    numeric_data <- data[, sapply(data, is.numeric), drop = FALSE]
    
    if (ncol(numeric_data) < 2) {
      plot(1, 1, type = "n", main = "No hay suficientes variables numericas")
      return()
    }
    
    cor_matrix <- cor(numeric_data, use = "complete.obs")
    
    corrplot(cor_matrix, 
             method = "color",
             type = "upper",
             tl.col = "black",
             tl.srt = 45,
             addCoef.col = "black",
             number.cex = 0.8,
             title = "Matriz de Correlacion")
  })
  
  output$scatter_matrix <- renderPlot({
    data <- dataset_input()
    numeric_data <- data[, sapply(data, is.numeric), drop = FALSE]
    
    if (ncol(numeric_data) < 2) {
      plot(1, 1, type = "n", main = "Se necesitan al menos 2 variables numericas")
      return()
    }
    
    # Limitar a 4 variables
    if (ncol(numeric_data) > 4) {
      numeric_data <- numeric_data[, 1:4]
    }
    
    simple_pairs_plot(numeric_data)
  })
  
  # Pestaña 5: Diagnostico
  output$model_summary <- renderPrint({
    req(regression_model())
    summary(regression_model())
  })
  
  output$diagnostic_plots <- renderPlot({
    req(regression_model())
    
    which_plot <- switch(input$diagnostic_plot,
                        "Residuos vs Ajustados" = 1,
                        "Q-Q Plot" = 2,
                        "Escala-Localizacion" = 3,
                        "Residuos vs Leverage" = 5)
    
    plot(regression_model(), which = which_plot,
         pch = 16, col = "steelblue", cex = 1.2)
  })
  
  output$assumption_tests <- renderPrint({
    req(regression_model())
    model <- regression_model()
    
    cat("=== PRUEBAS DE SUPUESTOS ===\n\n")
    
    # Normalidad
    cat("1. Normalidad de residuos (Shapiro-Wilk):\n")
    shapiro_test <- shapiro.test(residuals(model))
    print(shapiro_test)
    cat("Interpretacion:", 
        ifelse(shapiro_test$p.value > 0.05, 
               "Residuos normales (p > 0.05)\n",
               "Residuos no normales (p <= 0.05)\n"))
    
    cat("\n2. Homocedasticidad (Breusch-Pagan):\n")
    if (require("lmtest", quietly = TRUE)) {
      bp_test <- lmtest::bptest(model)
      print(bp_test)
      cat("Interpretacion:",
          ifelse(bp_test$p.value > 0.05,
                 "Varianza constante (p > 0.05)\n",
                 "Varianza no constante (p <= 0.05)\n"))
    } else {
      cat("Instalar paquete 'lmtest' para esta prueba\n")
    }
    
    cat("\n3. Multicolinealidad (VIF):\n")
    if (length(coef(model)) > 2) {
      vif_values <- car::vif(model)
      print(vif_values)
      if (any(vif_values > 10)) {
        cat("ALERTA: Multicolinealidad severa (VIF > 10)\n")
      } else if (any(vif_values > 5)) {
        cat("Precaucion: Multicolinealidad moderada (VIF > 5)\n")
      } else {
        cat("OK: Sin multicolinealidad severa (VIF < 5)\n")
      }
    }
  })
  
  # Nueva pestaña: Predicciones
  output$predictions_table <- renderDT({
    req(regression_model())
    model <- regression_model()
    data <- dataset_input()
    
    predictions <- data.frame(
      Real = model$model[[1]],
      Predicho = predict(model),
      Residuos = residuals(model)
    )
    
    datatable(predictions, 
              options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$prediction_plot <- renderPlot({
    req(regression_model())
    model <- regression_model()
    
    predictions <- data.frame(
      Real = model$model[[1]],
      Predicho = predict(model)
    )
    
    ggplot(predictions, aes(x = Real, y = Predicho)) +
      geom_point(color = "steelblue", size = 3, alpha = 0.7) +
      geom_abline(slope = 1, intercept = 0, color = "red", size = 1) +
      geom_smooth(method = "lm", se = FALSE, color = "darkgreen", linetype = "dashed") +
      theme_minimal() +
      labs(title = "Predicciones vs Valores Reales",
           x = "Valor Real",
           y = "Valor Predicho") +
      coord_equal()
  })
}

# Ejecutar aplicacion
shinyApp(ui = ui, server = server)