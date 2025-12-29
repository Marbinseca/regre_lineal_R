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
  theme = shinytheme("flatly"),
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
      helpText("Selecciona un dataset y las variables para el modelo.")
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