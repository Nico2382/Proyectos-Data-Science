library(shinydashboard)
library(shiny)
library(DT)
library(corrplot)
library(plotly)
library(ggplot2)
library(reshape2)
library(caTools)
library(NeuralNetTools)
library(neuralnet)
library(caret)


# leo el data frame
df<-read.csv("heart.csv", header = TRUE, sep = ",")
# leo el data frame. de accuracy generado en el rmd
accuracy_df<-read.csv("Accuracy_heart.csv", header = TRUE, sep = ",")

dfcol_frecuencia<-c("age","trestbps","chol","thalach","oldpeak")

#escalo los datos
df_sc = as.data.frame(cbind(df$target, scale(df[ ,c(1:13)])))

# renombro la columna objetivo
names(df_sc)[1] = 'target'

#convierto a factor el target
df_sc$target <- as.factor(df_sc$target)

#balanceo las clases
df_bal_max <- upSample(x = df_sc[, -which(names(df_sc) == 'target')],
                       y = df_sc$target)

df_bal_max <- as.data.frame(df_bal_max)

# renombro la columna objetivo
names(df_bal_max)[14]<-'target'

#|
# Paralelización para grid search 
#| 
num_cores <- 4  # Número deseado de hilos 
registerDoParallel(cores = num_cores)


# Entrenar el modelo con Grid Search
set.seed(123)
control_grid <- trainControl(method = "cv", number = 5)
grid <- expand.grid(mtry = c(3, 6, 10, 14))

modelo_gridsearch <- train(target~ .,data = df_bal_max,
                           method = "rf",
                           tuneGrid = grid,
                           trControl = control_grid)

# busqueda del optimo
mtry_optimo <- as.numeric(modelo_gridsearch$bestTune)


ui <- dashboardPage(
  title = "Heart",
  
  
  dashboardHeader(
    title = tags$span(class = "title-text", "Predicción de Enfermedad Cardiaca"),
    titleWidth = "100%"
  ),
  
  dashboardSidebar(
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$meta(charset = "UTF-8")),
    
    sidebarMenu(
      class = "menu",
      tags$h3(class="title_insti","Secciones"),
      
      menuItem("Autor", tabName = "integrantes", icon = icon("user")),
      menuItem("Marco Teórico", tabName = "MT", icon = icon("book")),
      menuItem("Introducción", tabName = "introduccion", icon = icon("table"),
               menuSubItem("Dataset", tabName = "info"),
               menuSubItem("Descripción", tabName = "dataset")),
      
      menuItem("EDA", tabName = "Analisis", icon = icon("chart-bar")),
      menuItem("Modelos", tabName = "Modelos", icon = icon("cogs")),
      menuItem("Diagnóstico", tabName = "Diagnostico", icon = icon("check"))
    )
  ),
  
  dashboardBody(class="body",
                tabItems(
                  tabItem(tabName = "integrantes",
                          column(4,
                                div(tags$ul(
                                   tags$li(class="integrantes-lista","Nombre y Apellido"),
                                 )),
                                 tags$h3("Castillo Nicolás"),
                                 tags$ul(
                                  tags$li(class="integrantes-lista","Contacto")),
                                 tags$h3(a("Linkedin", href='https://www.linkedin.com/in/nicol%C3%A1s-castillo-b3314b5b', target="_blank")),
                                 tags$ul(
                                  tags$li(class="integrantes-lista","Portafolio de Proyectos")),
                                 tags$h3(a("Sitio web", href='https://66b23e75f3279.site123.me/', target="_blank")),
                                ),
                          column(8,
                                 tags$image(type="port/png", src="portada.jpg",style="width:500px;height:500px;position:relative;margin-left:320px; margin-top:50px;box-shadow:2px 4px 8px  #2C242E;border: solid black 2px")) # El div con la imagen de fondo
                  ),
                  
                  tabItem(tabName = "MT",
                          h2("Objetivos"),
                          h3("Este proyecto se realizó en un entorno Markdown y luego se compilaron los resultados en el presente Shiny. Se trabajó sobre una base de datos de estudios clínicos de enfermedades cardiacas."), 
                          h3("Primero se realizó un análisis exploratorio, limpieza de los datos y pre-procesado de los mismos. Se trabajó sobre el escalado y el balanceo de clases. Luego se entrenearon 6 algoritmos (Modelo logístico con y sin balanceo de clases, Maquina de Soporte Vectorial con y sin balanceo y con tuning de los hiperparametros y Modelo de Random Forest), se compararon sus rendimientos y se construyó una apliación interactiva para predecir, en base a un cuestionario, si el paciente padece una enfermedad cardiaca."),
                          ),
                  
                  tabItem(tabName = "info",
                          fluidRow(
                            tags$h2("Dataset de enfermedades cardiacas de la clinica Cleveland"),br(),
                            tags$h4("Los datos fueron recopilados por el Dr. Robert Detrano, de la Cleveland Clinic Foundation."),
                            tags$h4("La base de datos está compuesta por 14 campos y 303 registros"),br(),
                            tags$li(strong("age"),": Edad del paciente"),
                            tags$li(strong("sex"),": Género del paciente (1 = masculino; 0 = femenino)"),
                            tags$li(strong("cp"),": Tipo de dolor en el pecho (Categórica con 4 tipos: Tipo 1: Angina típica; Tipo 2: Angina Atípica; Tipo 3: Dolor no anginal; Tipo 4: Asintomático)"),
                            tags$li(strong("trestbps"),": Presión arterial en reposo en mm Hg al ingreso al hospital (V. continua)"),
                            tags$li(strong("chol"),": Colesterol sérico en mg /dl (V. continua)"),
                            tags$li(strong("fbs"),": Azúcar en sangre en ayunas > 120 mg/dl (categórica con 2 niveles: TRUE, FALSE)"),
                            tags$li(strong("restecg"),": Resultados electrocardiográficos en reposo (Categórica con 3 niveles: 0: Normal; 1: Tener anormalidad de onda ST-T (inversiones de onda T y / o elevación o Depresión de ST de > 005 mV); 2: Muestra hipertropía ventricular izquierda probable o definitiva"),
                            tags$li(strong("thalach"),": Frecuencia cardiaca máxima alcanzada (V. continua)"),
                            tags$li(strong("exang"),": Angina inducida por el ejercicio (Categórica con dos niveles: 1-Si, 0-No)"),
                            tags$li(strong("oldpeak"),": Depresión del ST inducida por el ejercicio en relación con el descanso (V. continua)"),
                            tags$li(strong("slope"),": La pendiente del segmento ST de ejercicio pico (categórica con 3 niveles: Valor 0: ascenso; Valor 1: plano ;Valor 2: descenso"),
                            tags$li(strong("ca"),": Numero de vasos principales (0-3) coloreados por fluoroscopia (Categórica con 4 niveles: 1-2-3-4)"),
                            tags$li(strong("thal"),": El estado del corazón segC:n la prueba de Thallium (Categórica con 3 niveles: 1 = normal;2 = defecto fijo; 3 = defecto reversible)"),
                            tags$li(strong("num"),": Variable objetivo que representa el diagnóstico de enfermedad cardíaca mediante angiografía coronaria 0: < 50% de estrechamiento del diámetro Valor 1: > 50% de estrechamiento del diámetro"),
                          )
                  ),
                  
                  tabItem(tabName = "dataset",
                          navbarPage("",
                                     tabPanel("Datos",
                                              fluidPage(
                                                DT::DTOutput('table'))
                                     ),
                                     tabPanel("Datos estadisticos",
                                              verbatimTextOutput('data_summary')),
                                     tabPanel("Estructura",
                                              verbatimTextOutput('data_structure')),
                          )
                  ),
                  
                  tabItem(tabName = "Analisis",
                          navbarPage("",
                                     tabPanel("Gráficos individuales",
                                              selectInput("dist","Seleccione la variable:", choices = colnames(df)),
                                              plotlyOutput("distribucion")
                                     ),         
                                     tabPanel("Análisis de datos faltantes",
                                              verbatimTextOutput('faltantes')),         
                                     tabPanel("Matriz de correlación",
                                              div(plotlyOutput('correlacion', width = "100%", height = "600px"))),
                                     tabPanel("Gráficos de correlaciones",
                                              fluidRow(
                                                column(6,
                                                       selectInput("graf_corr_x","Seleccione la variable x:", choices = colnames(df))),
                                                column(6,
                                                       selectInput("graf_corr_y","Seleccione la variable y:", choices = colnames(df)))),
                                              plotlyOutput("graf_corr")
                                     ),                  
                                     
                                     
                          )),
                  tabItem(tabName = "Modelos",
                          tabPanel("Rendimientos de los modelos evaluados",
                                   plotlyOutput('Modelos', width = "100%", height = "600px")      
                            
                          )),
                  
                  tabItem(tabName = "Diagnostico",
                          fluidRow(
                            column(3,
                                   numericInput("age", "Ingrese la edad", min = 1, max = 100, value = 30),       
                                   radioButtons("sex", "Seleccione el género",
                                                choiceNames = list("0-Masculino","1-Femenino"),
                                                choiceValues = list(1,0)
                                   ),
                                   radioButtons("cp", "Seleccione el Tipo de dolor de pecho",
                                                choiceNames = list("0-Angina típica","1-Angina Atípica","2-Dolor no anginal","3-Asintomático"),
                                                choiceValues = list(0,1,2,3)
                                                
                                   ),
                                   sliderInput("trestbps", label = "Seleccione la presión arterial en reposo en mm Hg", min = 80, max = 250, value = 100),
                                   sliderInput("chol", label = "Seleccione el colesterol sérico en mg /dl", min = 100, max = 600, value = 300),
                            ),
                            column(3,
                                   radioButtons("fbs", "¿Azúcar en sangre en ayunas > 120 mg/dl?",
                                                choiceNames = list("1-Si","0-No"),
                                                choiceValues = list(1,0)
                                   ),
                                   radioButtons("restecg", "Resultados electrocardiográficos en reposo",
                                                choiceNames = list("0-Normal","1-Tener anormalidad de onda ST-T","2-Muestra Hipertrofia ventricular izquierda probable"),
                                                choiceValues = list(0,1,2)
                                   ),
                                   sliderInput("thalach", label = "Frecuencia cardiaca máxima alcanzada", min = 60, max = 300, value = 100),
                                   radioButtons("exang", "¿Angina inducida por el ejercicio?",
                                                choiceNames = list("1-Si","0-No"),
                                                choiceValues = list(1,0)
                                   ),
                                   sliderInput("oldpeak", label = "Depresión del ST inducida por el ejercicio en relación con el descanso", min = 0, max = 7, value = 3, step =0.1 )
                            ),
                            column(3,
                                   radioButtons("slope", " La pendiente del segmento ST de ejercicio pico",
                                                choiceNames = list("0-Ascenso","1-Plano","2-Descenso"),
                                                choiceValues = list(0,1,2)),
                                   radioButtons("ca", "Numero de vasos principales coloreados por fluoroscopia",
                                                choiceNames = list("0","1","2","3","4"),
                                                choiceValues = list(0,1,2,3,4)),
                                   radioButtons("thal", "Estado del corazón según la prueba de Thalliu",
                                                choiceNames = list("1-Normal","2-Defecto fijo","3-Defecto reversible"),
                                                choiceValues = list(1,2,3)),
                                   actionButton("predecir", "Predecir Diagnóstico")
                                   
                            ),
                            column(3,
                                   box(
                                     valueBoxOutput("Diagnostico")
                                   )
                            )
                            
                          ))
                  
                )
  ))

server <- function(input, output, session) {
  
  #tabitem dataset----------------
  
  output$table <- DT::renderDataTable(df)
  output$data_summary <- renderPrint(summary(df))
  
  output$data_structure <- renderPrint(str(df))
  
  #tabitem analisis------------
  
  output$faltantes<-renderPrint(sapply(df, function(x) sum(is.na(x))))
  
  output$correlacion <- renderPlotly({
    corr_matrix <- cor(df, use = "complete.obs")  # Calcular la matriz de correlación
    
    # Convertir la matriz de correlación a un formato largo para ggplot2
    corr_data <- melt(corr_matrix)
    colnames(corr_data) <- c("Var1", "Var2", "value")
    
    # Crear el gráfico con ggplot2
    p <- ggplot(corr_data, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(low = "#F8F5F3", high = "#3600FA", mid = "#FDE1BA", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           name="Correlación") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                       size = 12, hjust = 1)) +
      coord_fixed()
    
    # Convertir el gráfico a plotly para hacerlo interactivo
    ggplotly(p)
  })
  
  
  output$distribucion <- renderPlotly({
    req(input$dist)
    df$target <- as.factor(df$target)
    
    # Histograma por grupo en ggplot2
    plot_age<-ggplot(df, aes_string(x = input$dist, fill = df$target , colour = df$target )) + 
      geom_histogram(alpha = 0.5, position = "identity")+ 
      guides(fill = guide_legend(title = "Target"),
             colour = guide_legend(title = "Target"))
    
    grafico_interactivo<-ggplotly(plot_age)
    # Mostrar el gráfico
    print(grafico_interactivo)
    
  })
  
  output$graf_corr<-renderPlotly({
    
    grafico_dispersion <- ggplot(df, aes_string(x = input$graf_corr_x, y=input$graf_corr_y)) +
      geom_point(color = "skyblue") +
      labs(title =  paste(input$graf_corr_x,"vs",input$graf_corr_y), x = input$graf_corr_x, y = input$graf_corr_y) +
      theme_minimal()
    grafico_interactivo_dispersion<-ggplotly(grafico_dispersion)
    grafico_interactivo_dispersion
    
  })
 
  #Output Modelos
  output$Modelos<-renderPlotly({
    #| 
    #| graficar los valores de accuracy
    #| .
    comp<-ggplot(accuracy_df, aes(x = Modelo, y = Accuracy, fill = Modelo)) +
      geom_bar(stat = "identity") +
      labs(
        x = "Modelos",
        y = "Accuracy")+
      
      coord_flip() 
    
    gi6 <-ggplotly(comp)
    
    #| .
    #| Graficar los valores de Sensitivity
    comp<-ggplot(accuracy_df, aes(x = Modelo, y = Sensitivity, fill = Modelo)) +
      geom_bar(stat = "identity") +
      labs(x = "Modelos",
           y = "Sensitivity")+
      coord_flip() 
    gi7 <-ggplotly(comp)
    
    #| .
    #| Graficar los valores de Specificity
    comp<-ggplot(accuracy_df, aes(x = Modelo, y = Specificity, fill = Modelo)) +
      geom_bar(stat = "identity") +
      labs(x = "Modelos",
           y = "Specificity")+
      coord_flip() 
    gi8 <-ggplotly(comp)
    
    #|
    #| Graficar los valores de Tiempos
    comp<-ggplot(accuracy_df, aes(x = Modelo, y = Tiempos, fill = Modelo)) +
      geom_bar(stat = "identity") +
      labs(x = "Modelos",
           y = "Tiempos")+
      coord_flip() 
    gi9 <-ggplotly(comp)
    
    
    # agregar titulos
    annotations = list( 
      list( 
        x = 0.2,  
        y = 1.0,  
        text = "ACCURACY",  
        xref = "paper",  
        yref = "paper",  
        xanchor = "center",  
        yanchor = "bottom",  
        showarrow = FALSE 
      ),  
      list( 
        x = 0.8,  
        y = 1,  
        text = "SENSITIVITY",  
        xref = "paper",  
        yref = "paper",  
        xanchor = "center",  
        yanchor = "bottom",  
        showarrow = FALSE 
      ),  
      list( 
        x = 0.2,  
        y = 0.4,  
        text = "SPECIFICITY",  
        xref = "paper",  
        yref = "paper",  
        xanchor = "center",  
        yanchor = "bottom",  
        showarrow = FALSE 
      ),
      list( 
        x = 0.8,
        y = 0.4,  
        text = "TIEMPOS",  
        xref = "paper",  
        yref = "paper",  
        xanchor = "center",  
        yanchor = "bottom",  
        showarrow = FALSE
        
      ))
    
    
    
    #|
    #| Crear la grilla de gráficos
    subplot(gi6, gi7, gi8, gi9, nrows = 2, margin = 0.08) %>% 
      layout(showlegend = FALSE, annotations = annotations) 
  })
  
  
  
  
  #Output Diagnostico
  prediction <- reactiveVal(NULL)
  
  observeEvent(input$predecir, {
   
    # trainig grid search
    
    set.seed(123)
    
    split <- createDataPartition(df_bal_max$target, p=0.7, list=FALSE)
    
    datos_train_grid_rf <- df_bal_max[split, ]
    
    datos_test_grid_rf <- df_bal_max[-split, ]
    
    age <-  as.numeric(input$age)
    sex <-   as.factor(input$sex) 
    cp <-   as.numeric(input$cp) 
    trestbps <- as.numeric(input$trestbps) 
    chol <- as.numeric(input$chol)
    fbs <-  as.factor(input$fbs)
    restecg <- as.factor(input$restecg)
    thalach <- as.numeric(input$thalach)
    exang <-  as.factor(input$exang)
    oldpeak <- as.numeric(input$oldpeak)
    slope <-  as.factor(input$slope)
    ca <- as.factor(input$ca)
    thal <- as.factor(input$thal)
    
    vect <- c(age, sex, cp, trestbps, chol, fbs, restecg, thalach, exang, oldpeak, slope, ca, thal)
    
    df_eval <- data.frame(t(vect))
    colnames(df_eval) <- c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope", "ca", "thal")
    
    df_train_scaled_no_target <- datos_train_grid_rf [, -which(names(datos_train_grid_rf ) == "target")]
    
    # Extraer los valores de escalado
    mean_vals <- colMeans(df[, c(1:13)], na.rm = TRUE)
    std_vals <- apply(df[, c(1:13)], 2, sd, na.rm = TRUE)
    
    # Escalar el nuevo registro
    df_eval_scaled <- as.data.frame(scale(df_eval, center = mean_vals, scale = std_vals))
    
    # Realizar la predicción
    prediccion_eval <- predict(modelo_grid_rf, df_eval_scaled)
    
    
    
    output$Diagnostico <- renderValueBox({
      if(prediccion_eval=="0"){
      valueBox(
        subtitle = tags$div("Diagnóstico", style = "color: black;"),  # Letras negras para el título
        value = tags$div(paste0("Paciente Sano"), style = "color: black;"),  # Letras negras para el valor
        icon = icon("stethoscope"),  # Icono opcional
        color = "green",  # Fondo rojo
        
      )}else{
        valueBox(
          subtitle = tags$div("Diagnóstico", style = "color: black;"),  # Letras negras para el título
          value = tags$div(paste0("Paciente Enfermo"), style = "color: black;"),  # Letras negras para el valor
          icon = icon("stethoscope"),  # Icono opcional
          color = "red",  # Fondo rojo
          
        )
        }
          })
  })
  
  
  
}




shinyApp(ui, server)
