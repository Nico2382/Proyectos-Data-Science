#Librerías

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








#data frame
df<-read.csv("heart.csv", header = TRUE, sep = ",")
dfcol_frecuencia<-c("age","trestbps","chol","thalach","oldpeak")


df_sc = as.data.frame(cbind(df$target, scale(df[ ,c(1:13)])))
names(df_sc)[1] = 'target'

set.seed(123)
split <- sample.split(df_sc$target, SplitRatio = 0.7)
str(split)
train <- subset(df_sc, split == TRUE)
test <- subset(df_sc, split == FALSE)

model.net_bm <- neuralnet(target ~ . , 
                          data = train,
                          hidden = c(2,4),
                          stepmax = 1e+05,
                          linear.output = FALSE)
predictions <- neuralnet::compute(model.net_bm, test)$net.result
predictions <- round(predictions)
# Crear la matriz de confusión
conf_matrix <- table(estimado = predictions, observado = test$target)

# Convertir la matriz de confusión a un data frame para usar con ggplot2
conf_matrix_df <- as.data.frame(as.table(conf_matrix))

#Comentamos la creación de la grilla para grafico 3D dado que
# demora bastante tiempo por lo que se decidió guardar en un .csv los
# resultados
#Grilla para grafico 3d
#grilla <- expand.grid(x = 1:10, y = 1:10)

#Función gráfico 3D
#train_and_evaluate <- function(hidden_layers) {
#  model.net <- neuralnet(target ~ ., 
#                         data = train, 
#                         hidden = hidden_layers, 
#                         linear.output = TRUE)
#  predictions <- neuralnet::compute(model.net, 
#                                    test)$net.result
#  mse <- sum((predictions - test$target)^2) / nrow(test)
#  
#  return(mse)
#}





ui <- dashboardPage(
  title = "Redes Neuronales",
  
  
  dashboardHeader(
    title = tags$span(class = "title-text", "Redes Neuronales"),
    titleWidth = "100%"
  ),
  
  dashboardSidebar(
    
    #--------------------------------css-------------------------------------------
    tags$head(
      tags$style(HTML("
                                             @import url('https://fonts.googleapis.com/css2?family=Pinyon+Script&family=Playwrite+RO:wght@100..400&display=swap');
                                             @import url('https://fonts.googleapis.com/css2?family=Bad+Script&family=Charm:wght@400;700&family=Courgette&family=Gilda+Display&family=Pinyon+Script&family=Playwrite+RO:wght@100..400&display=swap');
                                             
                                              body {
                                              font-family: 'Gilda Display', serif;
                                              font-weight: 400;
                                              font-style: normal;
                                              font-size: 20px;
                                              width:100%
                                              
                                              }
                                             
                                              .title-text {
                                                font-size: 40px; /* Cambia este valor para ajustar el tamaño de la letra */
                                                font-family: 'Gilda Display', serif;
                                              font-weight: 400;
                                              font-style: normal;
                                              }
                                              .content-wrapper{
                                              width: 87%;
                                              background: linear-gradient(#5E85A7,8%, #EFF5FF);
                                              z-index: -20;
                                              }
                                               
                                              .content-wrapper, .right-side {
                                                background-color: #2B3544;
                                              }
                                              .main-header .logo {
                                                background-color: #2B3544 !important;
                                                color: white !important;
                                              }
                                              .main-header .navbar {
                                                background-color: #2B3544 !important;
                                                color: white !important;
                                              }
                                              
                                              .skin-blue .main-header .logo {
                                                background-color:#2B3544;
                                                color: white;
                                              }
                                              .skin-blue .main-header .navbar {
                                                background-color: #2B3544;
                                                color: white;
                                              }
                                              .integrantes-lista{
                                              color:black;
                                              line-height: 2;
                                              padding-left:30px;
                                              font-size:40px; 
                                              font-family: 'Charm', cursive;
                                              font-weight: 400;
                                              font-style: normal;
                                              }
                                              .title_insti{
                                              font-family: 'Gilda Display', serif;
                                              font-weight: 400; 
                                              text-decoration: underline; 
                                              font-size:28px;
                                              display:flex;
                                              justify-content:center;
                                              }
                                              .sidebar-toggle{
                                              display:none
                                              }
                                              
                                            "))
    ),
    
    #--------------------------------css-------------------------------------------
    sidebarMenu(
      class = "menu",
      tags$h3(class="title_insti","Instituto"),
      tags$h3(class="title_insti","Data Science"),
      
      
      menuItem("Integrantes", tabName = "integrantes", icon = icon("user-group")),
      menuItem("Marco Teórico", tabName = "MT", icon = icon("book")),
      menuItem("Introducción", tabName = "introduccion", icon = icon("table"),
               menuSubItem("Dataset", tabName = "info"),
               menuSubItem("Descripción", tabName = "dataset")),
      
      menuItem("EDA", tabName = "Analisis", icon = icon("chart-bar")),
      menuItem("Redes", tabName = "Modelos", icon = icon("cogs")),
      menuItem("Diagnostico", tabName = "Diagnostico", icon = icon("check"))
    )
  ),
  
  dashboardBody(class="body",
                tabItems(
                  tabItem(tabName = "integrantes",
                          column(4,
                                 div(tags$ul(
                                   tags$li(tags$a(class="integrantes-lista","Silvina Toffolo",href='https://www.linkedin.com/in/silvina-toffolo-b1b29927', target="_blank")),
                                   tags$li(tags$a(class="integrantes-lista","Leandro Consolini",href='https://www.linkedin.com/in/leandro-consolini', target="_blank")),
                                   tags$li(tags$a(class="integrantes-lista","Alejandra Becerra")),
                                   tags$li(tags$a(class="integrantes-lista","Nicolás Castillo",href='https://www.linkedin.com/in/nicol%C3%A1s-castillo-b3314b5b', target="_blank")),
                                   tags$li(tags$a(class="integrantes-lista","Marcos Acosta Ruiz",href='http://linkedin.com/in/marcos-acosta-ruiz', target="_blank")),
                                   tags$li(tags$a(class="integrantes-lista","Leonardo Goya",href='https://www.linkedin.com/in/leonardo-goya', target="_blank")),
                                   tags$li(tags$a(class="integrantes-lista","M Garcia Saggion",href = "https://www.linkedin.com/in/mariano-garcia-saggion-099a88268/",target="_blank")),
                                   tags$li(tags$a(class="integrantes-lista","Belén Chacón")),
                                   tags$li(tags$a(class="integrantes-lista","Julieta Arco",href='https://www.linkedin.com/in/julietaarcomolina/', target="_blank"))))),
                          column(8,
                                 tags$image(type="port/png", src="portada.jpg",style="width:500px;height:500px;position:relative;margin-left:320px; margin-top:50px;box-shadow:2px 4px 8px  #2C242E;border: solid black 2px")) # El div con la imagen de fondo
                          
                  ),
              
             
      
      tabItem(tabName = "MT",
              
                                  h3("Detalles de Teoría"),
                                  tags$h4("Las redes neuronales son un tipo de modelo utilizado en Machine Learning, un modelo computacional inspirado en el funcionamiento del cerebro humano."),
                                  tags$h4("Están compuestas por nodos interconectados llamados neuronas, organizados en capas. Cada neurona recibe una serie de entradas, las procesa mediante una función de activación, y produce una salida que se transmite a otras neuronas."),
                                  tags$h4("Cada neurona está conectada con otras a través de unos enlaces. En estos enlaces el valor de salida de la neurona anterior es multiplicado por un valor de peso."),
                                  tags$h4("Estos pesos en los enlaces pueden incrementar o inhibir el estado de activación de las neuronas adyacentes."),
                                  tags$h4("Estas capas procesan y transforman la información, permitiendo que el modelo aprenda patrones complejos a partir de los datos de entrada "),
                                  tags$h3("Tipos de Aprendizaje de las redes Neuronales"),
                                  tags$h4("Supervisado: Los datos están etiquetados con las respuestas deseadas."), 
                                  tags$h4("No Supervisado: Los datos no están etiquetados y el objeto es descubrir patrones o estructuras subyacentes de datos."),
                                  tags$h4("Por Refuerzo: Los datos están asociados con una señal de recompensa, y el sistema aprende a través de ensayo y error para maximizar la recompensa."),
                                  tags$h3("Estructura de una Red Neuronal"),
                                  tags$h4("Capa de entrada: Recibe los datos iniciales."),
                                  tags$h4("Capas ocultas: Procesan la información recibida, realizando cálculos complejos."),
                                  tags$h4("Capa de salida: Genera el resultado final del proceso."),
                                  tags$h4("Características del Dataset Para las Redes Neuronales"),
                                  tags$h4("Representatividad: Los datos deben ser representativos de la población que se quiere modelar o predecir"),
                                  tags$h4("Cantidad suficiente de Datos: Se necesita un conjunto de datos lo suficientemente grande para que el modelo pueda aprender patrones significativos."),
                                  tags$h4("Calidad de los Datos: Los datos deben ser precisos y confiables, sin errores significativos ni valores atípicos que puedan distorsionar el aprendizaje"),
                                  tags$h4("Variedad de características: Es beneficioso tener una variedad de características relevantes que capturen diferentes aspectos de los datos"),
                                  tags$h4("Balance (en clasificación): en problemas de clasificación es importantes que las clases estén balanceadas para evitar sesgos en el modelo hacia la clase mayoritaria."), 
                                  tags$h4("Preprocesamiento: Los datos deben ser preprocesados adecuadamente para eliminar el ruido, manejar valores faltantes y normalizar las características si es necesario")
                         
                         
              
      ),
      tabItem(tabName = "info",
              fluidRow(
                       tags$h2("Dataset de enfermedades cardiacas de la clinica Cleveland"),br(),
                       tags$h4("Los datos fueron recopilados por el Dr. Robert Detrano, de la Cleveland Clinic Foundation."),
                       tags$h4("La base de datos está compuesta por 14 campos y 303 registros"),br(),
                       tags$li(strong("age"),": edad del paciente"),
                       tags$li(strong("sex"),": Sexo del paciente (1 = masculino; 0 = femenino)"),
                       tags$li(strong("cp"),": Tipo de dolor en el pecho (Categórica con 4 tipos: Tipo 1: Angina típica; Tipo 2: Angina Atípica; Tipo 3: Dolor no anginal; Tipo 4: Asintomático)"),
                       tags$li(strong("trestbps"),": Presión arterial en reposo en mm Hg al ingreso al hospital (V. continua)"),
                       tags$li(strong("chol"),": Colesterol sérico en mg /dl (V. continua)"),
                       tags$li(strong("fbs"),": azúcar en sangre en ayunas > 120 mg/dl (categórica con 2 niveles: verdadero, falso)"),
                       tags$li(strong("restecg"),": Resultados electrocardiográficos en reposo (Categórica con 3 niveles: 0: Normal; 1: Tener anormalidad de onda ST-T (inversiones de onda T y / o elevación o Depresión de ST de > 005 mV); 2: Muestra hipertropía ventricular izquierda probable o definitiva"),
                       tags$li(strong("thalach"),": Frecuencia cardiaca máxima alcanzada (V. continua)"),
                       tags$li(strong("exang"),": Angina inducida por el ejercicio (Categórica con dos niveles: 1-Si, 0-No)"),
                       tags$li(strong("oldpeak"),": Depresión del ST inducida por el ejercicio en relación con el descanso (V. continua)"),
                       tags$li(strong("slope"),": La pendiente del segmento ST de ejercicio pico (categórica con 3 niveles: Valor 1: ascenso; Valor 2: plano ;Valor 3: descenso"),
                       tags$li(strong("ca"),": Numero de vasos principales (0-3) coloreados por fluoroscopia (Categórica con 4 niveles: 1-2-3-4)"),
                       tags$li(strong("thal"),": El estado del corazón según la prueba de Thallium (Categórica con 3 niveles: 1 = normal;2 = defecto fijo; 3 = defecto reversible)"),
                       tags$li(strong("num"),": variable objetivo que representa el diagnóstico de enfermedad cardíaca (estado de enfermedad angiográfica) en cualquier vaso principal Valor 0: < 50% de estrechamiento del diámetro Valor 1: > 50% de estrechamiento del diámetro"),
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
              navbarPage("",
              tabPanel("Modelos a evaluar",
                       fluidRow(column(6,
                                       radioButtons("capas", "Seleccione la cantidad de capas",
                                                    choiceNames = list("1","2"),
                                                    choiceValues = list(1,2)
                                                    ),
                                       numericInput("neuronas1", "Seleccione la cantidad de neuronas de la capa 1", min = 1, max = 10, value = 2),
                                       numericInput("neuronas2", "Seleccione la cantidad de neuronas de la capa 2", min = 1, max = 10, value = 3),
                                       actionButton("calcular", "Calcular Modelo")
                                       ),
                                column(6,
                                       tags$h3("MSE del modelo"),
                                       verbatimTextOutput("error"),
                                       div(plotOutput("plotmodel"))))),
              tabPanel("Grafico 3D",
                       tags$h3("Gráfico 3D Interactivo"),
                       plotlyOutput("plot3d"),
                       selectInput("ejeZ", 
                                   "Seleccionar variable para eje Z", 
                                   choices = c("MSE", "tp", "tn", "fp", "fn","costo"))
              ),
              
              
              
               
                       
              
              tabPanel("Matriz de confusión",
                       tags$h2("La mejor configuración encontrada es 2-4"),
                       plotlyOutput("confMatrixPlot")
              
              ))),
      
      tabItem(tabName = "Diagnostico",
              fluidRow(
                column(3,
                       numericInput("age", "Ingrese la edad", min = 1, max = 100, value = 30),       
                       radioButtons("sex", "Seleccione el género",
                                    choiceNames = list("Masculino","Femenino"),
                                    choiceValues = list(1,0)
                       ),
                       radioButtons("cp", "Seleccione el Tipo de dolor de pecho",
                                    choiceNames = list("Angina típica","Angina Atípica","Dolor no anginal","Asintomático"),
                                    choiceValues = list(1,2,3,4)
                       
                       ),
                       sliderInput("trestbps", label = "Seleccione la presión arterial en reposo en mm Hg", min = 80, max = 250, value = 100),
                       sliderInput("chol", label = "Seleccione el colesterol sérico en mg /dl", min = 100, max = 600, value = 300),
                ),
                column(3,
                       radioButtons("fbs", "¿azúcar en sangre en ayunas > 120 mg/dl?",
                                    choiceNames = list("Si","No"),
                                    choiceValues = list(1,0)
                       ),
                       radioButtons("restecg", "Resultados electrocardiográficos en reposo",
                                    choiceNames = list("Normal","Tener anormalidad de onda ST-T","Muestra hipertropía ventricular izquierda probable"),
                                    choiceValues = list(0,1,2)
                       ),
                       sliderInput("thalach", label = "Frecuencia cardiaca máxima alcanzada", min = 60, max = 300, value = 100),
                       radioButtons("exang", "¿Angina inducida por el ejercicio?",
                                    choiceNames = list("Si","No"),
                                    choiceValues = list(1,0)
                       ),
                       sliderInput("oldpeak", label = "Depresión del ST inducida por el ejercicio en relación con el descanso", min = 0, max = 15, value = 6)
                ),
                column(3,
                       radioButtons("slope", " La pendiente del segmento ST de ejercicio pico",
                                    choiceNames = list("ascenso","plano","descenso"),
                                    choiceValues = list(1,2,3)),
                       radioButtons("ca", "Numero de vasos principales coloreados por fluoroscopia",
                                    choiceNames = list("0","1","2","3","4"),
                                    choiceValues = list(0,1,2,3,4)),
                       radioButtons("thal", "Estado del corazón según la prueba de Thalliu",
                                    choiceNames = list("normal","defecto fijo","defecto reversible"),
                                    choiceValues = list(1,2,3)),
                       actionButton("predecir", "Predecir Diagnóstico")
                       
                       ),
                column(3,
                       tags$h2("Diagnostico"),
                       tableOutput("Diagnostico")
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
    
    grafico_dispersión <- ggplot(df, aes_string(x = input$graf_corr_x, y=input$graf_corr_y)) +
      geom_point(color = "skyblue") +
      labs(title =  paste(input$graf_corr_x,"vs",input$graf_corr_y), x = input$graf_corr_x, y = input$graf_corr_y) +
      theme_minimal()
    grafico_interactivo_dispersion<-ggplotly(grafico_dispersión)
    grafico_interactivo_dispersion
    
  })
  
  output$plot3d <- renderPlotly({
     
    grilla <- read.csv("grilla.csv")
    # Obtener el valor seleccionado del input
    ejeZ_seleccionado <- input$ejeZ
    # Crear gráfico 3D
    fig <- plot_ly(grilla, 
                   x = ~Primera_Capa, 
                   y = ~Segunda_Capa, 
                   z = as.formula(paste("~", ejeZ_seleccionado)), 
                   type = 'scatter3d', 
                   mode = 'markers',
                   marker = list(size = 5, 
                                 color = as.formula(paste("~", ejeZ_seleccionado)), 
                                 colorscale = 'Viridis',
                                 #modifique desde aca
                                 cmin=min(grilla[,ejeZ_seleccionado]),   # define el valor mínimo del rango para el color
                                 cmax=max(grilla[,ejeZ_seleccionado])    # define el valor máximo del rango para el color
                   ))
    fig <- fig %>% 
      layout(scene = list(
      xaxis= list(title= 'Primera Capa'),
      yaxis= list(title= 'Segunda Capa'),
      zaxis= list(title=toupper(ejeZ_seleccionado))),
      colorbar=list(
        title=ejeZ_seleccionado,
        titleside='right'),
      showlegend = F
      )
    #hasta aca
    #fig <- fig %>% 
     # layout(scene = list(
      #xaxis = list(title = 'Primera Capa'),
      #yaxis = list(title = 'Segunda Capa'),
      #zaxis = list(title= toupper(ejeZ_seleccionado))
    #))
    
    fig
  })
  
  
  
  #TabItemModelos----------------
  
  model.net <- reactiveVal(NULL)
  
  observeEvent(input$calcular, {
    set.seed(123)
    split <- sample.split(df_sc$target, SplitRatio = 0.7)
    train <- subset(df_sc, split == TRUE)
    test <- subset(df_sc, split == FALSE)
    
    if (input$capas == 1) {
      model.net(neuralnet(
        formula = target ~ ., # todas las variables
        data = train,
        hidden = c(input$neuronas1),
        stepmax = 1e+05,
        linear.output = FALSE
      ))
    } else {
      model.net(neuralnet(
        formula = target ~ ., # todas las variables
        data = train,
        hidden = c(input$neuronas1, input$neuronas2),
        stepmax = 1e+05,
        linear.output = FALSE
      ))
    }
  
  

    output$plotmodel <- renderPlot({
      req(model.net())
      
      plot(model.net(), 
           col.hidden = 'darkgreen',     
           col.hidden.synapse = 'darkgreen',
           show.weights = FALSE,
           information = FALSE,
           fill = 'lightblue')  # Mostrar el gráfico del modelo
    
   }, res = 96) # Establecer la resolución del gráfico
    
  })
  
  #Output para mostrar el MSE
  output$error <- renderPrint({
  req(model.net())
    set.seed(123)
    split <- sample.split(df_sc$target, SplitRatio = 0.7)
    train <- subset(df_sc, split == TRUE)
    test <- subset(df_sc, split == FALSE)  
  
  predictions <- neuralnet::compute(model.net(), test)$net.result
  predictions <- round(predictions)
  mse <- sum((predictions - test$target)^2) / nrow(test)
  cat(mse)
    
  })
  
  ggplot(data = conf_matrix_df, aes(x = Prediction, y = Reference, fill = Freq)) +
    geom_tile(color = "white") + 
    geom_text(aes(label = Freq), size = 5) + 
    scale_fill_gradient(low = "white", high = "blue") +
    theme_minimal() +
    labs(title = "Matriz de Confusión",
         x = "Estimado",
         y = "Observado") +
    theme(
      plot.title = element_text(size = 20, face = "bold"),
      axis.title = element_text(size = 15),
      axis.text = element_text(size = 12)
    )
  
  
  #Output para mostrar la matriz de confusión

  output$confMatrixPlot <- renderPlotly({
    plotmat<-ggplot(data = conf_matrix_df, aes(x = estimado, y = observado, fill = Freq)) +
      geom_tile(color = "white") + 
      geom_text(aes(label = Freq), size = 5) + 
      scale_fill_gradient(low = "white", high = "blue") +
      theme_minimal() +
      labs(title = "Matriz de Confusión",
           x = "Estimado",
           y = "Observado") +
      theme(
        plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12)
      )
    matriz_interactiva<-ggplotly(plotmat)
    matriz_interactiva
  })
  
  #Output Diagnostico
  prediction <- reactiveVal(NULL)
  
  observeEvent(input$predecir, {
    split <- sample.split(df_sc$target, SplitRatio = 0.7)
    train <- subset(df_sc, split == TRUE)
    test <- subset(df_sc, split == FALSE)  
    
    age <-  as.integer(input$age)
    sex <-   as.integer(input$sex) 
    cp <-   as.integer(input$cp) 
    trestbps <- as.integer(input$trestbps) 
    chol <- as.integer(input$chol)
    fbs <-  as.integer(input$fbs)
    restecg <- as.integer(input$restecg)
    thalach <- as.integer(input$thalach)
    exang <-  as.integer(input$exang)
    oldpeak <- as.integer(input$oldpeak)
    slope <-  as.integer(input$slope)
    ca <- as.integer(input$ca)
    thal <- as.integer(input$thal)
    
    vect <- c(age, sex, cp, trestbps, chol, fbs, restecg, thalach, exang, oldpeak, slope, ca, thal)
    df_eval <- data.frame(t(vect))
    colnames(df_eval) <- c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope", "ca", "thal")
    
    pred <- isolate(neuralnet::compute(model.net_bm, df_eval)$net.result)
    pred <- round(pred)
    prediction(pred)
    
    output$Diagnostico <- renderTable({
      data.frame(Diagnostico = prediction())
    })
  })
  
  
  
}
  
  


shinyApp(ui, server)
