library(shiny)
library(shinythemes)
library(shinydashboard)
library(datasets)
library(tidyverse)
library(tidyr)
library(DT)
library(cluster)
library(factoextra)
library(NbClust)
library(dplyr)
library(ggplot2)
library(plotly)
library(sf)
library(spData)
library(corrplot)
library(conflicted)

conflict_prefer("filter", "plotly")
conflict_prefer("dataTableOutput", "DT")
conflict_prefer("renderDataTable", "DT")
conflict_prefer("box", "shinydashboard")

#cargamos los mapas de colores para las graficas

mapa_colores_red  <- c('#fef0d9','#fdcc8a','#fc8d59','#e34a33','#b30000')
mapa_colores_blue <- c('#f0f9e8','#bae4bc','#7bccc4','#43a2ca','#0868ac')
mapa_colores2     <- c('#e34a33','#43a2ca')
mapa_colores4     <- c('#e34a33','#b30000','#43a2ca','#0868ac')
mapa_colores4_red <- c('#fdcc8a','#fc8d59','#e34a33','#b30000')
mapa_colores4_blue<- c('#bae4bc','#7bccc4','#43a2ca','#0868ac')
mapa_colores8     <- c('#fdcc8a','#fc8d59','#e34a33','#b30000','#bae4bc','#7bccc4','#43a2ca','#0868ac')



#Carga de los datasets a utilizar, el primero corresponde a la información geoespacial para poder mostrar el mapa
datasp_us<-us_states
data<-USArrests

#Calculo las medias y Desvios porque los voy a necesitar para desnormalizar
medias<-apply(data[,c(1,2,4)], 2, mean)
desvios<-apply(data[,c(1,2,4)], 2, sd)

#normalizar las variables
data_normalizada <- scale(data)
data_normalizada <-as.data.frame(data_normalizada)

# Realizar k-means con 2 grupos
set.seed(123)
kmeans_result <- kmeans(data_normalizada, centers = 2)

# Obtener los centroides
centroides <- kmeans_result$centers[,c(1,2,4)]

#calcular la matriz de distacias
m.distancia <- get_dist(data_normalizada, method = "euclidean") 

# # Crear el gráfico de distancias
# fviz <- fviz_dist(m.distancia, 
#                   gradient = list(low = "#7bccc4", mid = "#f0f9e8", high = "#0868ac")) +
#   theme_void()   # Eliminar los nombres de los ejes
# 
# 
# # Convertir a gráfico interactivo y ajustar tamaño
# fviz_interactivo<-ggplotly(fviz, tooltip = "text") # Aumentar el tamaño del texto emergente



                                        ####################
                                        ####################
                                        ####DASHBOARD#######
                                        ####################
                                        ####################

ui <- navbarPage(
  title = div(
    "Clustering",
    style = "color: #2C242E; font-weight: bold; font-size: 30px; font-family: Arial, sans-serif;"
  ),
  
#---------------------------------------  
  # Defino el CSS
  tags$head(
    tags$style(HTML("
      .navbar-default .navbar-brand {
        color: #000;
        font-weight: 900;
        font-size: 28px;
        text-shadow: 2px 4px 8px grey;
      }
      .container-panel {
        color: #646165;
      }
      .navbar-nav > li > a {
        color: #646165 !important;
      }
      .navbar-nav > li > a:hover {
        background-color: #f0ad4e !important;
        color: black !important;
      }
      .background-div {
        background-image: url('logoinst.jpg');
        background-size: cover;
        background-repeat: no-repeat;
        background-position: center;
        height: calc(100vh - 50px); /* Ajusta la altura restando el padding */
        opacity:1;
        width:100%;
        padding-top: 50px;
        position: absolute;
        top: 50px;
        left:0;
        
        z-index: -1; /* Para asegurarte de que estC) detrC!s del contenido */
      }
      .background-div2 {
        background: rgb(156,154,154);
        background: linear-gradient(138deg, rgba(156,154,154,0.6727065826330532) 9%, rgba(250,250,252,0.7875525210084033) 79%);
        background-size: cover;
        background-repeat: no-repeat;
        background-position: center;
        height: calc(100vh - 50px); /* Ajusta la altura restando el padding */
        width: 100%;
        padding-top: 50px;
        position: absolute;
        top: 50px;
        left:0;
        z-index: -10; /* Para asegurarte de que estC) detrC!s del contenido *
      
      .content {
        position: relative;
        z-index: 1; /* Asegura que el contenido estC) por encima de la imagen de fondo */
        padding-top: 50px; /* Asegura que el contenido tambiC)n se baje */
      }
      
      .cont_UsArrest {
        background-image: url('fondo.png');
        background-size: cover;
        background-repeat: no-repeat;
        background-position: center;
        opacity: 0.15;
        height: calc(100vh - 50px); /* Ajusta la altura restando el padding */
        width:48%;
        padding-top: 50px;
        position: absolute;
        top: 50px;
        left:450px;
        
        z-index: -1; /* Para asegurarte de que estC) detrC!s del contenido */
      }
      
      .integrantes-lista{
      font-size: 24px;
      margin-top: 20px;
      margin-bottom: 10px;
      font-family: inherit;
      font-weight: 500;
      line-height: 2.5;
      color: white;
      }
      
      
    "))
  ),
#-----------------------------------------------------------  
  
  
  #defino solapas(panel)
  
  tabPanel("INTEGRANTES",icon = icon("user-group"),
           fluidPage(class = "integrantes-lista",
                     tags$h1("Diplomatura en Ciencia de datos con R+Python",style = "font-weight: bold; font-size: 30px; font-family: Arial, sans-serif; text-decoration: underline; line-height: 2;"),
                       div(tags$ul(
                         tags$li(tags$a(class="integrantes-lista","Silvina Toffolo",href='https://www.linkedin.com/in/silvina-toffolo-b1b29927', target="_blank")),
                         tags$li(tags$a(class="integrantes-lista","Leandro Consolini",href='https://www.linkedin.com/in/leandro-consolini', target="_blank")),
                         tags$li(tags$a(class="integrantes-lista","Sergio Basta",href='https://www.linkedin.com/in/sergio-basta-19038411/', target="_blank")),
                         tags$li(tags$a(class="integrantes-lista","Alejandra Becerra")),
                         tags$li(tags$a(class="integrantes-lista","Nicolás Castillo",href='https://www.linkedin.com/in/nicol%C3%A1s-castillo-b3314b5b', target="_blank")),
                         tags$li(tags$a(class="integrantes-lista","Marcos Acosta Ruiz",href='http://linkedin.com/in/marcos-acosta-ruiz', target="_blank")),
                         tags$li(tags$a(class="integrantes-lista","Leonardo Goya",href='https://www.linkedin.com/in/leonardo-goya', target="_blank")),
                         tags$li(tags$a(class="integrantes-lista","Mariano Garcia Saggion")),
                         tags$li(tags$a(class="integrantes-lista","Julieta Arco",href='https://www.linkedin.com/in/julietaarcomolina/', target="_blank"))
                                 
                         ))
           ),
             div(class = "background-div") # El div con la imagen de fondo
           
  ),
  
  
  tabPanel("MARCO TEORICO",icon = icon("book"),
           
           
           tags$h3("El término clustering hace referencia a un amplio abanico de técnicas no supervisadas cuya finalidad es encontrar patrones o grupos (clusters) dentro de un conjunto de observaciones. "),
           tags$h3("Las particiones se establecen de forma que, las observaciones que están dentro de un mismo grupo, son similares entre ellas."),
           tags$h3("Se trata de un método no supervisado, ya que el proceso ignora la variable respuesta que indica a que grupo pertenece realmente cada observación (si es que existe tal variable). Esta característica diferencia al clustering de las técnicas supervisadas, que emplean un set de entrenamiento en el que se conoce la verdadera clasificación."),
           tags$h2("Dendograma"),
           tags$h3("En un dendograma, cada observación forma una terminación individual conocida como hoja o leaf del árbol. A medida que se asciende por la estructura, pares de hojas se fusionan formando las primeras ramas. Estas uniones (nodos) se corresponden con los pares de observaciones más similares. También ocurre que ramas se fusionan con otras ramas o con hojas. Cuanto más ocurre una fusión, mayor es la similitud."),
           tags$image(type="Dbscan/png", src="dendrograma.webp",style="display:flex; justify-content:center;width:300px;height:300px; "),
           div(class = "background-div2") 
  
           
  ),

navbarMenu("INTRODUCCIÓN",icon = icon("network-wired"),
           
           tabPanel("USARREST", 
                    tags$h1(strong("UsArrests.csv")),
                    
                    tags$h2("Este archivo contiene estadísticas sobre los arrestos en cada uno de los 50 estados de EE.UU. 
           en el año 1973. Los datos fueron recopilados por el FBI y proporcionan una vision general de la criminalidad en ese año."),br(),
                    tags$h2("El conjunto de datos tiene las siguientes columnas:"),
                    tags$h2(strong("State:"), "El nombre del estado."),
                    tags$h2(strong("Murder:"),"La tasa de asesinatos (número de asesinatos por cada 100,000 habitantes)."),
                    tags$h2(strong("Assault:"),"La tasa de asaltos (número de asaltos por cada 100,000 habitantes)."),
                    tags$h2(strong("UrbanPop:"),"El porcentaje de la poblacióon que vive en áreas urbanas."),
                    tags$h2(strong("Rape:"),"La tasa de violaciones (número de violaciones por cada 100,000 habitantes)."),
                    div(class = "cont_UsArrest") # El div con la imagen de fondo
             
             
             
           ),
           
           
           tabPanel("DATA SET", 
                    div(dataTableOutput("datos"),style="font-size:20px;")
                    
                    
           ),
           tabPanel("DATOS ESTADISTICOS",
                    fluidRow(
                    h2("Estructura de los datos"),
                    div(verbatimTextOutput("data_structure"),style="width:30%; margin:auto;box-shadow:2px 4px 8px  #2C242E"),
                    h2("Resumen Estadístico"),
                    div(verbatimTextOutput("data_summary"),style="width:35%; margin:auto;margin-bottom:60px; box-shadow:2px 4px 8px  #2C242E")),
                    fluidRow(
                      tags$h2("Análisis de Datos nulos"),
                      div(verbatimTextOutput("ceros"),style="width:30%; margin:auto;box-shadow:2px 4px 8px  #2C242E"),  
                    ),
                    div(class = "background-div2") 
           )
                    
),


navbarMenu("ANALISIS",icon = icon("lightbulb"),
             
           
    tabPanel("NORMALIZACION", 
            fluidRow(
            div(dataTableOutput("datos_normalizados"),style="font-size:16px;"),
            div(class = "background-div2")),
    ),
    
    tabPanel("MATRIZ DE DISTANCIA", 
            fluidRow(
            column(2,tags$h2("Matriz de Distancia"))),
            p(HTML('<span style= "font-size: 20px; background-color: lightgray; padding: 5px; font-style: italic; border: 2px solid black; display: block;">
                ```{r}<br>
                fviz <- fviz_dist(m.distancia, 
                  gradient = list(low = "#7bccc4", mid = "#f0f9e8", high = "#0868ac")) +theme_void()<br>
                           ``` </span>'
                          
                )),
                   
            div(plotlyOutput('fviz_interactivo', width = "800px", height = "600px"),style="margin-left:450px;"),
            div(class = "background-div2") 
             
    ),
    tabPanel("CLUSTERS", 
             tags$h3("ESTIMACION DEL NUMERO DE CLUSTERS"),
             fluidRow(
               column(4,
                      plotlyOutput('grafico_codo')
               ),
               column(4,
                      plotlyOutput('grafico_silueta')
               ),
               column(4,
                      plotlyOutput('grafico_brecha'))),
             tags$h4(""),
             fluidRow(
               column(4),
               column(4,
                      plotlyOutput('grafico2_silueta')
               ),
             ),
             div(class = "background-div2")
             
    )),

            
         
navbarMenu("MODELOS",icon = icon("chart-simple"), 
        
        tabPanel("K-MEANS", 
                 tags$h3("STR KMEANS"),
                 div(verbatimTextOutput("kmeans"),style="width:100%; margin:auto;margin-bottom:60px; box-shadow:2px 4px 8px  #2C242E"),
                 div(class = "background-div2")
                 ),
                   
        tabPanel("CLUSTERS", 
                 tags$h3("GRAFICA DE CLUSTERS CON KMEANS"),
                 fluidRow(
                   tabBox( width = 8, height = "800px",
                           tabPanel("Grafico 1",
                                    plotlyOutput('grafico_cluster1')
                           ),
                           tabPanel("Grafico 2",
                                    plotlyOutput('grafico_cluster2')
                           ),
                           tabPanel("Grafico 3",
                                    plotlyOutput('grafico_cluster3')
                           )
                   )
                 )
                 ,div(class = "background-div2")
        ),

        tabPanel("GRAFICO 3D",
                 fluidRow(
                   h2("Grafico Interactivo 3D"),  
                   div(plotlyOutput("grafico3D", width = "100%",height="100%"),style="margin-left:350px;box-shadow:4px 6px 15px  #2C242E;width:800px;height:600px"),
                   div(class = "background-div2")) 
        ),  
        
          tabPanel("DENDOGRAMA", 
                 tags$h3("Dendograma"),
                 div(plotOutput('dendograma', width = "800px", height = "600px"),style="margin-left:350px;"),
                 div(class = "background-div2")
                 )
         
),
tabPanel("CONCLUSIONES",icon = icon("check-double"), 
         tags$h3("Estudio del modelo elegido"),
         p(
           HTML( '<span style= "font-size: 14px"> 
             Luego de concluir que los grupos fueron dividos en estados con mayores indices en asaltos, asesinatos y violaciones, podríamos etiquetar que un Grupo corresponde a los estados más seguros y el otro a los menos seguros. A partir de dicha conclusión nos proponemos calcular la <strong>factibilidad</strong> de pasar de un grupo (menos seguro) al grupo alternativo (más seguro), en base a un <strong>presupuesto determinado</strong> y <strong>costos de implementación</strong> de políticas para reducir los distintos índices.<br><br>
             </span>'
           )
         ),
         #Agrego caja para gráfico
         dashboardBody(
           fluidRow(
             plotlyOutput("mapaUSA", height = 300)
           ),
           #Agrego caja para controles de input de datos (Estado, Costo Politica Asaltos, Asesinatos, Violaciones)
           fluidRow(
             column(3,selectInput("estado","Seleccione el Estado",choices="" )),
             column(3,sliderInput("pol_murder","Costo Politica Murder (kUSD)",min =0 ,max=100, step= 10,value=20)),
             column(3,sliderInput("pol_assault","Costo Politica Assault(kUSD)",min =0 ,max=100,step=10, value=20)),
             column(3,sliderInput("pol_rape","Costo Politica Rape (kUSD)",min =0 ,max=100, step=10,value=20))
           ),
           #Agrego caja para output de los porcentajes a mejorar, costo de implementación, diferencia respecto al presupuesto         
           fluidRow(
             #textOutput("debug"),
             h4("Porcentajes que se deben mejorar y Costo de la implementación de las políticas"),
             valueBoxOutput("Perc_Murder"),
             valueBoxOutput("Perc_Assault"),
             valueBoxOutput("Perc_Rape")
           ),
           fluidRow(
             valueBoxOutput("Costo_Cambio")
           )
         )
         #         ,div(class = "background-div2")
)
)



# Define server logic required to draw a histogram
server <- function(input, output) {

  #outputs de INTRODUCCION  
  output$datos <- DT::renderDataTable(data)
  
  output$data_summary <- renderPrint(summary(data))
  output$data_structure <- renderPrint(str(data))
  
  output$grafico3D <- renderPlotly({
    fig <- plot_ly(data = data_normalizada,
                   x = ~Murder,
                   y = ~Assault,
                   z = ~Rape,
                   size = 20,
                   color = kmeans_result$cluster,
                   type = 'scatter3d',
                   mode = 'markers',
                   showlegend = T,
                   name = "Data"
    )

    fig <- fig %>% add_markers(x=kmeans_result$centers[1,1],
                                   y=kmeans_result$centers[1,2],
                                   z=kmeans_result$centers[1,3],
                                   marker = list(
                                     color = "darkblue",
                                     size = 10
                                   ),
                                   text = "Center Cluster 1",
                                   name = "Center Cluster 1"
                        )
    fig <- fig %>% add_markers(x=kmeans_result$centers[2,1],
                                   y=kmeans_result$centers[2,2],
                                   z=kmeans_result$centers[2,3],
                                   marker = list(
                                     color = "red",
                                     size = 10
                                   ),
                                   text = "Center Cluster 2",
                                   name = "Center Cluster 2"
                  )
  })

  #outputs de ANALISIS    
  output$datos_normalizados <- DT::renderDataTable(data_normalizada)
  
  output$ceros<-renderPrint(sapply(data, function(x) sum(is.na(x))))
 
  output$fviz_interactivo<-renderPlotly({
    
      # Crear el gráfico de distancias
      fviz <- fviz_dist(m.distancia, 
                        gradient = list(low = "#7bccc4", mid = "#f0f9e8", high = "#0868ac")) +
        theme_void() +  # Eliminar los nombres de los ejes
        theme(plot.margin = margin(0, 0, 0, 0)) # Ajustar el margen
      
      # Convertir a gráfico interactivo y ajustar tamaño
      plot_interactivo <- ggplotly(fviz)
      
    
    })

    output$grafico_codo<-renderPlotly({
    g5<-fviz_nbclust(data_normalizada, 
                     kmeans, 
                     method = "wss",
                     linecolor = "#b30000")
    ggplotly(g5)
  })
  
  output$grafico_silueta<-renderPlotly({
    g6<-fviz_nbclust(data_normalizada, 
                     kmeans, 
                     method = "silhouette",
                     linecolor = "#0868ac")
    ggplotly(g6)
  })
  
  output$grafico_brecha<-renderPlotly({
    g7<-fviz_nbclust(data_normalizada, 
                     kmeans, 
                     method = "gap_stat",
                     linecolor = "#e34a33")
    ggplotly(g7)
  })

  output$grafico2_silueta<-renderPlotly({
    silueta <- silhouette(kmeans_result$cluster, dist(m.distancia))
    s <- fviz_silhouette(silueta)
    ggplotly(s)
  })
  
  
  #outputs de MODELOS  
  output$kmeans <- renderPrint({
    kmeans_result
  })

  output$grafico_cluster1<-renderPlotly({
    g1<-fviz_cluster(kmeans_result, 
                 data = data,
                 palette=mapa_colores2,
                 ggtheme = theme_classic()) 
    ggplotly(g1)
  })
  
  output$grafico_cluster2<-renderPlotly({
    g2<-fviz_cluster(kmeans_result, 
                     data = data, 
                     ellipse.type = "euclid", 
                     palette=mapa_colores2,
                     ggtheme = theme_classic(),
                     repel = FALSE,
                     star.plot = TRUE)
    ggplotly(g2)
  })
  
  output$grafico_cluster3<-renderPlotly({
    g3<-fviz_cluster(kmeans_result, 
                     data = data, 
                     ellipse.type = "norm",
                     palette=mapa_colores2,
                     ggtheme = theme_classic())
    ggplotly(g3)
  })

  output$dendograma<-renderPlot({
    # Clustering usando "hclust"
    #Seteamos una semilla para que el resultado sea reproducible en distintas ejecuciones
    set.seed=123 
    dist_matrix_df <- dist(data_normalizada)
    hclust_df <- hclust(dist_matrix_df, method = "complete")
    
    # Graficamos el dendrograma
    plot(hclust_df, main = "Dendrograma de Agrupamiento Jerárquico", xlab = "", sub = "", cex = 0.9)
    
    # Diferenciamos los clusters con rectangulos 
    rect.hclust(hclust_df, k = 2, border = "red")
    })
  
  #reactive expressions Conclusiones
  data2 <- reactive({
    data$Cluster <- kmeans_result$cluster
    data <- data %>% mutate("NAME"=rownames(data))
    merge(data,datasp_us,by="NAME")
  })
  
  data2_normalizada <- reactive({
    data_normalizada$Cluster <- kmeans_result$cluster
    data_normalizada <- data_normalizada %>% mutate("NAME"=rownames(data_normalizada))
  })

  estados_peligrosos <-reactive({
    data2() %>% filter(Cluster==1) %>% select (NAME)
  })

  estado_seleccionado <-reactive({
    data2() %>% filter(NAME==input$estado) %>% select (Murder, Assault, Rape)
  })  

  estado_selec_normalizado <-reactive({
    data2_normalizada() %>% filter(NAME==input$estado) %>% select (Murder, Assault, Rape)
  })  
  
  nuevo_punto_porc <- reactive({
    centroide_objetivo <- centroides[2, ]
    # Calcular el vector de cambio necesario para mover la observación al centro del grupo objetivo
    # Aumentar/Disminuir la magnitud del cambio para asegurar que la nueva observación esté más alejada/cercana del centroide objetivo
    # Un Factor = 1 iguala al centroide, entre (0 y 1): se va acercando al centroide, > 1: lo voy alejando cuando más grande sea el valor
    factor<-0.75
    obs <- estado_selec_normalizado()
    nueva_obs <- obs + (centroide_objetivo - obs)*factor

    #Desnormalizo el nuevo punto
    nueva_obs_desnorm <- nueva_obs * desvios + medias

    #Este codigo lo comento por ahora, porque es para validar que cambio de grupo
    #centroide_objetivo_desnorm <- centroide_objetivo * desvios + medias

    # Calcular la distancia de la nueva observación a los centroides
    #distancias_nueva <- apply(centroides, 1, function(centroide) {
    #  sqrt(sum((nueva_obs - centroide)^2))
    #})

    #Validar_Grupo_Clasificado <- names(distancias_nueva)[which.min(distancias_nueva)]
    #print(distancias_nueva)

    porcentajes<- (estado_seleccionado() - nueva_obs_desnorm) /estado_seleccionado()
  })
  
  valoracion <- reactive({
    valoracion<-c(input$pol_murder,input$pol_assault,input$pol_rape)
    calc_valor <-nuevo_punto_porc() * valoracion * rep(100,3)
    presupuesto_necesario<-sum(calc_valor)    
  })

  observeEvent(input$estado, once = TRUE, {
    updateSelectInput(inputId = "estado", choices=estados_peligrosos()) 
  }
  )
  #output$debug <- renderText(paste0(nuevo_punto_porc()," "))
  
  #Outputs de las Conclusiones
  output$mapaUSA <- renderPlotly({
    ggsf<- ggplot(data2()) +
      geom_sf(aes(geometry = geometry, fill = Cluster, text = paste(NAME, "<br>Asesinatos:",round(Murder,2),"<br>Asaltos:",round(Assault,2),"<br>Violaciones:",round(Rape,2)))) +
      scale_fill_gradient(low = "red", high = "green")
    ggplotly(ggsf,tooltip = "text")
  })
  
  
  output$Perc_Murder <- renderValueBox({
    valueBox(
      paste0(round(nuevo_punto_porc()[1]*100,2)), "Murder", icon = icon("percent"), color = "purple"
    )
  })
  
  output$Perc_Assault <- renderValueBox({
    valueBox(
      paste0(round(nuevo_punto_porc()[2]*100,2)),"Assault", icon = icon("percent"), color = "blue"
    )
  })
  
  output$Perc_Rape <- renderValueBox({
    valueBox(
      paste0(round(nuevo_punto_porc()[3]*100,2)), "Rape", icon = icon("percent"), color = "orange"
    )
  })
  
  output$Costo_Cambio <- renderValueBox({
    valueBox(
      paste0("$ ",prettyNum(round(valoracion()*1000,0), big.mark = "," )) ,"Costo Implementación", icon = icon("hand-holding-dollar"), color = "aqua"
    )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

