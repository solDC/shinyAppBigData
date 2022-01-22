############################################################################ 
## Authors
# AXIEL PAEZ CEBALLOS
# MARIA SOL DOMINGUEZ CARNUCCIO
# RODRIGO ARIAS CAPEL


############################################################################ 
## Set directory
setwd("~/Desktop/BIGDATA/PracticaShiny")

# runApp("App1") #el script está guardado en /Tutorial/App-1/App.r
# runApp("App-1", display.mode = "showcase") para ver el código en la ventana de la app

############################################################################ 
## Install or load package

pkg = c("tidyverse","dplyr","ggplot2","readxl","maps", "mapproj", "shiny","cluster","Rtsne")

## If not installed, install and load all packages
package.check <- lapply(
  pkg,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

############################################################################ 
## Read the data and load helpers

#First data set with a smaller set of columns
#data <- read.csv("telco.csv", header=TRUE, stringsAsFactors = TRUE)
#glimpse(data)

data <- read_excel("telcoChurn/data/Telco_customer_churn.xlsx")

## Add helpers
#source("telcoChurn/helpers.r")

############################################################################ 
# check out and prepare the data

# Drop space in colnames
colnames(data) <- gsub(" ","",colnames(data))

#glimpse(data)

#Transform all character variables into factors
data[sapply(data, is.character)] <- lapply(data[sapply(data,is.character)], as.factor)

#Undo for LatLong 
data$LatLong <- as.character(data$LatLong)

#Churn Value and zipcode are doubles but really are a factor
data$ChurnValue <- as.factor(data$ChurnValue) #same as churn label
data$ZipCode <- as.factor(data$ZipCode)

# Drop variables that we don't need
dropable <- c("Count","CustomerID","Country","State") # See later which can be dropped
data <- (data[, !(names(data) %in% dropable)])

#20 possible chrurn reasons
#levels(data$ChurnReason)

# Filter variables names for scatterplots inputs
scatterPlotNumeric <- names(data %>% select(TenureMonths,MonthlyCharges,TotalCharges,ChurnScore,CLTV))
scatterPlotFactor <- names(data %>% select_if(is.factor) %>% 
                             select(-contains("Churn")) %>% 
                             select(-contains("City")) %>% 
                             select(-contains("ZipCode")) 
                           )

### PAM
# Prep data: drop variables that have repetead data 
dropablePAM <- c("LatLong","Latitude","Longitude","ZipCode","ChurnValue") # See later which can be dropped
dataPAM <- (data[, !(names(data) %in% dropablePAM)])

# TODO SERVER SIDE
#Calculamos la distancia de Gower con la función daisy del paquete cluster
#Usar la distancia de gower es adecuado cuando tenemos variables categóricas
gower_dist <- daisy(dataPAM, metric = c("gower")) # --> INDICAR CUALES SON LOS CAMPOS QUE ENTRAN EN EL PAM
gower_mat <- as.matrix(gower_dist)

#Imprimir los más parecidos en función de la utilidad
similar <- dataPAM[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]), arr.ind = TRUE)[1, ], ]
#view(similar)

#Imprimir los más distintos en función de la utilidad
disimilar <- dataPAM[which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]), arr.ind = TRUE)[1, ], ]
#view(disimilar)

#Usamos el coeficiente de silhouette para identificar el número de cluesteres óptimos (suele ser entre 2 y 8)
sil_width <- c(NA)
for(i in 2:8){  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}

# Para ver gráficamente cuál sería el número de clusteres óptimo
# plot(1:8, sil_width,
#      xlab = "Number of clusters",
#      ylab = "Silhouette Width")
# lines(1:8, sil_width)

#Podemos definir el número de clusteres en 2 (óptimo) o 3 porque tenemos los valores más alto de silhouette width. 
#Va bajando para k >= 4.
k_PAM_Recomended <- which.max(sil_width) #Este va a ser el número de clusteres recomendado --> PINTAR SUGERENCIA EN TEXTO?

k <- 2
pam_fit <- pam(gower_dist, diss = TRUE, k)
pam_results <- dataPAM %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary

# names(pam_fit) #No voy a pintarlo

#para ver info de los clusteres
pam_fit$clusinfo

#para ver el índice de los medoides en el dataset 
# pam_fit$medoids #--> PODRIA PINTARLOS DIFERENTE
# dataPAM[pam_fit$medoids,]--> PINTAR LA TABLA PARA DAR PISTAS EN SCATTERPLOT

# reduzco dimensionalidad para graficar en dos dimensiones, uso t-sne
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster)) +
  labs(title = "Resultados clustering PAM")



                       
# Define UI ---
ui <- fluidPage(
  
  titlePanel('Telco Churn Analysis'),
  br(),
  h3("1- Are there any groups of customers that are churning?"),
  
  sidebarPanel(
    
    #PAM
    h2("1.1- PAM algorithm to see clusters"),
    #div("The optimal number of clusters is: ",k_PAM_Recomended),
    br(),
    radioButtons(inputId="PAMradio", label = h3("Select the number of clusters"),
                 choices = list("2 clusters" = 1, "3 clusters" = 2, "4 clusters" = 3, 
                                "5 clusters" = 4, "6 clusters" = 5, "7 clusters" = 6, "8 clusters" = 7))), 
    
    checkboxGroupInput(inputId="PAMcheckGroup", label = h4("Select which variables do you want to include in the clustering algorithm"), 
                       choices = list("City" = 1, "Gender" = 2, "Senior Citizen" = 3, "Partner"= 4,
                                      "Dependents" = 5, "TenureMonths" = 6, "PhoneService" = 7, "MultipleLines"= 8)),
    
    
    #SCATTERPLOT
    h2("1.2- Scatterplot to see the correlation between variables"),
    # Scatterplot Inputs
    selectInput(inputId = 'scatterPlot_x', label = 'X Variable', choices = scatterPlotNumeric), #, selected = "TenureMonths"
    selectInput(inputId = 'scatterPlot_y', label = 'Y Variable', choices = scatterPlotNumeric), #, selected = "TotalCharges"
    selectInput(inputId = 'scatterPlot_color_encoding', 'Select a categorical variable for color encoding',
                choices = scatterPlotFactor, selected = "Contract")
  ),
  
  mainPanel(
    plotOutput(outputId = 'scatterPlot'),
    plotOutput(outputId = "pamPlot")
  )
)

# Define server logic ---
server <- function(input, output) {
  
  #####
  # PAM
  #####
  output$pamPlot <- renderPlot({
    #Calculamos la distancia de Gower con la función daisy del paquete cluster
    #Usar la distancia de gower es adecuado cuando tenemos variables categóricas
    gower_dist <- daisy(dataPAM, metric = c("gower")) # --> INDICAR CUALES SON LOS CAMPOS QUE ENTRAN EN EL PAM
    gower_mat <- as.matrix(gower_dist)
    
    #Imprimir los más parecidos en función de la utilidad
    similar <- dataPAM[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]), arr.ind = TRUE)[1, ], ]
    #view(similar)
    
    #Imprimir los más distintos en función de la utilidad
    disimilar <- dataPAM[which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]), arr.ind = TRUE)[1, ], ]
    #view(disimilar)
    
    #Usamos el coeficiente de silhouette para identificar el número de cluesteres óptimos (suele ser entre 2 y 8)
    sil_width <- c(NA)
    for(i in 2:8){  
      pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
      sil_width[i] <- pam_fit$silinfo$avg.width  
    }
    
    # Para ver gráficamente cuál sería el número de clusteres óptimo
    # plot(1:8, sil_width,
    #      xlab = "Number of clusters",
    #      ylab = "Silhouette Width")
    # lines(1:8, sil_width)
    
    #Podemos definir el número de clusteres en 2 (óptimo) o 3 porque tenemos los valores más alto de silhouette width. 
    #Va bajando para k >= 4.
    k_PAM_Recomended <- which.max(sil_width) #Este va a ser el número de clusteres recomendado --> PINTAR SUGERENCIA EN TEXTO?
    
    k <- 2
    pam_fit <- pam(gower_dist, diss = TRUE, k)
    pam_results <- dataPAM %>%
      mutate(cluster = pam_fit$clustering) %>%
      group_by(cluster) %>%
      do(the_summary = summary(.))
    pam_results$the_summary
    
    # names(pam_fit) #No voy a pintarlo
    
    #para ver info de los clusteres
    pam_fit$clusinfo
    
    #para ver el índice de los medoides en el dataset 
    # pam_fit$medoids #--> PODRIA PINTARLOS DIFERENTE
    # dataPAM[pam_fit$medoids,]--> PINTAR LA TABLA PARA DAR PISTAS EN SCATTERPLOT
    
    # reduzco dimensionalidad para graficar en dos dimensiones, uso t-sne
    tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
    tsne_data <- tsne_obj$Y %>%
      data.frame() %>%
      setNames(c("X", "Y")) %>%
      mutate(cluster = factor(pam_fit$clustering))
    
    ggplot(aes(x = X, y = Y), data = tsne_data) +
      geom_point(aes(color = cluster)) +
      labs(title = "Resultados clustering PAM")
    
#     ggplot(data,aes_string(x=input$scatterPlot_x,y=input$scatterPlot_y,color=input$scatterPlot_color_encoding)) +
#       geom_point() + #data=scatterPlotData()
#       facet_wrap(~ ChurnLabel)
  })
  
  #####
  # Scatterplot: Register the x-variable selected so it doesn't appear on y-variable list
  remaining <- reactive({
    scatterPlotNumeric[c(-match(input$scatterPlot_x,scatterPlotNumeric))]
  })
  
  observeEvent(remaining(),{
    choices <- remaining()
    updateSelectInput(session = getDefaultReactiveDomain(),inputId = "scatterPlot_y",choices = choices)
  })
  
  # scatterPlotData <- reactive({
  #   data %>% filter(ChurnValue == 1)
  # })
  
  # Scatterplot: Render plot
  output$scatterPlot <- renderPlot({
    ggplot(data,aes_string(x=input$scatterPlot_x,y=input$scatterPlot_y,color=input$scatterPlot_color_encoding)) +
             geom_point() + #data=scatterPlotData()
      facet_wrap(~ ChurnLabel)
  })
}

# Run the App ---
shinyApp(ui = ui, server = server)
