############################################################################ 
## Authors
# ARIAS CAPE, RODRIGO
# DOMINGUEZ CARNUCCIO, MARIA SOL
# PAEZ CEBALLOS, AXIEL 
############################################################################ 
## Install or load package

pkg = c("tidyverse","dplyr","ggplot2","readxl","maps", "mapproj", "shiny","cluster","Rtsne","rsconnect","geojsonio","broom","geojsonR","viridis")

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
## Set directory. This part is personal for each enviroment. The wd must be the root github directory
#setwd("~/shinyAppBigData")

data <- read_excel("data/Telco_customer_churn.xlsx")
url = "data/county_ca.geojson"
california <- geojson_read(url,  what = "sp")

source("ggplotMap.r", encoding="utf-8")

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
dropable <- c("CustomerID","Country","State") # See later which can be dropped
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
# Prep data: drop variables that have repeated data 
dataPAM <- data %>% filter(ChurnValue == 1)
dropablePAM <- c("LatLong","Latitude","Longitude","ZipCode","ChurnValue","ChurnLabel","ChurnReason") # See later which can be dropped
dataPAM <- (dataPAM[, !(names(data) %in% dropablePAM)])

#calculate number of clusters recommended
gower_dist <- daisy(dataPAM, metric = c("gower")) 
gower_mat <- as.matrix(gower_dist)
sil_width <- c(NA)
for(i in 2:8){
  pamFit <- pam(gower_dist, diss = TRUE, k = i)
  sil_width[i] <- pamFit$silinfo$avg.width
}
k <- which.max(sil_width) #Recommended number of clusters

##############
aux <- distinct(data, ChurnReason)
aux$ChurnID <- seq.int(nrow(aux))
aux <- aux[aux$ChurnID < 21, ]
data <- left_join(data, aux, by = "ChurnReason")
churns <- data[data$ChurnID < 21, ]
churns <- churns[!is.na(churns$Gender),]
churns <- churns[!is.na(churns$Contract),]
churns[sapply(churns, is.character)] <- lapply(churns[sapply(churns,is.character)], as.factor)
churns$ChurnID <- as.character(churns$ChurnID)

scatterPlotFactor2 <- names(churns %>%select_if(is.factor) %>% 
                              select(-contains("Churn")) %>% 
                              select(-contains("City")) %>% 
                              select(-contains("ZipCode")))

# Define UI ---
ui <- fluidPage(
  titlePanel(h1("TELCO CHURN ANALYSIS", align="center")),
  br(),
  tabsetPanel(
    tabPanel("Question 1",
             h3("1- Are there any groups of customers that are churning?", style = "color:blue"), 
             sidebarPanel(
               br(),
               #SCATTERPLOT
               h4("1.1- Scatterplot", style = "color:blue"),
               # Scatterplot Inputs
               selectInput(inputId = 'scatterPlot_x', label = 'X Variable', choices = scatterPlotNumeric), 
               selectInput(inputId = 'scatterPlot_y', label = 'Y Variable', choices = scatterPlotNumeric),
               selectInput(inputId = 'scatterPlot_color_encoding', 'Select a categorical variable for color encoding',
                           choices = scatterPlotFactor, selected = "Contract"),
               br(),
               
               #PAM
               h4("1.2- K-Medoids Clustering", style = "color:blue"),
               br(),
               
               numericInput('numClusters','Select the number of clusters', k, min =2, max=8, step=1),
               textOutput(outputId = 'kRecommended'),
             ), #sidebarPanel
             
             mainPanel(
               plotOutput(outputId = 'scatterPlot'),
               plotOutput(outputId = 'pamPlot'),
               tableOutput(outputId= 'pamTable')
             )#main panel
             
    ), #tabpanel Question 1
    
    tabPanel("Question 2",
             h3("1- Are there any groups of customers that are churning?", style = "color:blue"), 
             sidebarPanel(
               selectInput(inputId = 'scatterPlot_color_encoding2', 'Select a categorical variable for color encoding',
                           choices = scatterPlotFactor2, selected = "Contract"),
               br(),
               tableOutput("table"),
               ),
             mainPanel(
               plotOutput(outputId = 'scatterPlot2')
             )             

    ), #tabPanel Question 3
    
    tabPanel("Question 3",
             h3("3- Where are the customers churning?", style = "color:blue"), 
             plotOutput("map")
    )#tabPanel Question 3
    
  ) #tabsetPanel
) #fluidPage

# Define server logic ---
server <- function(input, output) {
  
  #####
  # Scatterplot: Register the x-variable selected so it doesn't appear on y-variable list
  #####
  remaining <- reactive({
    scatterPlotNumeric[c(-match(input$scatterPlot_x,scatterPlotNumeric))]
  })
  
  observeEvent(remaining(),{
    choices <- remaining()
    updateSelectInput(session = getDefaultReactiveDomain(),inputId = "scatterPlot_y",choices = choices, selected = "TotalCharges")
  })
  
  # scatterPlotData <- reactive({
  #   data %>% filter(ChurnValue == 1)
  # })
  
  # Scatterplot: Render plot
  output$scatterPlot <- renderPlot({
    ggplot(data,aes_string(x=input$scatterPlot_x,y=input$scatterPlot_y,color=input$scatterPlot_color_encoding)) +
      geom_point() + #data=scatterPlotData()
      facet_wrap(~ ChurnLabel) + 
      ggtitle("Scatterplot faceting Churn")
  }) 
  
  #####
  # PAM
  #####
  
  #Calculate and print the recommended number of clusters
  output$kRecommended <- renderText({
    paste("The number of clusters recommended is ",k)
  })
  
  pamFit <- reactive({
    pam(gower_dist, diss = TRUE, input$numClusters)
  })
  
  #Calculate clusters and render plot
  output$pamPlot <- renderPlot({
    # reduzco dimensionalidad para graficar en dos dimensiones, uso t-sne
    tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
    tsne_data <- tsne_obj$Y %>%
      data.frame() %>%
      setNames(c("X", "Y")) %>%
      mutate(cluster = factor(pamFit()$clustering))
    
    ggplot(aes(x = X, y = Y), data = tsne_data) +
      geom_point(aes(color = cluster)) +
      labs(title = "Resultados clustering PAM")
  })
  
  output$pamTable <- renderTable({
    dataPAM[pamFit()$medoids,]      
  })  
  
  output$scatterPlot2 <- renderPlot({
    ggplot(churns, aes_string(x = "ChurnID", fill = input$scatterPlot_color_encoding2)) + 
      geom_bar()
    
  })
  
  
  
  
  output$table <- renderTable(aux)
  
  output$map <- renderPlot({
    # Preprocessing
    california_fortified <- tidy(california, region = "NAME")
    
    ciudades <- aggregate(Count ~ City, data, sum)
    
    california_fortified = california_fortified %>%
      left_join(. , ciudades, by=c("id"="City"))
    # Note that if the number of restaurant is NA, it is in fact 0
    california_fortified$Count[ is.na(california_fortified$Count)] = 0.001
    ggplotMap(california_fortified)
  })
  
}

# Run the App ---
shinyApp(ui = ui, server = server)

