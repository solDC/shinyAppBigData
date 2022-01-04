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

pkg = c("tidyverse","dplyr","ggplot2","readxl","maps", "mapproj", "shiny")

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
source("telcoChurn/helpers.r")

############################################################################ 
# check out and prepare the data

# Drop space in colnames
colnames(data) <- gsub(" ","",colnames(data))

glimpse(data)

#Transform all character variables into factors
data[sapply(data, is.character)] <- lapply(data[sapply(data,is.character)], as.factor)

#Undo for LatLong 
data$LatLong <- as.character(data$LatLong)

#Churn Value and zipcode are doubles but really is a factor
data$ChurnValue <- as.factor(data$ChurnValue) #same as churn label
data$ZipCode <- as.factor(data$ZipCode)

# Drop variables that we don't need
dropable <- c("Count","CustomerID","Country","State") # See later which can be dropped
data <- (data[, !(names(data) %in% dropable)])

#20 possible chrurn reasons
levels(data$ChurnReason)

# data %>% 
#   ggplot(aes(x = ChurnScore, y=CLTV, color = ChurnLabel)) +
#   geom_point() 
# 
# #Independientemente del churnScore y CLTV, el tipo de contrato Month-to-Month tiene muchísimo churn
# data %>% filter(ChurnValue == 1) %>% 
#   ggplot(aes(x = ChurnScore, y=CLTV, color = Contract)) +
#   geom_point() 
# 
# data %>% filter(ChurnValue == 1) %>% 
#   ggplot(aes(x = TenureMonths, y=TotalCharges, color = Contract)) +
#   geom_point() 
# 
# data %>% filter(ChurnValue == 1) %>% 
#   ggplot(aes(x = TenureMonths, y=TotalCharges, color = Contract)) +
#   geom_point() 


# Filter variables names for scatterplots inputs
scatterPlotNumeric <- names(data %>% select(TenureMonths,MonthlyCharges,TotalCharges,ChurnScore,CLTV))
scatterPlotFactor <- names(data %>% select_if(is.factor) %>% 
                             select(-contains("Churn")) %>% 
                             select(-contains("City")) %>% 
                             select(-contains("ZipCode")) 
                           )
                           
# Define UI ---
ui <- fluidPage(
  
  titlePanel('Telco Churn Analysis'),
  
  br(),
  h3("1- Are there any groups of customers that are churning?"),
  sidebarPanel(
    
    # Scatterplot Inputs
    selectInput(inputId = 'scatterPlot_x', label = 'X Variable', choices = scatterPlotNumeric), #, selected = "TenureMonths"
    selectInput(inputId = 'scatterPlot_y', label = 'Y Variable', choices = scatterPlotNumeric), #, selected = "TotalCharges"
    selectInput(inputId = 'color_encoding', 'Select a categorical variable for color encoding',
                choices = scatterPlotFactor, selected = "Contract")
  ),
  
  mainPanel(
    plotOutput(outputId = 'scatterPlot')
  )
)

# Define server logic ---
server <- function(input, output) {
  
  #Register the x-variable selected so it doesn't appear on y-variable list
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

  output$scatterPlot <- renderPlot({
    ggplot(data,aes_string(x=input$scatterPlot_x,y=input$scatterPlot_y,color=input$color_encoding)) +
             geom_point() + #data=scatterPlotData()
      facet_wrap(~ ChurnLabel)
  })
}

# Run the App ---
shinyApp(ui = ui, server = server)
