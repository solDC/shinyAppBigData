source("ggplotMap.r", encoding="utf-8")

# Data loading
data <- read_excel("data/Telco_customer_churn.xlsx")
url = "data/county_ca.geojson"
california <- geojson_read(url,  what = "sp")

# Define UI ---
ui <- fluidPage(
  titlePanel('Telco Churn Analysis'),
  br(),
  h3("3- Where are the customers churning?"),
  plotOutput("map")
)

# Define server logic ---
server <- function(input, output) {
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
