counties <- readRDS("counties.rds")
head(counties)

pkg = c("shiny","maps", "mapproj")

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
source("helpers.R")

args <- list(counties$white, "darkgreen", "% White")
do.call(percent_map,args)

# Define UI ---
ui <- fluidPage(
  titlePanel("censusVis"),
  
  sidebarLayout(
    
    sidebarPanel(
      helpText("Create demographic maps with the information from the 2010 US Census."),
      
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = c("Percent white", 
                              "Percent black",
                              "Percent hispanic", 
                              "Percent asian"), 
                  selected = "Percent white"),
      
      sliderInput("range", 
                  label = ("Range of interest:"),
                  min = 0, max = 100, value = c(0,100)
      )
    ), #sidebar panel
    
    mainPanel(
      plotOutput("map"),
      textOutput("selected_var")
    )
    
  ) #sidebar layout
) # fluidPage

# Define server logic ---
server <- function(input, output) {
  output$map <- renderPlot({
    args <- switch(input$var,
                   "Percent white" = list(counties$white, "darkgreen", "% White"),
                   "Percent black" = list(counties$black, "black", "% Black"),
                   "Percent hispanic" = list(counties$hispanic, "darkorange", "% Hispanic"),
                   "Percent asian" = list(counties$asian, "darkviolet", "% Asian"))
    
    args$min <- input$range[1]
    args$max <- input$range[2]
    
    do.call(percent_map, args)
  })
  output$selected_var <- renderText({ 
    paste("You have selected", input$var)
  })
}

# Run the App ---
shinyApp(ui = ui, server = server)
