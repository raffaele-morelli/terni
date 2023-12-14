library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(summarytools)
library(knitr)
library(kableExtra)

limiti <- read_excel("/home/rmorelli/R/terni/data/limiti.xlsx")
df <- readr::read_csv("/home/rmorelli/R/terni/data/dataframes/df_finale_lod_clean.csv", show_col_types = FALSE)
models <- readRDS("/home/rmorelli/R/terni/rds_out/modelli_all_clean.RDS")
crssvld <- readRDS("/home/rmorelli/R/terni/rds_out/cross_validation_all.RDS")

pltnts <-  readRDS("/home/rmorelli/R/terni/traccianti.RDS")

ui <- fluidPage(
  titlePanel("Esercizio Terni"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 10,
                  max = 60,
                  value = 40),
      selectInput(
        "traccianti", "Tracciante", choices = c(
          "All", pltnts
        )
      ),
      # selectInput(
      #   "set", "Set di variabili", choices = c(
      #     "All", "Scelte", "Mean"
      #   )
      # )            
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot", width = "75%", height = "400px"),
      verbatimTextOutput("summary"),
      plotOutput("check", width = "100%", height = "600px"),
      plotOutput("splines", width = "100%", height = "800px"),
      # textOutput("crossvld"),
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    if(input$traccianti == "All") { 
      return("Seleziona un tracciante")
    }
    df <- data.frame(log(df[[input$traccianti]]))
    
    reshape2::melt(df) %>% 
      ggplot(aes(x = value) ) + 
      geom_histogram(bins = input$bins, fill = 'dodgerblue4', colour = 'white') + xlab("") + ylab("") +
      ggtitle(input$traccianti)
    
  })
  
  output$splines <- renderPlot({
    if(input$traccianti == "All") { 
      return("Seleziona un tracciante") 
    }
    gratia::draw(models[[input$traccianti]])
  })
  
  output$check <- renderPlot({
    if(input$traccianti == "All") { 
      return("Seleziona un tracciante")
    }
    gratia::appraise(models[[input$traccianti]]) & ggtitle(input$traccianti)
  })    
  
  output$summary <- renderPrint({
    if(input$traccianti == "All") {
      return("Seleziona un tracciante") 
    }
    summary( (models[[input$traccianti]]))
  })
  
  # output$crossvld <- renderText({
  #   if(input$traccianti == "All") {
  #     return("Seleziona un tracciante")
  #   }
  #   crssvld[[input$traccianti]]
  #   })

}

# Run the application 
shinyApp(ui = ui, server = server)
