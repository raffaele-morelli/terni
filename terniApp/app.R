library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(summarytools)
library(knitr)
library(kableExtra)
library(sf)
library(glue)
library(terra)


df <- readr::read_csv("/home/rmorelli/R/terni/data/dataframes/df_finale_raw.csv", show_col_types = FALSE)
models <- readRDS("/home/rmorelli/R/terni/rds_out/modelli_gaussian_clean.RDS")
# crssvld <- readRDS("/home/rmorelli/R/terni/rds_out/cross_validation_gaussian.RDS")

# terni_sez <- st_read("~/R/terni/data/shp/Terni_sez.shp") # sezioni di censimento

dominio <- st_read(glue("/home/rmorelli/R/terni/data/dominio/dominio_200m.shp")) # 54 col
st_bbox(dominio) -> bbox
r_extent <- c(as.numeric(bbox["xmin"]), as.numeric(bbox["xmax"]), as.numeric(bbox["ymin"]), as.numeric(bbox["ymax"]))
rm(dominio)

pltnts <-  readRDS("/home/rmorelli/R/terni/rds_out/traccianti.RDS")

ui <- fluidPage(
  # titlePanel("Esercizio Terni"),
  
  # Sidebar with a slider input for number of bins 
  # sidebarLayout(
  #   sidebarPanel(
  #     # sliderInput("bins", "Number of bins:",
  #     #             min = 10, max = 60,
  #     #             value = 40),
  #     selectInput(
  #       "traccianti", "Tracciante", choices = c(
  #         "All", pltnts
  #       )
  #     ),
  #     selectInput(
  #       "res", "Risoluzione", choices = c(
  #         "200", "100"
  #       )
  #     ),
  #     plotOutput("distPlot", width = "100%", height = "300px")
  #     
  #   ),

    # Show a plot of the generated distribution
    # mainPanel(
      fluidRow(
        column(4, 
               selectInput("traccianti", "Tracciante", choices = c(pltnts)),
               selectInput("res", "Risoluzione", choices = c("100", "200")),
               plotOutput("distPlot", width = "100%", height = "300px"),
        ),
        column(4, verbatimTextOutput("summary")),
        column(4, plotOutput("check"))
        # verbatimTextOutput("summary"),
        # plotOutput("check", width = "500px", height = "500px"),
        # plotOutput("splines", width = "75%", height = "500px"),
        # plotOutput("predict", width = "75%")
      ),
      fluidRow(
        column(4, 
               plotOutput("splines")
        ),
        column(6, 
               plotOutput("predict", width = "70%")
        )
      )
    # )
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
      geom_histogram(bins = 60, fill = 'dodgerblue4', colour = 'white') + xlab("") + ylab("") +
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

  # image2 sends pre-rendered images
  output$predict <- renderImage({
    if (is.null(input$traccianti))
      return(NULL)

    return(list(
      src = glue("/home/rmorelli/R/terni/png_out/{input$traccianti}_{input$res}m_{input$res}res_01.png")
    ))

  }, deleteFile = FALSE)

}


# Run the application 
shinyApp(ui = ui, server = server)



