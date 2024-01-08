ui <- fluidPage(
  
  titlePanel("Esercizio Terni"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput("traccianti", "Tracciante", choices = c(pltnts)),
      selectInput("res", "Risoluzione", choices = c("100", "200")),
      plotOutput("distPlot"), width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("check", width = 800, height = 800)), 
        tabPanel("Splines", plotOutput("splines", width = 800, height = 800)),
        tabPanel("GAM Summary",  verbatimTextOutput("summary") ), 
        tabPanel("Predict", 
                 # plotOutput("predict"),
                 plotOutput("predictImage", width = 800, height = 250)
                 ),
        # tabPanel("test", plotlyOutput('plot'))

      )
    )
  )
)