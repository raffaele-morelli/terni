ui <- fluidPage(
  
  titlePanel("Esercizio Terni"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput("traccianti", "Tracciante", choices = c(pltnts)),
      selectInput("res", "Risoluzione", choices = c("100", "200")),
      selectInput("mese", "Mese", choices = c(seq(1:12))),
      plotOutput("distPlot"), 
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        # tabPanel("GAM Summary",  verbatimTextOutput("summary") ), 
        tabPanel("GAM check", 
                 fluidRow(
                   column(width = 4, plotOutput("check", width = 600, height = 600) ),
                   column(width = 6, offset = 2, verbatimTextOutput("summary") )
                 )
        ), 
        tabPanel("Splines", plotOutput("splines", width = 800, height = 800)),
        tabPanel("Predict", plotOutput("predictImage", width = 800, height = 250)),
        tabPanel("Boxplt", plotOutput("boxPlot", width = 1000, height = 800))
      )
    )
  )
)
