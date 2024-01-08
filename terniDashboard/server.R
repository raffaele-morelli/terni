server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    if(input$traccianti == "All") {
      return("Seleziona un tracciante")
    }
    df <- data.frame(log(df[[input$traccianti]]))
    
    reshape2::melt(df) %>%
      ggplot(aes(x = value) ) +
      geom_histogram(bins = 60, fill = 'dodgerblue4', colour = 'white') + xlab("") + ylab("log(value)") +
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
  
  output$predictImage <- renderPlot({
    if (is.null(input$traccianti))
      return(NULL)
    
    f <- glue::glue('/home/rmorelli/R/terni/rds_out_traccianti/{input$traccianti}_{input$res}m_{input$res}res.RDS')
    
    trcnt <- readRDS(f)
    trcnt_df <- do.call(rbind.data.frame, trcnt)
    
    if(input$res == 100) {
      n_col <- 109
    }else{
      n_col <- 54
    }
    
    r <- matrix(trcnt_df[, 1], ncol = n_col,  byrow = FALSE) %>% raster::raster()
    raster::extent(r) <- r_extent
    crs(r) <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"
    r_df <- as.data.frame(r, xy = TRUE) %>% na.omit() %>% setNames(c("x", "y", "value"))

    ggplot(data = r_df) +
      geom_raster(aes(x = x, y = y, fill = value)) +
      geom_sf(data = st_crop(terni_sez, st_bbox(r)), color = "grey90", fill = "transparent", size = 0.5) +
      geom_sf(data = pt_misura_utm32, shape = 21, fill = "lightgray", color = "black", size = 3) +
      scale_fill_viridis_c(direction = -1, option = "magma") +
      theme_void() +
      theme(legend.position = "none") +
      coord_sf(datum = sf::st_crs(32632)) -> g1
    
    ggplot(data = r_df) + 
      geom_histogram(aes(x = value), bins = 80, color = "dodgerblue4", fill = "lightgray") +
      theme_light() + theme(axis.title.x = element_blank(), axis.title.y = element_blank()) -> g2
    
    gridExtra::grid.arrange(g1, g2, 
                            layout_matrix = rbind(c(1, 1, 2),
                                                  c(1, 1, 2))
                            )
    
  }, width = 1200, height = 600)
}
