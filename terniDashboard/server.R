server <- function(input, output) {
  
  # distPlot ####
  output$distPlot <- renderPlot({
    if(input$traccianti == "All") {
      return("Seleziona un tracciante")
    }
    df <- data.frame((df[[input$traccianti]])) %>% setNames(c("value"))
    
    reshape2::melt(df) %>%
      ggplot(aes(x = value) ) +
      geom_histogram(bins = 80, fill = 'dodgerblue4', colour = 'white') + xlab("") + ylab("") +
      ggtitle(input$traccianti)
    
  })

  # boxPlot ####
  output$boxPlot <- renderPlot({
    if(input$traccianti == "All") {
      return("Seleziona un tracciante")
    }
    df_m <- data.frame((df[[input$traccianti]]))
    
    reshape2::melt(df_m) %>%
      ggplot(aes(x = value)) +
      geom_boxplot(fill = 'dodgerblue4', colour = 'white') + xlab("") + ylab("") +
      ggtitle(input$traccianti)
    
    idx <- grep(input$traccianti, names(df))
    names(df)[idx] <- "value"
    df %>% 
      select(c("data", "site", idx)) %>% 
      ggplot(aes(data, value)) + geom_boxplot() + facet_wrap(~site, ncol = 6) + 
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank() ) + xlab("") + ylab("")
    
  })

  # distros ####
  output$distros <- renderPlot({
    if(input$traccianti == "All") {
      return("Seleziona un tracciante")
    }
    
    idx <- grep(input$traccianti, names(df))
    names(df)[idx] <- "value"
    
    df %>% select(site, value) %>% 
      ggplot(aes(x = value) ) +
      geom_histogram(bins = 80, fill = 'dodgerblue4', colour = 'white') + 
      facet_wrap(~site, scales = "free_y") +
      xlab("") + ylab("") +
      ggtitle(input$traccianti)
  })
  
  # splines ####
  output$splines <- renderPlot({
    if(input$traccianti == "All") {
      return("Seleziona un tracciante")
    }
    gratia::draw(models[[input$traccianti]], scales = "fixed", residuals = FALSE)
  })

  # effects ####
  output$effects <- renderPlot({
    if(input$traccianti == "All") {
      return("Seleziona un tracciante")
    }
    df_temp <- readr::read_csv("/home/rmorelli/R/terni/data/dataframes/df_finale_raw.csv", show_col_types = FALSE)
    
    # models <- readRDS("~/R/terni/rds_out/modelli_gaussian_clean.RDS")
    index <- grep(as.character(input$traccianti), names(df_temp), value = FALSE)
    names(df_temp)[index] <- "value"
    
    
    gam_tdf <- mgcv::gam(formula(models[[input$traccianti]]), data = cbind(df_temp[,1:92], scale(df_temp[,92:247])), gamma = 1.4, family = family(models[[input$traccianti]]))
    
    plot(ggeffects::ggpredict(gam_tdf), facets = TRUE)
  })
  
  # check ####
  output$check <- renderPlot({
    if(input$traccianti == "All") {
      return("Seleziona un tracciante")
    }
    gratia::appraise(models[[input$traccianti]]) & ggtitle(input$traccianti)
  })
  
  # summary ####
  output$summary <- renderPrint({
    if(input$traccianti == "All") {
      return("Seleziona un tracciante")
    }
    summary( (models[[input$traccianti]]))
  })
  
  # predict pre render ####
  # output$predict <- renderImage({
  #   if (is.null(input$traccianti))
  #     return(NULL)
  #   
  #   return(list(
  #     src = glue("/home/rmorelli/R/terni/png_out/{input$traccianti}_{input$res}m_{input$res}res_01.png")
  #   ))
  #   
  # }, deleteFile = FALSE)
  
  # predict "ar volo" ####
  output$predictImage <- renderPlot({
    if (is.null(input$traccianti))
      return(NULL)
    
    print(input$kappa)
    if(input$kappa < 5) {
      f <- glue::glue('/home/rmorelli/R/terni/rds_out_traccianti/{input$traccianti}_{input$res}m_{input$res}res_{input$kappa}k.RDS')
    }else{
      f <- glue::glue('/home/rmorelli/R/terni/rds_out_traccianti/{input$traccianti}_{input$res}m_{input$res}res.RDS')
    }
    
    trcnt <- readRDS(f)
    trcnt_df <- do.call(rbind.data.frame, trcnt)
    
    if(input$res == 100) {
      n_col <- 109
    }else{
      n_col <- 54
    }
    
    # print(as.numeric( input$mese) )
    r <- matrix(trcnt_df[, as.numeric( input$mese)], ncol = n_col,  byrow = FALSE) %>% raster::raster()
    raster::extent(r) <- r_extent
    crs(r) <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"
    r_df <- as.data.frame(r, xy = TRUE) %>% na.omit() %>% setNames(c("x", "y", "value"))

    ggplot(data = r_df) +
      geom_raster(aes(x = x, y = y, fill = value)) +
      geom_sf(data = st_crop(terni_sez, st_bbox(r)), color = "grey90", fill = "transparent", size = 0.5) +
      geom_sf(data = pt_misura_utm32, shape = 21, fill = "lightgray", color = "black", size = 3) +
      scale_fill_viridis_c(option = "B", direction = -1) +
      theme_void() +
      theme(legend.position = "bottom", 
            legend.title = element_blank(),
            legend.text = element_text(size = 12),
            legend.key.size = unit(80, "points"),
            legend.key.height = unit(15, "points"),
            legend.key.width = unit(100, "points")
      ) + 
      ggtitle(glue("{input$traccianti} - {input$mese}")) +
      coord_sf(datum = sf::st_crs(32632)) -> g1
    
    ggplot(data = r_df) + 
      geom_histogram(aes(x = value), bins = 80, color = "dodgerblue4", fill = "lightgray") +
      theme_light() + theme(axis.title.x = element_blank(), axis.title.y = element_blank()) -> g2
    
    gridExtra::grid.arrange(g1, g2, layout_matrix = rbind(c(1, 1, 2),
                                                          c(1, 1, NA)))
    
  }, width = 1200, height = 600)
}
