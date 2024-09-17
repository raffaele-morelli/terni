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
    df_temp <- readr::read_csv("/home/rmorelli/R/terni/data/dataframes/df_finale_raw.csv", show_col_types = FALSE) %>% 
      mutate(stagione = case_when(
        month_y %in% c("December_16", "January_17", "February_17", "December_17", "February_18") ~ "I",
        month_y %in% c("March_17", "April_17", "May_17") ~ "P",
        month_y %in% c("July_17", "August_17") ~ "E",
        month_y %in% c("September_17", "November_17") ~ "A"
      ))
    
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
    summary(models[[input$traccianti]])
  })
  
  # estimates as variance components ####
  output$vcomp <- renderPrint(({
    mgcv::gam.vcomp(models[[input$traccianti]])
  }))

  # predict "ar volo" ####
  output$predictImage <- renderPlot({
    if (is.null(input$traccianti))
      return(NULL)
    
    # f <- glue::glue('/home/rmorelli/R/terni/{rds_out_traccianti}/{input$traccianti}_{input$res}m_{input$res}res.RDS')
    f <- glue::glue('/home/rmorelli/R/terni/{rds_out_traccianti}/{input$traccianti}_200m_100res.RDS')
    
    trcnt <- readRDS(f)
    trcnt_df <- do.call(rbind.data.frame, trcnt)
    
    # if(input$res == 100) {
      n_col <- 109
    # }else{
    #   n_col <- 54
    # }
    
    # print(as.numeric( input$mese) )
    r <- matrix(trcnt_df[, as.numeric( input$mese)], ncol = n_col,  byrow = FALSE) %>% raster::raster()
    raster::extent(r) <- r_extent
    crs(r) <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"
    r_df <- as.data.frame(r, xy = TRUE) %>% na.omit() %>% setNames(c("x", "y", "value"))

    ggplot(data = r_df) +
      geom_raster(aes(x = x, y = y, fill = value)) +
      geom_sf(data = st_crop(terni_sez, st_bbox(r)), color = "grey95", fill = "transparent") +
      geom_sf(data = pt_misura_utm32, shape = 21, fill = "lightgray", color = "black", size = 4) +
      geom_sf(data = acciaieria, shape = 24, fill = "grey70", color = "gray15", size = 4) +
      scale_fill_viridis_c(option = "B", direction = -1) +
      theme_void() +
      theme(legend.position = "bottom", 
            legend.title = element_blank(),
            legend.text = element_text(size = 12),
            legend.key.size = unit(80, "points"),
            legend.key.height = unit(15, "points"),
            legend.key.width = unit(100, "points")
      ) + 
      ggtitle(glue("{input$traccianti} (mese {input$mese})")) +
      coord_sf(datum = sf::st_crs(32632)) -> g1
    
    ggplot(data = r_df) + 
      geom_histogram(aes(x = value), bins = 80, color = "dodgerblue4", fill = "lightgray") +
      theme_light() + theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
      ggtitle("Distribuzione predicted") -> g2
    
    gridExtra::grid.arrange(g1, g2, layout_matrix = rbind(c(1, 1, 2),
                                                          c(1, 1, NA)))
    
  }, width = 1200, height = 600)
}
