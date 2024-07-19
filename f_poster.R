library(ggplot2)
library(gratia)
library(ggthemes)
library(dplyr)
library(mgcViz)
library(readr)

source("~/R/incendi/theme_fires.R")
theme_terni <- function (base_size = 12, base_family = "sans") 
{
  colors <- deframe(ggthemes::ggthemes_data[["fivethirtyeight"]])
  (theme_foundation(base_size = base_size, base_family = base_family) + 
      theme(line = element_line(colour = "black"), 
            rect = element_rect(fill = colors["Light Gray"], linetype = 0, colour = NA), 
            text = element_text(colour = colors["Dark Gray"]), 
            axis.title = element_blank(), axis.text = element_text(), 
            axis.ticks = element_blank(), axis.line = element_blank(), 
            legend.background = element_rect(), legend.position = "bottom", 
            legend.direction = "horizontal", legend.box = "vertical", 
            panel.grid = element_line(colour = NULL), panel.grid.major = element_line(colour = colors["Medium Gray"]), 
            panel.grid.minor = element_blank(), 
            plot.title = element_text(hjust = 0, size = rel(1.5), face = "bold"), 
            plot.margin = unit(c(1, 1, 1, 1), "lines"), 
            strip.background = element_rect()))
}

selezione_terni <- read_csv("selezione_terni.csv", col_names = FALSE, show_col_types = FALSE)

# spline di Mo_s, PM10, Sn_i 
# box plot della cross validation  eliminando però rsq20 e rmse 20 lasciando solo rsq80 e rsme80 con la dicitura generica rsq e rmse

modelli_test8_clean <- readRDS("~/R/terni/rds_gaussian_test8/modelli_test8_clean.RDS")
cross_validation <- readRDS("~/R/terni/rds_gaussian_test8/cross_validation.RDS")
incertezza <- readRDS("~/R/terni/rds_gaussian_test8/incertezza.RDS")

{
  # gratia::draw( modelli_test8_clean[["PM10"]], select = 1, scales = "fixed", residuals = FALSE) & theme_fivethirtyeight() 
  # ggsave(g, filename = "immagini_poster/PM10_pbl00.jpg")
  # gratia::draw( modelli_test8_clean[["PM10"]], select = 3, scales = "fixed", residuals = FALSE) & theme_fivethirtyeight() 
  # ggsave(g, filename = "immagini_poster/PM10_pbl00.jpg")
  # gratia::draw( modelli_test8_clean[["PM10"]], select = 4) & theme_fivethirtyeight()
}

gspline <- function(pltnt) {
  titolo <- ggtitle(glue::glue("pltnt"))
  
  g <- gratia::draw( modelli_test8_clean[[pltnt]], scales = "fixed", residuals = FALSE) & 
    theme_fivethirtyeight(base_size = 8) %+replace%
    theme(axis.title = element_blank(), 
          axis.text.x = element_blank(),
          legend.position = "none",
          panel.background = element_blank(),
          plot.background = element_blank(),
          strip.background = element_blank()
    )

  ggsave(g, filename =  glue::glue("immagini_articolo/splines/{pltnt}.jpg"), 
         width = 14, height = 14, units = c("cm"), dpi = 200)  
  return(g)
}
gspline("PM10")

bxplt <- function(pltnt) {
  incertezza[[pltnt]] %>% 
    as.data.frame() %>% 
    select(c("rsq80", "rmse80", "FB", "NMSE", "FAC2")) %>% 
    setNames(c("RSQ", "RMSE", "FB", "NMSE", "FAC2")) %>% 
    mutate(
      across(all_of(names(.)), as.numeric)
    ) -> df
  
  reshape2::melt(df) %>% 
    ggplot() +
    geom_boxplot(aes(y = value, x = variable, fill = variable)) +
    facet_wrap(~variable, scales = "free", ncol = 5) +
    theme_fivethirtyeight(base_size = 10) %+replace%
    theme(text = element_text(family = "Arial", size = 9),
          axis.title = element_blank(), 
          axis.text.x = element_blank(),
          legend.position = "none",
          panel.background = element_blank(),
          plot.background = element_blank(),
          strip.background = element_blank()
    ) +
    scale_fill_brewer(palette = "BuPu") + ggtitle(glue::glue("{pltnt}"))
    ggsave(filename = glue::glue("immagini_articolo/bxplt/bxplt_{pltnt}.jpg"),
           width = 9, height = 10, units = c("cm"), dpi = 200)
}

bxplt_cv <- function(pltnt) {
  do.call(rbind, cross_validation[[pltnt]]) %>% 
    as.data.frame() %>% 
    setNames(c("rmse20", "RMSE", "rsq20", "RSQ", "FAC2", "FB", "NMSE"))  %>% 
    select(c("RSQ", "RMSE", "FB", "NMSE", "FAC2")) %>% 
    mutate(
      across(all_of(names(.)), as.numeric)
    ) -> df
  
  reshape2::melt(df) %>% 
    ggplot() +
    geom_boxplot(aes(y = value, x = variable, fill = variable)) +
    facet_wrap(~variable, scales = "free", ncol = 5) +
    theme_fivethirtyeight(base_size = 10) %+replace%
    theme(text = element_text(family = "Arial", size = 9),
          axis.title = element_blank(), 
          axis.text.x = element_blank(),
          legend.position = "none",
          panel.background = element_blank(),
          plot.background = element_blank(),
          strip.background = element_blank()
    ) +
    scale_fill_brewer(palette = "BuPu")  + ggtitle(glue::glue("{pltnt}"))
  
  ggsave(filename = glue::glue("immagini_articolo/bxplt_cv/bxplt_cv_{pltnt}.jpg"),
         width = 9, height = 10, units = c("cm"), dpi = 200)
}

purrr::map(selezione_terni$X1, \(p) {
  gspline(p)
  # bxplt(p)
  # bxplt_cv(p)
})


