library(ggplot2)
library(gratia)
library(ggthemes)
library(dplyr)
library(mgcViz)
library(readr)


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
  g <- gratia::draw( modelli_test8_clean[[pltnt]], scales = "fixed", residuals = FALSE) & theme_fivethirtyeight(base_size = 9) 
  ggsave(g, filename =  glue::glue("immagini_articolo/{pltnt}.jpg"), width = 6, height = 6)  
}


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
    theme(text = element_text(family = "Arial", size = 10),
          axis.title = element_blank(), 
          axis.text.x = element_blank(),
          legend.position = "none"
    ) +
    scale_fill_brewer(palette = "BuPu") 
    ggsave(filename = glue::glue("immagini_articolo/bxplt_{pltnt}.jpg"),
           width = 9, height = 10, units = c("cm"), dpi = 150)
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
    scale_fill_brewer(palette = "BuPu")
  ggsave(filename = glue::glue("immagini_articolo/bxplt_cv_{pltnt}.jpg"),
         width = 9, height = 10, units = c("cm"), dpi = 150)
}

purrr::map(selezione_terni$X1, \(p) {
  gspline(p)
  bxplt(p)
  bxplt_cv(p)
})


