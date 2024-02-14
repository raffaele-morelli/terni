library(ggplot2)
library(gratia)
library(ggthemes)
library(dplyr)
library(mgcViz)

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

gratia::draw( modelli_test8_clean[["PM10"]], scales = "fixed", residuals = FALSE) & theme_fivethirtyeight() -> g_PM10
ggsave(g_PM10, filename = "immagini_poster/PM10.jpg", width = 12, height = 12)

gratia::draw( modelli_test8_clean[["Mo_s"]], scales = "fixed", residuals = FALSE) & theme_fivethirtyeight() -> g_Mo_s
ggsave(g_Mo_s, filename = "immagini_poster/Mo_s.jpg", width = 12, height = 12)

gratia::draw( modelli_test8_clean[["Sn_i"]], scales = "fixed", residuals = FALSE) & theme_fivethirtyeight() -> g_Sn_i
ggsave(g_Sn_i, filename = "immagini_poster/Sn_i.jpg", width = 12, height = 12)


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
    theme_fivethirtyeight() %+replace%
    theme(text = element_text(family = "Arial", size = 10),
          axis.title = element_blank(), 
          axis.text.x = element_blank(),
          legend.position = "none"
    ) +
    scale_fill_brewer(palette = "BuPu")
    ggsave(filename = glue::glue("immagini_poster/bxplt_{pltnt}.jpg"),
           width = 9, height = 12, units = c("cm"), dpi = 300)
}

bxplt("PM10") 
bxplt("Mo_s")
bxplt("Sn_i")

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
    theme_fivethirtyeight() %+replace%
    theme(text = element_text(family = "Arial", size = 9),
          axis.title = element_blank(), 
          axis.text.x = element_blank(),
          legend.position = "none",
          panel.background = element_blank(),
          plot.background = element_blank(),
          strip.background = element_blank()
    ) +
    scale_fill_brewer(palette = "BuPu")
  ggsave(filename = glue::glue("immagini_poster/bxplt_cv_{pltnt}.jpg"),
         width = 9, height = 12, units = c("cm"), dpi = 300)
}
bxplt_cv("PM10")
bxplt_cv("Mo_s")
bxplt_cv("Sn_i")

