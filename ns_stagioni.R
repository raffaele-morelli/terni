# tibble::tribble(
#   ~data_inizio, ~data_fine,  ~Season, ~season1,
#   "2016-11-19",  "2016-12-12", "Winter", "I",
#   "2016-12-17",  "2017-01-12", "Winter", "I",
#   "2017-01-21",  "2017-02-20", "Winter", "I",
#   "2017-02-25",  "2017-03-27", "Winter", "P",
#   "2017-04-01",  "2017-05-01", "Summer", "P",
#   "2017-05-06",  "2017-06-05", "Summer", "P",
#   "2017-06-10",  "2017-07-17", "Summer", "E",
#   "2017-07-22",  "2017-08-28", "Summer", "E",
#   "2017-09-02",  "2017-10-02", "Summer", "A",
#   "2017-10-21",  "2017-11-20", "Winter", "A",
#   "2017-11-25",  "2018-01-15", "Winter", "I",
#   "2018-01-20",  "2018-02-19", "Winter", "I"
# ) %>% mutate(dta_inizio = as.Date(data_inizio, format = "Y-%m-%d"),
#              dta_fine = as.Date(data_fine, format = "Y-%m-%d")) -> associazione
library(forcats)

f_stagioni <-  function(df) {
  df %>% 
    mutate(
      season1 = case_when(
        data_inizio %in% c("2016-11-19", "2016-12-17", "2017-01-21", "2017-11-25", "2018-01-20") ~ "I",
        data_inizio %in% c("2017-02-25", "2017-04-01", "2017-05-06") ~ "P",
        data_inizio %in% c("2017-06-10", "2017-07-22") ~ "E",
        data_inizio %in% c("2017-09-02", "2017-10-21") ~ "A"
      )) %>% 
    mutate(stagione = as.factor(season1)) %>% 
    mutate(stagione = fct_relevel(stagione, c("P", "E", "A", "I"))) -> df
  
  return(df)
  
}
