library(tidyverse)
setwd("C:/Users/ZBFZHR/Desktop/Pandas-demo")
exp_imp_tbl <- read.csv("C:/Users/ZBFZHR/Desktop/Pandas-demo/data_comexstat.csv")
exp_imp_tbl <- exp_imp_tbl %>% mutate_if(is.character, as_factor)
head(exp_imp_tbl)
exp_imp_tbl$date <-  as.Date(exp_imp_tbl$date, format="%m/%d/%Y")
head(exp_imp_tbl)
library(skimr)
skim(exp_imp_tbl)

library(lubridate) 
library(timetk)
library(gt)
library(tidyquant)

exp_imp_year_month_tbl <- exp_imp_tbl %>% 
    mutate(year = year(date),
           month = months(date))
head(exp_imp_year_month_tbl)
tail(exp_imp_year_month_tbl)

expt_year_month_total_tbl <- exp_imp_year_month_tbl %>% 
    filter(type == 'Export') %>% 
    group_by(month, year) %>% 
    summarise(total_tons = sum(tons)) %>% 
    ungroup()

head(expt_year_month_total_tbl)

expt_year_month_total_tbl %>% 
    mutate(by_month = month %>% str_to_title() %>% as_factor()) %>% 
    ggplot(aes(x = year,
           y = total_tons)) + 
    geom_point(size = .8) + 
    geom_line() +
    facet_wrap(~by_month) +
    theme_tq() +
    scale_y_continuous(labels = scales::number_format(scale = 1e-6, suffix = "M")) +
    scale_x_continuous(breaks = c(1997, 2000, 2005, 2010)) +
    labs(
        title = "Total of Tons for the Monthly Exports Between 1997 and 2013",
        subtitle = "Considering all Brazilian States, and Soybean, Soybeans Meal, Soybean Oil, Sugar, Corn and Wheat commodities",
        x = "",
        y = "Millions of Tons",
        caption = "Source:COMEXSTAT, Brazil"
        ) +
    theme(axis.text.x = element_text(size = 7)) 

top_5_product_exp_tbl <- exp_imp_year_month_tbl %>%  
    filter(year %in% c(2010:2013)) %>% 
    filter(type == "Export") %>% 
    group_by(product) %>% 
    summarise(total_tons_exp = sum(tons)) %>% 
    ungroup() %>% 
    slice_max(total_tons_exp, n=5)
top_5_product_exp_tbl

top_5_product_exp_tbl %>% 
    mutate(product_str = case_when(
        product == "soybeans" ~ "Soybean",
        product == "corn" ~ "Corn",
        product == "soybean_oil" ~ "Soya_Oil",
        product == "soybean_meal" ~ "Soya_Meal",
        TRUE ~ "Sugar"
            )) %>% 
  ggplot(aes(x=total_tons_exp, 
               y=fct_reorder(product_str, total_tons_exp),
               fill = product)) + 
    geom_col() +
    #scale_fill_manual(values = c("#7EBEF7", "#2595F5", "#BBD7F0")) +
    scale_x_continuous(labels = scales::number_format(scale = 1e-6, suffix = "M")) +
    guides(fill = FALSE) +
    theme_tq() +
    labs(
        title = "Top % - Brazilian Commodities Exports",
        subtitle = "Considering the Last 5 Years",
        caption = "Source:COMEXSTAT-Brazil",
        x = "Millions of Tons",
        y = "Commodities"
    )

