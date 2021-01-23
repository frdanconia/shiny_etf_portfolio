## code to prepare `xetra` dataset goes here

xetra <- arrow::read_feather(here::here('data', 'df_meta.feather'))

usethis::use_data(xetra)
