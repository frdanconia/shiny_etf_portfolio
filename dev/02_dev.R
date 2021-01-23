# Building a Prod-Ready, Robust Shiny Application.
# 
# Each step is optional. 
# 

# 2. All along your project

## 2.1 Add modules
## 
golem::add_module( name = "raw" ) 
golem::add_module( name = "universe" ) 
golem::add_module( name = "finder" )

golem::add_utils(name = 'ui', module = 'universe')
golem::add_utils(name = 'server', module = 'finder')

golem::add_fct(name = 'nav')



## 2.2 Add dependencies
usethis::use_package('shinydashboard')
usethis::use_package('markdown')
usethis::use_package('magrittr')
usethis::use_package('arrow')
usethis::use_package('vroom')
usethis::use_package('data.table')
usethis::use_package('dplyr')
usethis::use_package('ggplot2')
usethis::use_package('glue')
usethis::use_package('gt')
usethis::use_package('highcharter')
usethis::use_package('plotly')
usethis::use_package('purrr')
usethis::use_package('quantmod')
usethis::use_package('stringr')
usethis::use_package('tidyquant')
usethis::use_package('zoo')
usethis::use_package('tidyr')
usethis::use_package('tibble')
usethis::use_package('timetk')

## 2.3 Add tests

usethis::use_test( "app" )

## 2.4 Add a browser button

golem::browser_button()

## 2.5 Add external files

golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

# 3. Documentation

## 3.1 Vignette
usethis::use_vignette("shinyETF")
devtools::build_vignettes()

## 3.2 Code coverage
## You'll need GitHub there
usethis::use_github()
usethis::use_travis()
usethis::use_appveyor()

# You're now set! 
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
