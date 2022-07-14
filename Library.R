# This is the list of libraries require to source all R files from this project
options(repos = list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2022-07-14/'))
library(data.tree)
library(stringr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(magrittr)


R.Version()$version.string # "R version 4.1.0 (2021-05-18)"
