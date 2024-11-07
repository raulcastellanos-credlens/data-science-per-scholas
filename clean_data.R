
packages_list <- c("tidyverse", 
                   "lubridate", 
                   "janitor",
                   "dplyr",
                   "stringr",
                   "readr", 
                   "knitr",
                   "ggplot2", 
                   "readxl", 
                   "tinytex")
#tinytex::install_tinytex(force = TRUE)

sapply(packages_list,
       function(x) {
         if(! x %in% rownames(installed.packages()))
           install.packages(x)
         require(x, character.only=T)
       })

options(scipen=999) # prevent scientific notation

#remove all of the previous work in my environment
rm(list = ls())
