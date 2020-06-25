
### Title: Advancing SexEd for Young African Adults

### Author 1: *************
### Author 2: *************
### Author 3: *************




# Loading/installing required packages

pkgs <- c("tidyverse",
          "waffle",
          "lubridate",
          "tidytext",
          "extrafont",
          "showtext",
          "kableExtra",
          "stargazer",
          "scales",     # for automatically determining breaks and labels for axes and legends.
          "ggpubr",      # Publication Ready Plots
          "ggpol",       # Data Visualization - geom_boxjitter
          "stringr",
          "htmlwidgets",
          "tweenr",
          "webshot",
          "readxl",
          "MASS"
          )
          
          # # "mosaic",
          # "oddsratio",

miss_pkgs <- pkgs[!pkgs %in% 
                    installed.packages()[,1]] 

              # Installing the missing packages
              if(length(miss_pkgs)>0){
                install.packages(miss_pkgs)
              }
              
              # Loading all the packages
              invisible(lapply(pkgs,library,character.only=TRUE))
              rm(miss_pkgs)
              rm(pkgs)


          ## Install from DevTools
              
          #devtools::install_github("lchiffon/wordcloud2")
          library(wordcloud2)

          
          source("FB - DataMgt.R")  
              
          signif.num <- function(x) {
            symnum(x, corr = FALSE, na = FALSE, legend = FALSE,
                   cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                   symbols = c("***", "**", "*", " "))
          }
              
              
              
