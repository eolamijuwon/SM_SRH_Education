################################################################################
#
## Advancing sexual health education for young African adults in the digital age: 
## Uncovering strategies for successful organic engagement
#
# R session preparation
#
################################################################################



# these are the required packages
pkgs <- c("tidyverse",   ## Data Management
          "here",        ## Set work directory
          "ggwaffle",
          "lubridate",
          "tidytext",
          "extrafont",   ## Importing Fonts
          "showtext",    ## Fonts
          "kableExtra",
          "stargazer",
          "scales",      # for automatically determining breaks and labels for axes and legends.
          "ggpubr",      # Publication Ready Plots
          "ggpol",       # Data Visualization - geom_boxjitter
          "stringr",
          "htmlwidgets",
          "tweenr",
          "webshot",
          "readxl",
          "MASS")

# replaced w pacman, basically the same
if (!require("pacman", character.only = TRUE)){
  install.packages("pacman", dep = TRUE)
  if (!require("pacman", character.only = TRUE))
    stop("Package not found")
}

library(pacman)
if(!sum(!p_isinstalled(pkgs))==0){
  p_install(
    package = pkgs[!p_isinstalled(pkgs)], 
    character.only = TRUE
  )
}


p_load(pkgs, character.only = TRUE)
rm(pkgs)



# devtools::install_github("liamgilbey/ggwaffle")
# library(ggwaffle)
