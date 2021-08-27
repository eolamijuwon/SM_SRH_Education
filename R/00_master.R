
################################################################################
#
## Advancing sexual health education for young African adults in the digital age: 
## Uncovering strategies for successful organic engagement
#
# MASTER SCRIPT
#
################################################################################

# This is a master script that calls other scripts one-by-one
# to replicate the figure discussed in the paper. 


## Step 1: Package Installation -------------------------
source ("R/01_packages_install.R")

## Step 2: Set Theme for Figures and Import Font --------
#' Note: You need to have font Roboto Installed
source ("R/02_theme.R")

## Step 3: Data Wrangling and Analysis ------------------
# Two versions of the script are available:
# Depending on internet connection, you can 
# source/retrieve new data from online
source ("R/03_visualizations.R")


## Step 4: Regression Models ----------------------------
# Figure is saved in "Output"
source ("R/04_regression_models.R")


