#--------------------------------------------------------------------#
#### PRACTICAL ASSIGNMENT - MEASURRING AND VALUING HEALTH EFFECTS ####
#--------------------------------------------------------------------#

# Set up
rm(list = ls())
install.packages(shiny) # install this package if not already done
install.packages(diagram) # install this package if not already done
install.packages(tidyverse) # install this package if not already done

# Load package
library(shiny)
library(diagram)
library(tidyverse)

# Run shiny app
shiny::runGitHub("Teaching", "Xa4P", subdir = "Basics/shiny_app_health_effects/",  ref = "main")

# Perform the assignments in the shiny app!