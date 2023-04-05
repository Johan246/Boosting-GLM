# =========================================================================
# =========================================================================
#                         Boosting GLM 
# =========================================================================
# =========================================================================
 
###### Kända buggar / TO DO ######
# Duration ej hanterad om utgångsmodellen inte är tariff 


# INIT
setwd("//e75a0679/sakta/UTV/SU/Program/Analys/Boosting GLM/Boosting-GLM")
source("load_packages.r")
load_packages(updateR = FALSE)
 
# Output parameters --------------------------------------------------

date <- "20220403"
data <- "auspriv" # REAL, freMTPL, auspriv, beMTPL, norauto
suffix <- data

new_models <- TRUE
new_univar_effects <- TRUE
new_twoway_effects <- TRUE
save <- TRUE
scoring_boosting_factors <- TRUE

plot_folder <- paste("Plottar/",suffix,"_",date, sep="")
dir.create(plot_folder)


# Modeling hyperparameters ---------------------------------------------------------
 
n_trees_mu <- 1000
tree_depth_mu <- 2
learning_rate_mu <- 0.025

train_frac <- 0.8
cal_frac <- 0.3 # Of the training data, how much for calibration of marginal effects?
