# =========================================================================
# =========================================================================
#                         Boosting GLM 
# =========================================================================
# =========================================================================
 
# INIT
setwd("//e75a0679/sakta/UTV/SU/Program/Analys/Boosting GLM/Boosting-GLM")

source("load_packages.r")
load_packages(updateR = FALSE)

# Output parameters --------------------------------------------------

date <- "20220403"

for (data in c( "auspriv","freMTPL","beMTPL", "norauto")){ #  REAL 
rm(list = setdiff(ls(),c("data","date")))

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
learning_rate_mu <- 0.01

train_frac <- 0.8
cal_frac <- 0.3 # Of the training data, how much for calibration of marginal effects?


#source("modeling.r")
source("vis_and_eval.r")

}
 

 
 