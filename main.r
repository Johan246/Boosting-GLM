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


for (data in c("norauto","beMTPL", "auspriv","freMTPL","REAL")){ # "norauto","beMTPL", "auspriv","freMTPL","REAL"
  rm(list = setdiff(ls(),c("data","date")))
  
  new_models <- FALSE
  new_univar_effects <- FALSE
  new_twoway_effects <- FALSE
  scoring_boosting_factors <- FALSE
  save <- TRUE
  save_last <- TRUE
  
  suffix <- data
  
  plot_folder <- paste("Plottar/",suffix,"_FINAL_",date, sep="")
  dir.create(plot_folder)
  
  # Factors
  
  # Factors of interest
  
  if (data == "REAL"){
    facts <- c("BOYTA", 
               "Bolag" , 
               "Fast_alder" , 
               "Byggnadstyp" , 
               "Brevobjekt" , 
               "Alder" )
  }
  
  if (data == "freMTPL"){
    facts <- c("Area"	, 
               "VehPower"	, 
               "VehAge" ,	
               "DrivAge" , 
               "VehBrand", 
               "VehGas",
               "Density", 
               "Region", 
               "BonusMalus" )
  }
  
  
  
  if (data == "beMTPL"){ ### OBSOBS: EXCLUDING POSTCODE!!
    facts <- c("coverage",
               "ageph" ,
               "sex",
               "bm" ,
               "power",
               "agec" ,
               "fuel",
               "use"  ,
               "fleet")
  }
  
  
  if (data == "auspriv"){
    
    facts <- c("VehValue",
               "VehAge",
               "VehBody",
               "Gender",
               "DrivAge")
    
    
  }
  
  if (data == "norauto"){
    
    facts <- c("Male",
               "Young",
               "DistLimit",
               "GeoRegion")
    
  }
  
  # Modeling hyperparameters ---------------------------------------------------------
  
  n_trees_mu <- 2000
  tree_depth_mu <- 2
  learning_rate_mu <- 0.01
  
  train_frac <- 0.8
  cal_frac <- 0.3 # Of the training data, how much for calibration of marginal effects?
  
  
  source("modeling.r")
  source("vis_and_eval.r")
  
}




