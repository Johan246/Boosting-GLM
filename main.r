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
suffix <- "CAS"
date <- "raw_20220317"
REAL_data <- FALSE

new_models <- FALSE
new_univar_effects <- FALSE
new_twoway_effects <- FALSE
save <- FALSE
scoring_boosting_factors <- FALSE

plot_folder <- paste("Plottar/",suffix,"_",date, sep="")
dir.create(plot_folder)
n_sim <- 650000


# Algo parameters ---------------------------------------------------------

plot_during_algo <- FALSE
n_trees_mu <- 1000
tree_depth_mu <- 2
learning_rate_mu <- 0.025

# Factors of interest

if (REAL_data== TRUE){
facts <- c("BOYTA", 
           "Bolag" , 
           "Fast_alder" , 
           "Byggnadstyp" , 
           "Brevobjekt" , 
           "Alder" )
 
}else{
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

# Data ----------------------------------------------------------------

df <- list()
train_frac <- 0.8
cal_frac <- 0.3 # Of the training data, how much for calibration of marginal effects?
 

### IMPORT CAS ###
if (REAL_data == FALSE){
  
  data_input <- read.csv("//e75a0679/sakta/UTV/SU/Program/Analys/Boosting GLM/Boosting-GLM/Data/freMTPL2freq.csv") 
  
  data_idx <- sample(c(1:dim(data_input)[1] ), size = n_sim, replace = FALSE)
  df$all <- as.data.frame(data_input[data_idx,])
  
  
  # Corrections according to Schelldorfer and W?thrich (2019)
   df$all$ClaimNb <-  pmin( df$all$ClaimNb,4)
   df$all$Exposure <- pmin( df$all$Exposure,1) 
  
  
  # Naming to fit algo
  colnames( df$all)[2] <- "freq"
  colnames( df$all)[3] <- "dur"
  
  # Factors
   df$all$Area <- as.factor( df$all$Area)
   df$all$VehBrand  <- as.factor( df$all$VehBrand)
   df$all$VehGas    <- as.factor( df$all$VehGas)
   df$all$Region    <- as.factor( df$all$Region)
  
  # Correcting n
   df$all$freq <- as.numeric( df$all$freq)
  
  # Data splits
  n_obs <-  df$all$freq[1:n_sim]
  
  
  df$train <- df$all[1:(n_sim*train_frac*(1-cal_frac)), ]
  df$cal <- df$all[(n_sim*train_frac*(1-cal_frac)+1):(n_sim*train_frac), ]
  df$test <- df$all[(n_sim*train_frac +1):n_sim, ]
  
  
  } 

### IMPORT REAL ###
if (REAL_data == TRUE){
  source("//e75a0679/sakta/UTV/SU/Program/Analys/Pricing_ML/import_och_tvatt.r")
  library("haven")
  data_input <- import_och_tvatt("//e75a0679/sakta/UTV/SU/DATA/Tariffanalysdata/df_vb_alla_skador_20170101.sas7bdat",divtest="N")[[3]]
  
  # Variable selction gross
  data_input <- data_input %>% dplyr::select(c(freq,BOYTA, Bolag , Fast_alder , Byggnadstyp , Brevobjekt , Alder, dur))
  
  data_idx <- sample(c(1:dim(data_input)[1] ), size = n_sim, replace = FALSE)
  
  df$all <- as.data.frame(data_input[data_idx,])
  
  # factors 
  df$all$Bolag <- as.factor(df$all$Bolag)
  df$all$Byggnadstyp <- as.factor(df$all$Byggnadstyp)
  df$all$Brevobjekt <- as.factor(df$all$Brevobjekt)
  
  df$train <- df$all[1:(n_sim*train_frac*(1-cal_frac)), ]
  df$cal <- df$all[(n_sim*train_frac*(1-cal_frac)+1):(n_sim*train_frac), ]
  df$test <- df$all[(n_sim*train_frac +1):n_sim, ]
  
} 




# Helpfunctions -----------------------------------------------------------

# Deviance loss
deviance <- function(pred_mu, pred_phi=NULL, y, res){
  
  # Init
  dev <- rep(0,length(y))
  
  # Deviance calculations
  for (i in 1:length(y)){
    if (y[i] == 0){
      dev[i] <- 2*pred_mu[i]
    }
    else {
      dev[i] <- 2*(pred_mu[i] - y[i]  +  y[i]  * log(y[i]/pred_mu[i])) 
    }
  }
  
  # Scaling deviance if wanted
  if (!is.null(pred_phi)) {
    #print("Scaled deviance")
    
    dev <- dev / pred_phi
  } else {
    #print("Unscaled deviance")
  }
  
  if (res==TRUE){
    return(dev)
  }
  else{
    return(sum(dev))
  }
  
}


# Helpvariables -----------------------------------------------------------

num_facts <-  colnames(df$test)[sapply(df$test,class) %in% c("numeric","integer")]
num_facts <- num_facts[!num_facts %in% c("freq","dur","IDpol")]
cat_facts <-  colnames(df$test)[sapply(df$test,class) =="factor" ] 



# ======================================================================
                           ## MODELS ##
# ======================================================================

  models <- list()
  pred <- list()
  boosting_df <- list()
  
  
  if (REAL_data == FALSE){
  # MU
  model.freq_gbm.ref <- formula(freq ~  Area	+ 
                                    VehPower	+ 
                                    VehAge +	
                                    DrivAge + 
                                    VehBrand+ 	
                                    VehGas	+
                                    Density+ 	
                                    Region+ 
                                    BonusMalus   +
                                    offset(log(dur)))
  
  
  model.freq_glm.tariff <- formula( freq ~  Area+ 
                               VehPower	+ 
                               VehAge +
                               DrivAge + 
                               VehBrand+ 
                               BonusMalus +	
                               VehGas	+ 
                               Density+ 
                               Region )
  
  
  model.freq_gbm.boost <- formula( freq ~  Area	+ 
                                   VehPower	+ 
                                   VehAge +
                                   DrivAge + 
                                   VehBrand+ 
                                   BonusMalus +	
                                   VehGas	+ 
                                   Density+ 
                                   Region + 
                                   offset(log(init_pred))) # Note that the original predictor (init_pred) contain duration as offset hence not needed here
  
  }
  
  
  if (REAL_data == TRUE){
  # MU
  model.freq_glm.tariff <- formula( freq ~  BOYTA + 
                                      Bolag + 
                                      Fast_alder + 
                                      Byggnadstyp + 
                                      Brevobjekt +
                                      Alder)
  
  model.freq_gbm.ref <- formula( freq ~ BOYTA + 
                                   Bolag + 
                                   Fast_alder + 
                                   Byggnadstyp + 
                                   Brevobjekt + 
                                   Alder  + 
                                   offset(log(dur)))
  
  model.freq_gbm.boost <- formula( freq ~ BOYTA + 
                                     Bolag +
                                     Fast_alder +
                                     Byggnadstyp +
                                     Brevobjekt +
                                     Alder  +  
                                     offset(log(init_pred))) # Note that the original predictor (init_pred) contain duration as offset hence not needed here
}
  
  if (new_models == TRUE){ 
    # INIT and modeling parameters
    
  
  
  # ======================================================================
  ## Existing tariff ##
  # ======================================================================
   # INIT - Import existing tariff --------------------------------------------
  
  # Note: In this case, a simple GLM-structure exemplifies the tariff to be boosted
  
  models$glm_init  <- glm(model.freq_glm.tariff, 
                          data = df$train,
                          offset = log(dur),
                          family = quasipoisson(link = "log"))
  
  summary(models$glm_init)
  
  # INIT - Tariff predictions -----------------------------------------------
  
  # Note: IF original tariff model is not in glm()-form, a scoring procedure that maps tariff input to policy predictions (e.g. tabular -> mu(x_i) )
  
  pred$train$init <- predict.glm(object = models$glm_init, 
                                 newdata = df$train , 
                                 type = "response") 
  
  
  pred$cal$init <- predict.glm(object = models$glm_init, 
                                newdata = df$cal , 
                                type = "response") 
  
  pred$test$init <- predict.glm(object = models$glm_init, 
                                newdata = df$test , 
                                type = "response") 
  
  # Init data for boosting algorithm 
  
  boosting_df$train <- data.frame(df$train, init_pred = pred$train$init )
  boosting_df$cal <- data.frame(df$cal, init_pred = pred$cal$init )
  boosting_df$test <- data.frame(df$test, init_pred = pred$test$init )
  
  # ======================================================================
  ## Reference model: Raw GBM 
  # ======================================================================
  
  # GBM 
  models$raw_gbm <- gbm(model.freq_gbm.ref, 
                  data = boosting_df$train, 
                  distribution = "poisson",
                  n.trees = n_trees_mu,
                  n.minobsinnode = 10,
                  interaction.depth = tree_depth_mu,
                  shrinkage = learning_rate_mu,
                  cv.folds = 5
                    )
  
  models$raw_gbm.ntrees <- gbm.perf(models$raw_gbm, method = "cv")      
  
  pred$train$ref <- predict.gbm(object = models$raw_gbm, 
                                n.trees=models$raw_gbm.ntrees, 
                                newdata = boosting_df$train , 
                                type = "response") *boosting_df$train$dur
  
  balance_factor <- mean(boosting_df$train$freq)/mean(pred$train$ref)
  
  pred$train$ref <-  pred$train$ref*balance_factor
  
  pred$test$ref <- predict.gbm(object = models$raw_gbm,
                              n.trees=models$raw_gbm.ntrees,
                              newdata = boosting_df$test , 
                              type = "response")  *boosting_df$test$dur * balance_factor
  
  
  pred$cal$ref <- predict.gbm(object = models$raw_gbm,
                               n.trees=models$raw_gbm.ntrees,
                               newdata = boosting_df$cal , 
                               type = "response")  *boosting_df$cal$dur * balance_factor
  
  # ======================================================================
  ## Training boosting GBM ##
  # ======================================================================
  
  # GBM 
  models$gbm_boost <- gbm(model.freq_gbm.boost, 
                  data = boosting_df$train, 
                  distribution = "poisson",
                  n.trees = n_trees_mu,
                  n.minobsinnode = 10,
                  interaction.depth = tree_depth_mu,
                  shrinkage = learning_rate_mu,
                  cv.folds = 5
                  )
  
  models$gbm_boost.ntrees <- gbm.perf(models$gbm_boost , method = "cv")      
  
  pred$train$boost <- predict.gbm(object = models$gbm_boost,n.trees=models$gbm_boost.ntrees, newdata = boosting_df$train , type = "response") *boosting_df$train$init_pred
  balance_factor <- mean(boosting_df$train$freq)/mean(pred$train$boost)
  pred$train$boost <- pred$train$boost*balance_factor
  pred$test$boost <- predict.gbm(object = models$gbm_boost,n.trees=models$gbm_boost.ntrees, newdata = boosting_df$test , type = "response")  *boosting_df$test$init_pred * balance_factor
  pred$cal$boost <- predict.gbm(object = models$gbm_boost,n.trees=models$gbm_boost.ntrees, newdata = boosting_df$cal , type = "response")  *boosting_df$cal$init_pred * balance_factor
  
  
  boosting_df$train <- data.frame(df$train, init_pred = pred$train$init , boost_pred = pred$train$boost )
  boosting_df$cal <- data.frame(df$cal, init_pred = pred$cal$init , boost_pred = pred$cal$boost )
  boosting_df$test <- data.frame(df$test, init_pred = pred$test$init , boost_pred = pred$test$boost )
  
  # Balance checks
  assert( round(mean(boosting_df$train$boost_pred),4) == round(mean(boosting_df$train$init_pred),4) )
  
  
  if (save == TRUE){
    save(models, file = paste("Data/Models_",suffix,".RData", sep = ""))
    save(boosting_df, file = paste("Data/Boost_data_",suffix,".RData", sep = ""))
    save(pred, file = paste("Data/Predictions_",suffix,".RData", sep = ""))
  }
}else{

    load(paste("Data/Models_",suffix,".RData", sep = ""))
    load(paste("Data/Boost_data_",suffix,".RData", sep = ""))
    load(paste("Data/Predictions_",suffix,".RData", sep = ""))
}


# ======================================================================
                    ## Extracting boosting factors ##
# ======================================================================
  
  
#### SELECT WHICH MODEL TO EXTRACT FROM ("Boost" or "raw") #### 
  
model_select <- "raw" # "Boost"
  

# PDP-functions 
pgbm <- function(object, newdata){
  mean(exp(predict.gbm(object, newdata = newdata)))
}
pgbm_raw <- function(object, newdata){
  mean(exp(predict.gbm(object, newdata = newdata))*newdata[,"dur"])
}
pglm <- function(object, newdata){
  mean(predict.glm(object, newdata = newdata, type="response"))
}

  
if (new_univar_effects == TRUE){
  # Marginal (univariate) effects -------------------------------------------
  
  univariate_pdp_data <- list()
  
  for (fact in num_facts){
  
    #for (fact in "VehPower"){
      
    
      if (is.factor(df$train[,fact]) == FALSE){ 
      
      
      xlim <- quantile(df$train[,fact], c(.01,.99)) 
      
      p1 <- partial(models$gbm_boost, pred.fun=pgbm,
                    chull = TRUE , pred.var = c(fact), n.trees= models$gbm_boost.ntrees,recursive = FALSE) 
      p2 <- partial(models$glm_init, pred.fun=pglm,
                    chull = TRUE, pred.var = c(fact))
      p3 <- partial(models$raw_gbm, pred.fun=pgbm_raw,
                    chull = TRUE, pred.var = c(fact), n.trees= models$raw_gbm.ntrees,recursive = FALSE)
      p4 <- p1$yhat*p2$yhat
      
      
      indx_plt <- which(p1[,fact] > xlim[1] & p1[,fact] < xlim[2])
      scale_factor_boosting <- mean(df$train$freq) 
      
      univariate_pdp_data[[fact]] <- data.frame(factor_val = p1[,1], 
                                                Tariff = p2$yhat[]* mean(df$train$freq)/mean(p2$yhat[]), 
                                                Boost = p1$yhat[] , 
                                                Ref_gbm = p3$yhat[]*mean(df$train$freq)/mean(p3$yhat[]), 
                                                After_boost = p4[]* mean(df$train$freq)/mean(p4[]))
      
       
        
      }
        
    }
  }
  
  
  if (save == TRUE){
    save(univariate_pdp_data, file = paste("Data/Univar_effects_",suffix,".RData", sep = ""))
  }
}else{

  load(paste("Data/Univar_effects_",suffix,".RData", sep = ""))
  
} 
  
  
# Marginal (Two-way) effects -------------------------------------------

  
## Scanning interaction effects with Friedmans H'statistics
  
if (new_twoway_effects == TRUE){
  
  if (model_select == "Boost"){effect_model <- models$gbm_boost
                                n_trees <- models$gbm_boost.ntrees }
  if (model_select == "raw"){effect_model <- models$raw_gbm
                              n_trees <- models$raw_gbm.ntrees}
  
  for (i in 1:length(facts)){
    for (j in 1:max(1,(i-1)) ){
      if (i == 1 & j ==1 ){H_stat <- data.frame()}
      
      # Friedmans H-stat f?r varje faktorkombination
      H_stat <- bind_rows(H_stat, data.frame(Faktor_1 = facts[j],
                                             Faktor_2 = facts[i],
                                             
                                             H_stat = interact.gbm(effect_model, 
                                                                   data= boosting_df$train, 
                                                                   n.trees = n_trees,
                                                                   i.var = c(facts[j],facts[i]) )))
      
      
    }
    print(paste("Done with interactions for", facts[i]))
  }
  
  png(paste(plot_folder,"/H_statistics.png",sep=""))
  ggplot(H_stat, aes(Faktor_1, Faktor_2, fill= H_stat)) + 
    geom_tile(color="white")+
    geom_text(aes(label = round(H_stat,2)), color = "white", size = 4) 
  dev.off()
  
  n_interactions <- 5    
  max_grid <- 10
    
  top_interactions <- H_stat %>% filter(H_stat <1) %>% arrange(desc(H_stat)) %>% top_n(n_interactions) %>% arrange(Faktor_1)
  
  interaction_effects <- list()
  interaction_effects$top_interactions <- top_interactions 
  
  for (i in 1:length(top_interactions$Faktor_1)) {
  
    if (top_interactions$Faktor_1[i] %in% num_facts & top_interactions$Faktor_2[i] %in% num_facts){max_grid <- 10}
      else{max_grid <-  min(nrow(unique(boosting_df$train[top_interactions$Faktor_1[i]] )), nrow(unique(boosting_df$train[top_interactions$Faktor_2[i]] )))}
    
    interaction_effects[[top_interactions$Faktor_1[i]]][[top_interactions$Faktor_2[i]]] <- partial(
                  effect_model, pred.fun=pgbm, pred.var = c(top_interactions$Faktor_1[i], top_interactions$Faktor_2[i]),
                  n.trees= n_trees,recursive = FALSE,
                  chull = TRUE,
                  grid.resolution = max_grid)
  
  
  }
  
  
    if (save == TRUE){
      if(model_select == "Boost"){ save(interaction_effects, file = paste("Data/Interaction_effects_",suffix,".RData", sep = ""))}
      if(model_select == "raw"){ save(interaction_effects, file = paste("Data/Interaction_effects_",suffix,"_raw.RData", sep = ""))}
    }
    
    
  }else{
    
    
    if(model_select == "Boost"){ load(paste("Data/Interaction_effects_",suffix,".RData", sep = ""))}
    if(model_select == "raw"){ load(paste("Data/Interaction_effects_",suffix,"_raw.RData", sep = ""))}
    
  } 


  # Testing boosting factors  -----------------------------------------------

if (scoring_boosting_factors == TRUE){  
    
  factor_update <- list()
  old_factor_effect <- list() 
  factor_levels <- list()
  new_factor <- list()
  univariate_pdp_data_complete <- list()
  interaction_effects_complete <- list()
  
  train_factors_new <- data.frame(matrix(rep(1,nrow(boosting_df$train)*(length(num_facts)+2) ), nrow = nrow(boosting_df$train)))
  cal_factors_new <- data.frame(matrix(rep(1,nrow(boosting_df$cal)*(length(num_facts)+2) ), nrow = nrow(boosting_df$cal)))
  test_factors_new <-data.frame(matrix(rep(1,nrow(boosting_df$test)*(length(num_facts)+2) ), nrow = nrow(boosting_df$test)))
  
  names(train_factors_new) <- c(num_facts, "init_pred","freq")
  names(cal_factors_new) <- c(num_facts, "init_pred","freq")
  names(test_factors_new) <- c(num_facts, "init_pred","freq")
  
  
  ## Univariate
  for (fact in num_facts){
    if (model_select == "Boost") {model_name <- "Boost"}
    if (model_select == "raw") {model_name <- "Ref_gbm"}
    # Factor range
    range <- min(df$all[fact]):max(df$all[fact])
    
    univariate_pdp_data[[fact]]$factor_val <- round(univariate_pdp_data[[fact]]$factor_val) # OBS: Hanterar endast factorer med niv?er n?ra heltal!
    
    
    factor_update[[fact]] <- as.numeric( approx(univariate_pdp_data[[fact]]$factor_val, 
                                                univariate_pdp_data[[fact]][[model_name]], 
                                                xout=range)[["y"]])
    
 
    factor_update[[fact]][is.na(factor_update[[fact]])] <- 1
    # Saving interpolated PDP-values
    univariate_pdp_data_complete[[fact]] <- data.frame(factor_val = range, 
                                                       Ref_gbm_interpol =  factor_update[[fact]] )
    
    
    train_factors_new[fact] <- factor_update[[fact]][boosting_df$train[[fact]]  - min(range) + 1 ]
    cal_factors_new[fact] <- factor_update[[fact]][boosting_df$cal[[fact]]  - min(range) + 1 ]
    test_factors_new[fact] <- factor_update[[fact]][boosting_df$test[[fact]]  - min(range) + 1 ]
  } 
   
  
  ## Interactions
  for (k in 1:length(top_interactions$Faktor_1)){
    
    fact_1 <- top_interactions$Faktor_1[k]
    fact_2 <- top_interactions$Faktor_2[k]
    
    temp_interaction_effects <- interaction_effects[[fact_1]][[fact_2]]
    temp_interaction_effects <- data.frame(temp_interaction_effects,idx=(1:nrow(temp_interaction_effects)) )
    
    # Factor range
    if (is.numeric(df$all[[top_interactions$Faktor_1[k]]])){
      range1 <- min(df$all[top_interactions$Faktor_1[k]]):max(df$all[top_interactions$Faktor_1[k]])
    }else{
      range1 <- unique(df$all[top_interactions$Faktor_1[k]])
    }
    
    if (is.numeric(df$all[[top_interactions$Faktor_2[k]]])){
      range2 <- min(df$all[top_interactions$Faktor_2[k]]):max(df$all[top_interactions$Faktor_2[k]])
      }else{
        range2 <- unique(df$all[[top_interactions$Faktor_2[k]]])
          }
      
    range_grid <- data.frame(expand.grid(range1,range2), pred = NA )
     
    
     
    
    for (i in 1: nrow(range_grid)){
      if (is.numeric(df$all[[top_interactions$Faktor_1[k]]])){
        temp_fact1_val <- temp_interaction_effects[[fact_1]][which.min(abs(temp_interaction_effects[[fact_1]] - range_grid[i,"Var1"]))]
        temp_interaction_effects_sub  <- temp_interaction_effects[temp_interaction_effects[[fact_1]] == temp_fact1_val,]
      }else{
        temp_interaction_effects_sub  <- temp_interaction_effects[temp_interaction_effects[[fact_1]] == range_grid[i,"Var1"],]
      }
      
      if (is.numeric(df$all[[top_interactions$Faktor_2[k]]])){
        range_grid[i,"pred"] <- temp_interaction_effects_sub$yhat[which.min(abs(temp_interaction_effects_sub[[fact_2]] - range_grid[i,"Var2"]))]
      }else{
        range_grid[i,"pred"] <- temp_interaction_effects_sub$yhat[which(temp_interaction_effects_sub[[fact_2]] == range_grid[i,"Var2"])]
      }
    }
    
    
    inter_fact_name <-  paste(fact_1,fact_2,sep="")
    for (i in 1:nrow(boosting_df$train)){
      
      train_factors_new[i,inter_fact_name] <-  (range_grid[range_grid$Var1 == boosting_df$train[[fact_1]][i],] %>% filter(Var2 == boosting_df$train[[fact_2]][i]))$pred
      if (any(i == round((nrow(boosting_df$train)/(2:10) )))){
        print(paste("Done with ",percent(i/nrow(boosting_df$train))," of training" ))
      }
    }
    
    for (i in 1:nrow(boosting_df$cal)){
      
      cal_factors_new[i,inter_fact_name] <-  (range_grid[range_grid$Var1 == boosting_df$cal[[fact_1]][i],] %>% filter(Var2 == boosting_df$cal[[fact_2]][i]))$pred
      
      if (any(i == round((nrow(boosting_df$cal)/(2:10) )))) {
        print(paste("Done with ",percent(i/nrow(boosting_df$cal))," of calibration" ))
      }
    }
    
    for (i in 1:nrow(boosting_df$test)){
      
      test_factors_new[i,inter_fact_name] <-  (range_grid[range_grid$Var1 == boosting_df$test[[fact_1]][i],] %>% filter(Var2 == boosting_df$test[[fact_2]][i]))$pred
      if (any(i == round((nrow(boosting_df$test)/(2:10) )))){
        print(paste("Done with ",percent(i/nrow(boosting_df$test))," of test" ))
      }
    }
     
    
    print(paste("Done with interaction between ", fact_1, " and ", fact_2))
    
  } 
   
  
  # Adding init, dur and y-values
  train_factors_new["freq"] <- boosting_df$train$freq
  cal_factors_new["freq"] <- boosting_df$cal$freq
  test_factors_new["freq"] <- boosting_df$test$freq
  
  # Adding init, dur and y-values
  train_factors_new["dur"] <- boosting_df$train$dur
  cal_factors_new["dur"] <- boosting_df$cal$dur
  test_factors_new["dur"] <- boosting_df$test$dur
  
  if(model_select =="Boost"){
    train_factors_new["init_pred"] <- boosting_df$train$init
    cal_factors_new["init_pred"] <- boosting_df$cal$init
    test_factors_new["init_pred"] <-boosting_df$test$init
      } 
  
  if(model_select =="raw"){
    train_factors_new["init_pred"] <- mean(boosting_df$train$freq)
    cal_factors_new["init_pred"] <-  mean(boosting_df$train$freq)
    test_factors_new["init_pred"] <- mean(boosting_df$train$freq)
    }
  
   
  boosting_df$train_factors <- train_factors_new
  boosting_df$cal_factors <- cal_factors_new
  boosting_df$test_factors <- test_factors_new
   
  
  if (save == TRUE){
    if(model_select == "Boost"){save(boosting_df, file=paste("Data/Boost_data_",suffix,".RData", sep = ""))}
    if(model_select == "raw"){save(boosting_df, file=paste("Data/Boost_data_",suffix,"_raw.RData", sep = ""))}
    
  }
}else{
  
  if(model_select == "Boost"){load(paste("Data/Boost_data_",suffix,".RData", sep = ""))}
  if(model_select == "raw"){load(paste("Data/Boost_data_",suffix,"_raw.RData", sep = ""))}
  
}  

# Bias regularisation of boosting factors  -----------------------------------------------

tree_control = rpart.control(minbucket=10, cp=0.00001)

all_facts <- names(boosting_df$train_factors %>% dplyr::select(-c("init_pred","freq","dur")))

boosting_df$train_factors_final <- boosting_df$train_factors
boosting_df$cal_factors_final <- boosting_df$cal_factors
boosting_df$test_factors_final <-  boosting_df$test_factors


boosting_df$train_factors_final_pdp <- boosting_df$train_factors
boosting_df$cal_factors_final_pdp <- boosting_df$cal_factors
boosting_df$test_factors_final_pdp <-  boosting_df$test_factors

## CALIBRATION TO Y

for (fact in all_facts){
  
  tree_deep <- rpart(formula(eval(paste("freq ~ ", fact," + offset(init_pred)", sep=""))), 
                      data = boosting_df$train_factors, 
                      method = "anova",
                      control = tree_control )
  
  
  # Pruning
  prune_cp_glm <- tree_deep$cptable[which.min(tree_deep$cptable[, "xerror"]), "CP"]
  tree_temp <- prune.rpart(tree_deep, cp = prune_cp_glm)
 
  sort(unique(boosting_df$train_factors$VehAge))
  sort(unique(univariate_pdp_data[["VehAge"]]$Ref_gbm))
  
  
  
  ## Updating final factors
  boosting_df$train_factors_final[fact] <- predict(tree_temp, newdata = boosting_df$train_factors, newoffset=boosting_df$train_factors$init_pred)
  boosting_df$cal_factors_final[fact] <- predict(tree_temp, newdata = boosting_df$cal_factors, newoffset=boosting_df$cal_factors$init_pred)
  boosting_df$test_factors_final[fact] <- predict(tree_temp, newdata = boosting_df$test_factors, newoffset=boosting_df$test_factors$init_pred)
  
  # Updating PDP-values
  if (fact %in% facts) { 
  temp_data <- data.frame(univariate_pdp_data_complete[[fact]]$Ref_gbm_interpol , init_pred = mean(boosting_df$train$freq))
  names(temp_data) <- c(fact, "init_pred")
  
  univariate_pdp_data_complete[[fact]]$Final_model <- mean(boosting_df$train$freq) + predict(tree_temp, newdata = temp_data)
  }
  
  

} 


# Adding back categoricals

boosting_df$train_factors_final <- data.frame(boosting_df$train_factors_final, boosting_df$train[, cat_facts])
boosting_df$cal_factors_final <- data.frame(boosting_df$cal_factors_final, boosting_df$cal[, cat_facts])
boosting_df$test_factors_final <- data.frame(boosting_df$test_factors_final, boosting_df$test[, cat_facts])


# Only include factors that have at least 2 levels --->>> MANUAL STEP AT THIS TIME!!
 
final_factors <- apply(boosting_df$train_factors_final,2, FUN= function(x) length(unique(x)))   
#final_factors_pdp <- apply(boosting_df$train_factors_final_pdp,2, FUN= function(x) length(unique(x)))   

#### Balancing with GLM - Training glm on retrieved factor effects  
# Vanilla GLM 

model.freq_glm.final <- formula(freq ~   factor(VehPower) +  factor(VehAge) + factor(DrivAge) + factor(BonusMalus) + factor(Density) + 
                                    factor(Area) + factor(VehBrand) + factor(VehGas) + factor(Region) +
                                  factor(DrivAgeBonusMalus) + factor(VehAgeVehBrand) +factor(VehAgeVehGas)  + factor(VehPowerVehBrand) + factor(DrivAgeRegion) + 
                                  offset(log(dur)))

models$final$vanilla  <- glm(model.freq_glm.final, 
                                data = boosting_df$train_factors_final, 
                                family = quasipoisson(link = "log"))
 
# No interactions
model.freq_glm.final_no_inter <- formula(freq ~    factor(VehPower) +  factor(VehAge) + factor(DrivAge) + factor(BonusMalus) + factor(Density) + 
                                           factor(Area) + factor(VehBrand) + factor(VehGas) + factor(Region) +
                                  offset(log(dur)))


models$final$vanilla_no_inter  <- glm(model.freq_glm.final_no_inter, 
                             data = boosting_df$train_factors_final, 
                             family = quasipoisson(link = "log"))


# Lasso GLM

model.freq_glm.final_lasso <- formula(freq ~   factor(VehPower) +  factor(VehAge) + factor(DrivAge) + factor(BonusMalus) + factor(Density) + factor(DrivAgeRegion) + 
                                        factor(Area) + factor(VehBrand) + factor(VehGas) + factor(Region) +
                                  factor(DrivAgeBonusMalus) + factor(VehAgeVehBrand) + factor(VehAgeVehGas)  + factor(VehPowerVehBrand) 
                                 )

glmnet_data <- model.matrix(model.freq_glm.final_lasso , boosting_df$train_factors_final )
glmnet_data_y <- as.matrix(boosting_df$train_factors_final %>% dplyr::select(c(freq,dur)))
 

models$final$lasso  <- cv.glmnet(x = glmnet_data, 
                              y = glmnet_data_y[,"freq"], 
                              intercept=T ,
                              offset = log(glmnet_data_y[,"dur"]),
                             family = poisson(link = "log"),
                             alpha = 1,
                             nfolds = 5, 
                             lambda = seq(0, 0.1, length.out=100))
 
# Finalizing factors
plot(models$final$lasso)

best_lambda <- unique(models$final$lasso$lambda[min(models$final$lasso$cvm) == models$final$lasso$cvm])
  gfdsg

 
##### Finalizing factor
 
coef(models$final$lasso) 
summary(models$final$vanilla)
summary(models$final$vanilla_no_inter)

# Finalizing predictions 

pred$train$boosted_glm$vanilla <- as.numeric(predict.glm(models$final$vanilla, newdat=boosting_df$train_factors_final, type="response", newoffset=boosting_df$train_factors_final$dur))   
pred$cal$boosted_glm$vanilla <- sapply(as.numeric(predict.glm(models$final$vanilla, newdat=boosting_df$cal_factors_final, type="response"), newoffset=boosting_df$cal_factors_final$dur), function(x) min(x,2)) # Ugly fix of one extreme value
pred$test$boosted_glm$vanilla <- sapply(as.numeric(predict.glm(models$final$vanilla, newdat=boosting_df$test_factors_final, type="response") , newoffset=boosting_df$test_factors_final$dur ) , function(x) min(x,2))

pred$train$boosted_glm$vanilla_no_inter <- as.numeric(predict.glm(models$final$vanilla_no_inter, newdat=boosting_df$train_factors_final, type="response"), newoffset=boosting_df$train_factors_final$dur)   
pred$cal$boosted_glm$vanilla_no_inter <- as.numeric(predict.glm(models$final$vanilla_no_inter, newdat=boosting_df$cal_factors_final, type="response")  , newoffset=boosting_df$cal_factors_final$dur )
pred$test$boosted_glm$vanilla_no_inter <- as.numeric(predict.glm(models$final$vanilla_no_inter, newdat=boosting_df$test_factors_final, type="response") , newoffset=boosting_df$test_factors_final$dur ) 


pred$train$boosted_glm$lasso <- as.numeric(predict(models$final$lasso,  newx = model.matrix(model.freq_glm.final_lasso , boosting_df$train_factors_final ), type = "response",  s = best_lambda, newoffset = log(boosting_df$train_factors_final$dur) ))
pred$cal$boosted_glm$lasso <- as.numeric(predict(models$final$lasso, newx = model.matrix(model.freq_glm.final_lasso , boosting_df$cal_factors_final ), type = "response", s = best_lambda, newoffset = log(boosting_df$cal_factors$dur)))
pred$test$boosted_glm$lasso <- as.numeric(predict(models$final$lasso, newx = model.matrix(model.freq_glm.final_lasso , boosting_df$test_factors_final ), type = "response", s = best_lambda, newoffset = log(boosting_df$test_factors$dur))) 
  
 
mean(boosting_df$train$freq)
mean(pred$train$boosted_glm$vanilla)
mean(pred$train$boosted_glm$vanilla_no_inter)
mean(pred$train$boosted_glm$lasso)

 
###### Updating PDP according to lasso selections ######


## Extracting lasso values
row_names <- rownames(coef(models$final$lasso))
factors <- sub(".*\\((.*)\\).*", "\\1", row_names)
factor_values <- sub(".*\\)(.*)$", "\\1", row_names)
coef_values <- coef(models$final$lasso)
coef_values <- as.numeric(as.matrix(coef_values))
coef_values[is.na(coef_values)] <- 0

 
for (fact in num_facts){ 
  
   temp_pdp_val <- round(univariate_pdp_data[[fact]]$Final_model  -   mean(boosting_df$train$freq),6)
   temp_coef_level <- round(as.numeric(factor_values[which(factors==fact) ]),6)
   temp_coef_val <- round(as.numeric(coef_values[which(factors==fact) ]),6)
   
   
   temp_new_pdp_val <- rep(0, length(temp_pdp_val))
   for (i in 1:length(temp_pdp_val)){ 
      if( any(temp_pdp_val[i] == temp_coef_level)){
       idx <- which(temp_pdp_val[i] == temp_coef_level)
        
        temp_new_pdp_val[i] <- temp_coef_val[idx]
      }
     }
       

   univariate_pdp_data[[fact]]$Final_model_lasso <- exp(temp_new_pdp_val)*mean(boosting_df$train$freq)
   }
  
  
  
# FINAL: Normalizing PDPs for figures

# Normalising new PDPs 

for  (fact in num_facts){
  univariate_pdp_data[[fact]]$Final_model <-  univariate_pdp_data[[fact]]$Final_model/mean( univariate_pdp_data[[fact]]$Final_model)*mean( univariate_pdp_data[[fact]]$Ref_gbm)
  univariate_pdp_data[[fact]]$Final_model_lasso <-  univariate_pdp_data[[fact]]$Final_model_lasso/mean( univariate_pdp_data[[fact]]$Final_model_lasso)*mean( univariate_pdp_data[[fact]]$Ref_gbm)
  #univariate_pdp_data[[fact]]$Final_model_pdp <-  univariate_pdp_data[[fact]]$Final_model_pdp/mean( univariate_pdp_data[[fact]]$Final_model_pdp)*mean( univariate_pdp_data[[fact]]$Ref_gbm)
}
 

if (save == TRUE){
  save(models, file = paste("Data/Models_",suffix,".RData", sep = ""))
  save(boosting_df, file = paste("Data/Boost_data_",suffix,".RData", sep = ""))
  save(pred, file = paste("Data/Predictions_",suffix,".RData", sep = ""))
  save(univariate_pdp_data, file = paste("Data/PDP_uni_",suffix,".RData", sep = ""))
}




# Updated model factors
 
for (fact in num_facts){
   
  p <-  univariate_pdp_data[[fact]] %>% 
      ggplot(aes(x=factor_val))+
      geom_line(aes(y=Tariff, color="black")) + 
      geom_line(aes(y=Final_model, color="red"))+
      geom_line(aes(y=Ref_gbm, color="grey"), lty=2) +
      geom_line(aes(y=Final_model_lasso, color="blue"))+
      geom_abline(intercept = mean(df$train$freq),slope=0, color="grey", alpha=0.5)+
      #xlim(xlim[1],xlim[2])+
      labs(x= fact,
           y="PDP" )+
     scale_colour_manual(name = '', 
                      values =c('black'='black','red'='red','grey'='grey','blue'='blue'), 
                      labels = c('Linear','Final GLM','GBM (PDP)','After trees')
                          )+
      scale_y_continuous(sec.axis = sec_axis( trans= ~./mean(df$train$freq), name="Boosting factor")) + 
      theme_classic() +
        theme(legend.position ="bottom")
  
  ggsave(filename = paste(plot_folder,"/PDP_boost_",fact , ".png",sep=""), plot = p, dpi = 300,width = 10, height = 8)
     
}   


# Predictive performance --------------------------------------------------

MSEP_cal <- data.frame( Model=c("Intercept","Reference", "Boosted", "Init-tariff", "Boosted_GLM_vanilla", "Boosted_GLM_no_interactions", "Boosted_GLM_lasso"),
                         
                         
                         MSEP=round(c( mean((mean(boosting_df$train$freq) - boosting_df$cal$freq) ^2), 
                                 mean((pred$cal$ref - boosting_df$cal$freq) ^2), 
                                 mean((pred$cal$boost - boosting_df$cal$freq) ^2),
                                 mean((pred$cal$init - boosting_df$cal$freq) ^2),
                                 mean((pred$cal$boosted_glm$vanilla - boosting_df$cal$freq)^2),
                                 mean((pred$cal$boosted_glm$vanilla_no_inter - boosting_df$cal$freq)^2),
                                 mean((pred$cal$boosted_glm$lasso - boosting_df$cal$freq)^2)),4),
                         
                         Deviance = round(c(deviance(rep(mean(boosting_df$train$freq),length(boosting_df$cal$freq)) , pred_phi=NULL, boosting_df$cal$freq, res = FALSE),
                                      deviance(pred$cal$ref, pred_phi=NULL, boosting_df$cal$freq, res = FALSE),
                                      deviance(pred$cal$boost, pred_phi=NULL, boosting_df$cal$freq, res = FALSE),
                                      deviance(pred$cal$init, pred_phi=NULL, boosting_df$cal$freq, res = FALSE),
                                      deviance(pred$cal$boosted_glm$vanilla , pred_phi=NULL, boosting_df$cal$freq, res = FALSE),
                                      deviance(pred$cal$boosted_glm$vanilla_no_inter , pred_phi=NULL, boosting_df$cal$freq, res = FALSE),
                                      deviance(pred$cal$boosted_glm$lasso , pred_phi=NULL, boosting_df$cal$freq, res = FALSE)),4),
                                      
                                      
                                      
                        Fidelity= round(c(0,
                                          cor(pred$cal$ref, pred$cal$ref),
                                          cor(pred$cal$boost,pred$cal$ref),
                                          cor(pred$cal$init, pred$cal$ref),
                                          cor(pred$cal$boosted_glm$vanilla , pred$cal$ref),
                                          cor(pred$cal$boosted_glm$vanilla_no_inter ,pred$cal$ref),
                                          cor(pred$cal$boosted_glm$lasso ,pred$cal$ref)),2)
           )



MSEP_test <- data.frame( Model=c("Intercept","Reference", "Boosted", "Init-tariff","Boosted_GLM_vanilla", "Boosted_GLM_no_interactions","Boosted_GLM_lasso"),
                    
                    
                    MSEP=round(c( mean((mean(boosting_df$train$freq) - boosting_df$test$freq) ^2),
                            mean((pred$test$ref - boosting_df$test$freq) ^2), 
                            mean((pred$test$boost - boosting_df$test$freq) ^2),
                            mean((pred$test$init - boosting_df$test$freq) ^2),
                            mean((pred$test$boosted_glm$vanilla - boosting_df$test$freq) ^2),
                            mean((pred$test$boosted_glm$vanilla_no_inter - boosting_df$test$freq) ^2),
                            mean((pred$test$boosted_glm$lasso - boosting_df$test$freq) ^2)),4),
                    
                    Deviance = round(c(deviance(rep(mean(boosting_df$train$freq),length(boosting_df$cal$freq)) , pred_phi=NULL, boosting_df$test$freq, res = FALSE),
                                 deviance(pred$test$ref, pred_phi=NULL, boosting_df$test$freq, res = FALSE),
                                 deviance(pred$test$boost, pred_phi=NULL, boosting_df$test$freq, res = FALSE),
                                 deviance(pred$test$init, pred_phi=NULL, boosting_df$test$freq, res = FALSE),
                                 deviance(pred$test$boosted_glm$vanilla , pred_phi=NULL, boosting_df$test$freq, res = FALSE),
                                 deviance(pred$test$boosted_glm$vanilla_no_inter , pred_phi=NULL, boosting_df$test$freq, res = FALSE),
                                 deviance(pred$test$boosted_glm$lasso , pred_phi=NULL, boosting_df$test$freq, res = FALSE)),4) ,
                                 
                      
                    Fidelity= round(c(0,
                                      cor(pred$test$ref, pred$test$ref),
                                      cor(pred$test$boost,pred$test$ref),
                                      cor(pred$test$init, pred$test$ref),
                                      cor(pred$test$boosted_glm$vanilla , pred$test$ref),
                                      cor(pred$test$boosted_glm$vanilla_no_inter ,pred$test$ref),
                                      cor(pred$test$boosted_glm$lasso ,pred$test$ref)),2)
                    )
                    
 
MSEP_test$MSEP <- round(MSEP_test$MSEP/MSEP_test$MSEP[2],4)
MSEP_cal$MSEP <- round(MSEP_cal$MSEP/MSEP_cal$MSEP[2],4)
MSEP_test$Deviance <- round(MSEP_test$Deviance/MSEP_test$Deviance[2],4)
MSEP_cal$Deviance <- round(MSEP_cal$Deviance/MSEP_cal$Deviance[2],4)

 

png(paste(plot_folder,"/Model_evaluation_test.png",sep="") )
ggplot(tibble(x=0,y=0, tb=list(MSEP_test ))) +
  theme_void() + 
  geom_table(aes(x, y, label = tb),parse = TRUE)  
dev.off()
# All predictions
 
pred_data <- data.frame(model="1.Initial tariff", pred = pred$test$init ,dur =  boosting_df$test$dur, obs= boosting_df$test$freq) %>%
  bind_rows(  data.frame(model="2.Boosted GLM (vanilla)", pred = pred$test$boosted_glm$vanilla ,dur =  boosting_df$test$dur, obs= boosting_df$test$freq)) %>%
  bind_rows(  data.frame(model="4.GBM (reference)" , pred = pred$test$ref,dur =  boosting_df$test$dur, obs= boosting_df$test$freq)) %>%
  bind_rows(  data.frame(model="5.Boosted (GBM) tariff", pred = pred$test$boost ,dur =  boosting_df$test$dur, obs= boosting_df$test$freq)) %>%
  bind_rows(  data.frame(model="3.Boosted GLM (lasso)",  pred= pred$test$boosted_glm$lasso , dur =  boosting_df$test$dur, obs= boosting_df$test$freq) )


# MU


png(paste(plot_folder,"/Predictions.png",sep=""))

pred_data %>% 
  group_by(model) %>%
  arrange(pred, .by_group = TRUE) %>%
  mutate(bin = ceiling( (cumsum(dur)/sum(dur)*100) )) %>%
  group_by(model,bin) %>%
  summarise(
    pred  = mean(pred),
    obs = mean(obs)
  ) %>%
  ggplot(aes(x=bin)) + 
  geom_point(aes(y=obs)) +  
  geom_line(aes(y=pred), linewidth=1, colour="red" ) +
  theme_classic() +
  facet_grid(model ~. )

dev.off()

# Game 

game_data <- data.frame(init= pred$test$init, 
                        boosted_glm_vanilla =  pred$test$boosted_glm$vanilla,
                        boosted_glm_lasso =  pred$test$boosted_glm$lasso,
                        boosted_gbm = pred$test$boost,
                        ref_gbm = pred$test$ref,
                        dur =  boosting_df$test$dur,
                        obs =   boosting_df$test$freq
                        )
                        

png(paste(plot_folder,"/Game_boosted_vs_ref.png",sep=""))                      
 game_data %>% mutate(premium_boosted = cumsum(boosted_glm_vanilla*(boosted_glm_vanilla < ref_gbm)),
                     profit_boosted_glm_vanilla = cumsum((boosted_glm_vanilla - obs)*(boosted_glm_vanilla < ref_gbm)),
                     premium_ref = cumsum(ref_gbm*(boosted_glm_vanilla > ref_gbm)),
                     profit_ref = cumsum((ref_gbm - obs)*(boosted_glm_vanilla > ref_gbm)),
                     id=row_number()/n())     %>%
  
  ggplot(aes(x=id))+
  geom_line(aes(y=profit_boosted_glm_vanilla, colour="blue") )+
  geom_line(aes(y=profit_ref, colour="red") )+
  theme_classic()+
  theme(legend.position = c(.8, .5))+
  labs(
        y="Profit (accumulated)",
        x="% of portfolio (lowest to highest risk)", color="",shape="" ) +
  scale_colour_manual(name = 'model', 
                      values =c('red'='red','blue'='blue'), 
                      labels = c( 'After trees','GBM')) 
dev.off()                 


png(paste(plot_folder,"/Game_init_vs_ref.png",sep="")) 
  game_data %>% mutate(premium_init = cumsum(init*(init < ref_gbm)),
                     profit_init = cumsum((init - obs)*(init < ref_gbm)),
                     premium_ref = cumsum(ref_gbm*(init > ref_gbm)),
                     profit_ref = cumsum((ref_gbm - obs)*(init > ref_gbm)),
                     id=row_number()/n())     %>%
  
  ggplot(aes(x=id))+
  geom_line(aes(y=profit_init, colour="blue") )+
  geom_line(aes(y=profit_ref, colour="red") )+
  theme_classic()+
  theme(legend.position = c(.8, .5))+
  labs( 
        y="Profit (accumulated)",
        x="% of portfolio (lowest to highest risk)", color="",shape="" ) +
  scale_colour_manual(name = 'model', 
                      values =c('red'='red','blue'='blue'), 
                      labels = c('Linear', 'GBM')) 
dev.off()



png(paste(plot_folder,"/Game_boosted_vs_init.png",sep="")) 

  game_data %>% mutate(premium_boosted = cumsum(boosted_glm_vanilla*(boosted_glm_vanilla < init)),
                     profit_boosted_glm_vanilla = cumsum((boosted_glm_vanilla - obs)*(boosted_glm_vanilla < init)),
                     premium_init = cumsum(init*(boosted_glm_vanilla > init)),
                     profit_init = cumsum((init - obs)*(boosted_glm_vanilla > init)),
                     id=row_number()/n())     %>%
  
  ggplot(aes(x=id)) +
  geom_line(aes(y=profit_boosted_glm_vanilla, colour="red"))+
  geom_line(aes(y=profit_init, colour="blue"))+
  theme_classic()+
  theme(legend.position = c(.8, .5))+
  labs(
        y="Profit (accumulated)",
        x="% of portfolio (lowest to highest risk)", color="",shape="" ) +
  scale_colour_manual(name = 'model', 
                      values =c('red'='red','blue'='blue'), 
                      labels = c('GBM', 'Boosted GLM')) 
dev.off()

t <- game_data %>% mutate(premium_boosted = cumsum(boosted_glm_vanilla*(boosted_glm_vanilla < init)),
                     profit_boosted = cumsum((boosted_glm_vanilla - obs)*(boosted_glm_vanilla < init)),
                     premium_init = cumsum(init*(boosted_glm_vanilla > init)),
                     profit_init = cumsum((init - obs)*(boosted_glm_vanilla > init)),
                     id=row_number()/n())   



 