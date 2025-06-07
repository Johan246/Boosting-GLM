

print(paste("STARTING WITH DATASET: ", data,sep="" ))




# ======================================================================
## Data import ##
# ======================================================================
df <- list()

### IMPORT freMTPL ###
if (data == "freMTPL"){
  
  n_sim <- 650000
  
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
  
} 

### IMPORT REAL ###
if (data == "REAL"){
  source("//e75a0679/sakta/UTV/SU/Program/Analys/Pricing_ML/import_och_tvatt.r")
  library("haven")
  
  n_sim <- 650000
  
  data_input <- import_och_tvatt("//e75a0679/sakta/UTV/SU/DATA/Tariffanalysdata/df_vb_alla_skador_20170101.sas7bdat",divtest="N")[[3]]
  
  # Variable selction gross
  data_input <- data_input %>% dplyr::select(c(freq,BOYTA, Bolag , Fast_alder , Byggnadstyp , Brevobjekt , Alder, dur))
  
  data_idx <- sample(c(1:dim(data_input)[1] ), size = n_sim, replace = FALSE)
  
  df$all <- as.data.frame(data_input[data_idx,])
  
  # factors 
  df$all$Bolag <- as.factor(df$all$Bolag)
  df$all$Byggnadstyp <- as.factor(df$all$Byggnadstyp)
  df$all$Brevobjekt <- as.factor(df$all$Brevobjekt)
 
} 


if (data == "auspriv"){
  
  load(paste("Data/ausprivauto0405.rda", sep = "")) 
  n_sim <- nrow(ausprivauto0405)
  
  data_idx <- sample(c(1:dim(ausprivauto0405)[1] ), size = n_sim, replace = FALSE)
  df$all <- as.data.frame(ausprivauto0405[data_idx,]) %>% dplyr::select(-c("ClaimOcc","ClaimAmount"))
 
  # Naming to fit algo
  colnames( df$all)[which(colnames( df$all) == "ClaimNb")] <- "freq"
  colnames( df$all)[which(colnames( df$all) == "Exposure")]  <- "dur"
  
  # Factors
  df$all$VehAge <- as.factor( df$all$VehAge)
  df$all$VehBody  <- as.factor( df$all$VehBody)
  df$all$Gender    <- as.factor( df$all$Gender)
  df$all$DrivAge    <- as.factor( df$all$DrivAge)
  
    
}

if (data == "beMTPL"){
  
  load(paste("Data/mtpl_be.rda", sep = "")) 
  n_sim <- nrow(mtpl_be)
  
  data_idx <- sample(c(1:dim(mtpl_be)[1] ), size = n_sim, replace = FALSE)
  df$all <- as.data.frame(mtpl_be[data_idx,])  %>%  dplyr::select(-c("id"))
  
  # Naming to fit algo
  colnames( df$all)[which(colnames( df$all) == "nclaims")] <- "freq"
  colnames( df$all)[which(colnames( df$all) == "expo")]  <- "dur"  
  
  # Factors
  df$all$coverage <- as.factor( df$all$coverage)
  df$all$sex  <- as.factor( df$all$sex)
  df$all$fuel    <- as.factor( df$all$fuel)
  df$all$use    <- as.factor( df$all$use)
  df$all$fleet    <- as.factor( df$all$fleet)
   
}

if (data == "norauto"){
  
  load(paste("Data/norauto.rda", sep = "")) 
  n_sim <- nrow(norauto)
  
  data_idx <- sample(c(1:dim(norauto)[1] ), size = n_sim, replace = FALSE)
  df$all <- as.data.frame(norauto[data_idx,]) %>%  dplyr::select(-c("ClaimAmount"))
  
  # Naming to fit algo
  colnames( df$all)[which(colnames( df$all) == "NbClaim")] <- "freq"
  colnames( df$all)[which(colnames( df$all) == "Expo")]  <- "dur" 
  
  # Factors
  df$all$Male <- as.factor( df$all$Male)
  df$all$Young  <- as.factor( df$all$Young)
  df$all$DistLimit    <- as.factor( df$all$DistLimit)
  df$all$GeoRegion    <- as.factor( df$all$GeoRegion) 
  
}


# Correcting n
df$all$freq <- as.numeric( df$all$freq)

# Data splits
n_obs <-  df$all$freq[1:n_sim]

df$train <- df$all[1:(n_sim*train_frac*(1-cal_frac)), ]
df$cal <- df$all[(n_sim*train_frac*(1-cal_frac)+1):(n_sim*train_frac), ]
df$test <- df$all[(n_sim*train_frac +1):n_sim, ]


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

if (data == "freMTPL"){
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
}

if (data == "auspriv"){
  # MU
  model.freq_gbm.ref <- formula(freq ~  VehValue + 
                                  VehAge +
                                  VehBody + 
                                  Gender + 
                                  DrivAge + 
                                  offset(log(dur)))
  
  model.freq_glm.tariff <-  formula(freq ~  VehValue + 
                                      VehAge +
                                      VehBody + 
                                      Gender + 
                                      DrivAge )
}

if (data == "beMTPL"){
  # MU
  model.freq_gbm.ref <- formula(freq ~  coverage + 
                                  ageph + 
                                  sex + 
                                  bm + power +
                                  agec + 
                                  fuel +
                                  use +
                                  fleet +  
                                  offset(log(dur)))
  
  model.freq_glm.tariff <- formula(freq ~  coverage + 
                                     ageph + 
                                     sex + 
                                     bm + power +
                                     agec + 
                                     fuel +
                                     use +
                                     fleet )
}


if (data == "norauto"){
  # MU
  model.freq_gbm.ref <- formula(freq ~  Male + 
                                  Young + 
                                  DistLimit + 
                                  GeoRegion +  
                                  offset(log(dur)))
  
  model.freq_glm.tariff <- formula(freq ~  Male + 
                                     Young + 
                                     DistLimit + 
                                     GeoRegion  )
}


if (data == "REAL"){
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
 }


# Helpfunctions -----------------------------------------------------------
source("helpfunctions.r")

# ======================================================================
## MODELING ##
# ======================================================================


if (new_models == TRUE){ 
    # INIT and modeling parameters
    
  # Benchmark model --------------------------------------------------------
  
  
  # Note: In this case, a simple GLM-structure exemplifies the a naive glm-benchmark
  
  models$glm_init  <- glm(model.freq_glm.tariff, 
                          data = df$train,
                          offset = log(dur),
                          family = quasipoisson(link = "log"))
  
  summary(models$glm_init)
  
  # INIT - Tariff predictions -----------------------------------------------
   
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
  
  boosting_df$train <- data.frame(df$train)
  boosting_df$cal <- data.frame(df$cal)
  boosting_df$test <- data.frame(df$test)
  
  # ======================================================================
  ## Reference model: Raw GBM 
  # ======================================================================
  
  # GBM 
  models$raw_gbm <- gbm(model.freq_gbm.ref,
                        bag.fraction = 0.75 ,
                        data = boosting_df$train, 
                        distribution = "poisson",
                        n.trees = n_trees_mu,
                        n.minobsinnode = 10,
                        interaction.depth = tree_depth_mu,
                        shrinkage = learning_rate_mu,
                        cv.folds = 5 )
  
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
   
  
  print("Modeling done!")
  
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

# PDP-functions 
pgbm_raw <- function(object, newdata){mean(exp(predict.gbm(object, newdata = newdata))*newdata[,"dur"])}
pglm <- function(object, newdata){mean(predict.glm(object, newdata = newdata, type="response"))}

  
if (new_univar_effects == TRUE){
  # Marginal (univariate) effects -------------------------------------------
  
  print("Extracting univariate effects...")
  univariate_pdp_data <- list()
  
  for (fact in num_facts){
    
    #for (fact in "VehPower"){
      
    
      if (is.factor(df$train[,fact]) == FALSE){ 
      
        k <- min(30,length(unique(df$train[,fact])))
      
      p <- suppressMessages( partial(models$raw_gbm, 
                   pred.fun=pgbm_raw,
                   chull = TRUE, 
                   pred.var = c(fact), 
                   n.trees= models$raw_gbm.ntrees,
                   quantiles = TRUE,
                   recursive = FALSE,
                   probs = c(1:k)/k ))
     
      univariate_pdp_data[[fact]] <- data.frame(factor_val = p[,1], 
                                                Ref_gbm = p$yhat[])
      
      }
        
  }
  # Load or save data
  save(univariate_pdp_data, file = paste("Data/Univar_effects_",suffix,".RData", sep = ""))
  }else{
    load(paste("Data/Univar_effects_",suffix,".RData", sep = ""))
    } 
  
  
# Marginal (Two-way) effects -------------------------------------------

## Scanning interaction effects with Friedmans H'statistics
  
if (new_twoway_effects == TRUE){
  
  print("Extracting two-way effects...")
   effect_model <- models$raw_gbm
   n_trees <- models$raw_gbm.ntrees
  
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
   
  n_interactions <- 5    
  max_grid <- 10
    
  top_interactions <- H_stat %>% filter(H_stat <1) %>% arrange(desc(H_stat)) %>% top_n(n_interactions) %>% arrange(Faktor_1)
  
  interaction_effects <- list()
  interaction_effects$top_interactions <- top_interactions 
  
  for (i in 1:length(top_interactions$Faktor_1)) {
  
    if (top_interactions$Faktor_1[i] %in% num_facts & top_interactions$Faktor_2[i] %in% num_facts){max_grid <- 10}
      else{max_grid <-  min(nrow(unique(boosting_df$train[top_interactions$Faktor_1[i]] )), 
                            nrow(unique(boosting_df$train[top_interactions$Faktor_2[i]] )))}
    
    interaction_effects[[top_interactions$Faktor_1[i]]][[top_interactions$Faktor_2[i]]] <- suppressMessages( partial(
                  effect_model, 
                  pred.fun=pgbm_raw, 
                  pred.var = c(top_interactions$Faktor_1[i], top_interactions$Faktor_2[i]),
                  n.trees= n_trees,
                  recursive = FALSE,
                  chull = TRUE,
                  grid.resolution = max_grid))
  
  }
  
  # Load or save data
  save(interaction_effects, file = paste("Data/Interaction_effects_",suffix,"_raw.RData", sep = ""))
}else{
  load(paste("Data/Interaction_effects_",suffix,"_raw.RData", sep = ""))
} 

# ======================================================================
##  Scoring with extracted factors ##
# ======================================================================

if (scoring_boosting_factors == TRUE){  
  print("Start scoring...")
  
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
    
    model_name <- "Ref_gbm"
    # Factor range
    range <- min(df$all[fact]):max(df$all[fact])
    
    univariate_pdp_data[[fact]]$factor_val <- round(univariate_pdp_data[[fact]]$factor_val)  
    
    
    factor_update[[fact]] <- as.numeric( approx(univariate_pdp_data[[fact]]$factor_val, 
                                                univariate_pdp_data[[fact]][[model_name]], 
                                                xout=range)[["y"]],
                                                rule=2)
    
 
    factor_update[[fact]][is.na(factor_update[[fact]])] <- mean(boosting_df$train$freq)
    
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
    if (is.numeric(df$all[[fact_1 ]])){
      range1 <- sort( unique(df$all[fact_1])[[fact_1]] )
    }else{
      range1 <- unique(df$all[fact_1])[[fact_1]]
    }
    
    
    if (is.numeric(df$all[[fact_2]])){
      range2 <- sort( unique(df$all[fact_2])[[fact_2]] )
      }else{
        range2 <- unique(df$all[[fact_2]])
          }
      
    range_grid <- data.frame(expand.grid(range1,range2), pred = NA )
    
    # If any of the two factors are categorical, matching exact, otherwise nearest neighbor
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
      
      
      
      train_factors_new[i,inter_fact_name] <-  (range_grid[intersect(which(range_grid$Var1 == boosting_df$train[[fact_1]][i]), which(range_grid$Var2 == boosting_df$train[[fact_2]][i])) ,])$pred
      
      
      if (any(i == round( nrow(boosting_df$train) - (1:9)* nrow(boosting_df$train)/10 ) )){
        
        print(paste("Done with ",percent(i/nrow(boosting_df$train))," of training" ))
      }
    }
    
    
    for (i in 1:nrow(boosting_df$cal)){
      
      cal_factors_new[i,inter_fact_name] <-  (range_grid[intersect(which(range_grid$Var1 == boosting_df$cal[[fact_1]][i]), which(range_grid$Var2 == boosting_df$cal[[fact_2]][i])) ,])$pred
      
      if (any(i == round( nrow(boosting_df$cal) - (1:9)* nrow(boosting_df$cal)/10 ) )){
        print(paste("Done with ",percent(i/nrow(boosting_df$cal))," of calibration" ))
      }
    }
    
    for (i in 1:nrow(boosting_df$test)){
      
      test_factors_new[i,inter_fact_name] <-  (range_grid[intersect(which(range_grid$Var1 == boosting_df$test[[fact_1]][i]), which(range_grid$Var2 == boosting_df$test[[fact_2]][i])) ,])$pred
      if (any(i == round( nrow(boosting_df$test) - (1:9)* nrow(boosting_df$test)/10 ) )){
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

  
  # Init pred is hereof legacy reasons...
  train_factors_new["init_pred"] <- mean(boosting_df$train$freq)
  cal_factors_new["init_pred"] <-  mean(boosting_df$train$freq)
  test_factors_new["init_pred"] <- mean(boosting_df$train$freq)

   
  boosting_df$train_factors <- train_factors_new
  boosting_df$cal_factors <- cal_factors_new
  boosting_df$test_factors <- test_factors_new
  
  
  # Save or load
  if (save == TRUE){save(boosting_df, file=paste("Data/Boost_data_",suffix,"_raw.RData", sep = ""))
    
    save(univariate_pdp_data_complete, file= paste("Data/PDP_uni_complete",suffix,".RData", sep = ""))
    }
  
  }else{
  load(paste("Data/Boost_data_",suffix,"_raw.RData", sep = ""))  
  load(paste("Data/PDP_uni_",suffix,".RData", sep = ""))
  load(paste("Data/PDP_uni_complete",suffix,".RData", sep = ""))
}

# ======================================================================
##  Auto-calibration of PDP-factors ##
# ======================================================================
 
tree_control = rpart.control(minbucket=10, cp=0.00001)

all_facts <- names(boosting_df$train_factors %>% dplyr::select(-c("freq","dur")))
all_trees <- list()

boosting_df$train_factors_final <- boosting_df$train_factors
boosting_df$cal_factors_final <- boosting_df$cal_factors
boosting_df$test_factors_final <-  boosting_df$test_factors

## CALIBRATION TO Y

for (fact in all_facts){

  model_trees <- formula(eval(paste("freq ~ ", fact, sep="")))
   
  tree_deep <- rpart(model_trees, 
                      data = boosting_df$train_factors %>% mutate(freq = freq/dur), 
                      method = "anova",
                      weights = dur,
                      control = tree_control )
  
  
  # Pruning
  prune_cp_glm <- tree_deep$cptable[which.min(tree_deep$cptable[, "xerror"]), "CP"]
  tree_temp <- prune.rpart(tree_deep, cp = prune_cp_glm)
 
  ## Updating final factors
  boosting_df$train_factors_final[fact] <- predict(tree_temp, newdata = boosting_df$train_factors) 
  boosting_df$cal_factors_final[fact] <- predict(tree_temp, newdata = boosting_df$cal_factors) 
  boosting_df$test_factors_final[fact] <- predict(tree_temp, newdata = boosting_df$test_factors) 
  
  all_trees[[fact]] <- tree_temp
  
    # Updating PDP-values
  if (fact %in% facts) { 
  temp_data <-  data.frame(val = univariate_pdp_data_complete[[fact]]$Ref_gbm_interpol)
  names(temp_data) <- fact 
  univariate_pdp_data_complete[[fact]]$Final_model <- predict(tree_temp, newdata = temp_data)*mean(boosting_df$train_factors$freq)
  }
  
} 

# Adding back categoricals
boosting_df$train_factors_final <- data.frame(boosting_df$train_factors_final, boosting_df$train[, cat_facts])
boosting_df$cal_factors_final <- data.frame(boosting_df$cal_factors_final, boosting_df$cal[, cat_facts])
boosting_df$test_factors_final <- data.frame(boosting_df$test_factors_final, boosting_df$test[, cat_facts])

# ======================================================================
##  FINAL GLM-modeling - Training GLM on extracted factor effects   ##
# ======================================================================

final_factors <- apply(boosting_df$train_factors_final,2, FUN= function(x) length(unique(x)))   
final_factors <- names(final_factors[final_factors>1])  
final_factors <- final_factors[!final_factors %in% c("dur", "freq")] 
models$final$Final_factors$all <- final_factors 
models$final$Final_factors$num_facts <- num_facts
models$final$Final_factors$cat_facts <- cat_facts


# Models
models$final$functional_form <- formula(eval(paste("freq ~ factor(", paste(final_factors, collapse = ") + factor(" ), ") + offset(log(dur))" , sep="")))
models$final$functional_form_lasso <- formula(eval(paste("freq ~ factor(", paste(final_factors, collapse = ") + factor(" ), ")" , sep="")))

# Vanilla GLM -------------------------------------------------------------


models$final$vanilla  <- glm(models$final$functional_form, 
                                data = boosting_df$train_factors_final, 
                                family = quasipoisson(link = "log"))
 
# Lasso GLM -------------------------------------------------------------

glmnet_data <- model.matrix(models$final$functional_form_lasso , boosting_df$train_factors_final )
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
 

# Final factors -----------------------------------------------------------
coef(models$final$lasso) 
summary(models$final$vanilla) 

# Predictions  -------------------------------------------------
pred$train$boosted_glm$vanilla <- sapply(
  as.numeric(predict.glm(
    models$final$vanilla,
    newdata = boosting_df$train_factors_final,
    type = "response",
    offset = log(boosting_df$train_factors_final$dur)
  )),
  function(x) min(x, 2)
)
pred$cal$boosted_glm$vanilla <- sapply(
  as.numeric(predict.glm(
    models$final$vanilla,
    newdata = boosting_df$cal_factors_final,
    type = "response",
    offset = log(boosting_df$cal_factors_final$dur)
  )),
  function(x) min(x, 2)
)
pred$test$boosted_glm$vanilla <- sapply(
  as.numeric(predict.glm(
    models$final$vanilla,
    newdata = boosting_df$test_factors_final,
    type = "response",
    offset = log(boosting_df$test_factors_final$dur)
  )),
  function(x) min(x, 2)
)
 
pred$train$boosted_glm$lasso <- sapply(as.numeric(predict(models$final$lasso,  newx = model.matrix(models$final$functional_form_lasso , boosting_df$train_factors_final ), type = "response",  s = best_lambda, newoffset = log(boosting_df$train_factors_final$dur) )), function(x) min(x,2))  
pred$cal$boosted_glm$lasso <- sapply(as.numeric(predict(models$final$lasso, newx = model.matrix(models$final$functional_form_lasso , boosting_df$cal_factors_final ), type = "response", s = best_lambda, newoffset = log(boosting_df$cal_factors$dur))), function(x) min(x,2))  
pred$test$boosted_glm$lasso <- sapply(as.numeric(predict(models$final$lasso, newx = model.matrix(models$final$functional_form_lasso , boosting_df$test_factors_final ), type = "response", s = best_lambda, newoffset = log(boosting_df$test_factors$dur))) , function(x) min(x,2))  

# Balance check  -------------------------------------------------
mean(boosting_df$train$freq)
mean(pred$train$boosted_glm$vanilla)
mean(pred$train$boosted_glm$lasso)


# ======================================================================
##  Defining final models: Updating PDP according to lasso selections ##
# ======================================================================

# Extracting lasso values
row_names <- rownames(coef(models$final$lasso))
factors <- sub(".*\\((.*)\\).*", "\\1", row_names)
factor_values <- sub(".*\\)(.*)$", "\\1", row_names)
coef_values <- coef(models$final$lasso)
coef_values <- as.numeric(as.matrix(coef_values))
coef_values[is.na(coef_values)] <- 0

# Adjusting model values
for (fact in num_facts){
   temp_pdp_val <- round(univariate_pdp_data_complete[[fact]]$Final_model/mean(boosting_df$train_factors$freq) ,6)
   temp_coef_level <- round(as.numeric(factor_values[which(factors==fact) ]),6)
   temp_coef_val <- round(as.numeric(coef_values[which(factors==fact) ]),6)
   
   temp_new_pdp_val <- rep(mean(boosting_df$train$freq), length(temp_pdp_val))
   for (i in 1:length(temp_pdp_val)){ 
      if( any(temp_pdp_val[i] == temp_coef_level)){
       idx <- which(temp_pdp_val[i] == temp_coef_level)
        
        temp_new_pdp_val[i] <- temp_coef_val[idx]
      }
     }
       

   univariate_pdp_data_complete[[fact]]$Final_model_lasso <- exp(temp_new_pdp_val)*mean(boosting_df$train_factors$freq)
   }

# ======================================================================
##  Visualisation and performance metrics ##
# ======================================================================
 
if (save_last == TRUE){
  save(models, file = paste("Data/Models_",suffix,".RData", sep = ""))
  save(boosting_df, file = paste("Data/Boost_data_",suffix,".RData", sep = ""))
  save(pred, file = paste("Data/Predictions_",suffix,".RData", sep = ""))
  save(univariate_pdp_data_complete, file = paste("Data/PDP_uni_",suffix,".RData", sep = ""))
  save(all_trees, file = paste("Data/All_trees_",suffix,".RData", sep = ""))
  
}


 