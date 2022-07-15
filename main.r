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
suffix <- "CAS"
date <- "20220701"
REAL_data <- FALSE

new_models <- TRUE
new_univar_effects<- TRUE
save <- TRUE

plot_folder <- paste("Plottar/",suffix,"_",date, sep="")
dir.create(plot_folder)
n_sim <- 600000


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
train_frac <- 0.7


### IMPORT CAS ###
if (REAL == FALSE){
  library(CASdatasets)
  data("freMTPL2freq")
  
  data_input <- freMTPL2freq 
  
  data_idx <- sample(c(1:dim(data_input)[1] ), size = n_sim, replace = FALSE)
  df$all <- as.data.frame(data_input[data_idx,])
  
  
  # Corrections according to Schelldorfer and Wüthrich (2019)
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
   df$all$n <- as.numeric( df$all$n)
  
  # Data splits
  n_obs <-  df$all$n[1:n_sim]
  
  
  df$train <- df$all[1:(n_sim*train_frac), ]
  df$test <- df$all[(n_sim*train_frac + 1):n_sim, ]
  
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
  
  df$train <- df$all[1:(n_sim*train_frac), ]
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
if (new_models == TRUE){ 
  # INIT and modeling parameters
  
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
  
  pred$test$init <- predict.glm(object = models$glm_init, 
                                newdata = df$test , 
                                type = "response") 
  
  # Init data for boosting algorithm 
  
  boosting_df$train <- data.frame(df$train, init_pred = pred$train$init )
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
  
  pred$train$ref <- pred$test$train*balance_factor
  
  pred$test$ref <- predict.gbm(object = models$raw_gbm,
                              n.trees=models$raw_gbm.ntrees,
                              newdata = boosting_df$test , 
                              type = "response")  *boosting_df$test$dur * balance_factor
  
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
  
  
  boosting_df$train <- data.frame(df$train, init_pred = pred$train$init , boost_pred = pred$train$boost )
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
if (new_univar_effects == TRUE){
  # Marginal (univariate) effects -------------------------------------------
  
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
  
  univariate_pdp_data <- list()
  
  for (fact in num_facts){
    if (is.factor(df$train[,fact]) == FALSE){ 
      
      
      xlim <- quantile(df$train[,fact], c(.01,.99)) 
      
      p1 <- partial(models$gbm_boost, pred.fun=pgbm, pred.var = c(fact), n.trees= models$gbm_boost.ntrees,recursive = FALSE, 
                    grid.resolution = 50) 
      p2 <- partial(models$glm_init, pred.fun=pglm, pred.var = c(fact), 
                    grid.resolution = 50)
      p3 <- partial(models$raw_gbm, pred.fun=pgbm_raw, pred.var = c(fact), n.trees= models$raw_gbm.ntrees,recursive = FALSE, 
                    grid.resolution = 50)
      p4 <- p1$yhat*p2$yhat
      
      
      indx_plt <- which(p1[,fact] > xlim[1] & p1[,fact] < xlim[2])
      scale_factor_boosting <- mean(df$train$freq) 
      
      univariate_pdp_data[[fact]] <- data.frame(factor_val = p1[,1], 
                                                Tariff = p2$yhat[], 
                                                Boost = p1$yhat[], 
                                                Ref_gbm = p3$yhat[], 
                                                After_boost = p4[])
        
      if (plot_during_algo == TRUE){
        univariate_pdp_data[[fact]] %>% 
          ggplot(aes(x=factor_val)) + 
          geom_line(aes(y=Tariff, color="black")) +
          geom_line(aes(y=Boost*mean(df$train$freq), color="red"))+
          geom_line(aes(y=Ref_gbm, color="grey"), lty=2)+
          geom_line(aes(y=After_boost, color="blue"))+
          geom_abline(intercept = mean(df$train$freq),slope=0, color="grey", alpha=0.5)+
          xlim(xlim[1],xlim[2])+
          labs(x= fact,
               y="PDP",
               subtitle="Bosting factor (red) correspond to the multiplicative difference (according to PDP) 
               between original tariff (black) and boosted tariff (blue)")+
          scale_colour_manual(name = 'Model', 
                              values =c('black'='black','red'='red','grey'='grey','blue'='blue'), 
                              labels = c('Tariff (GLM)','Boosted tariff', 'Reference (pure GBM)', 'Boosting factor'))+
          scale_y_continuous(sec.axis = sec_axis( trans=~./mean(df$train$freq), name="Boosting factor"))+
          theme_classic()  +
          ggsave(paste(plot_folder,"/PDP_boost_",fact , ".png",sep=""))
      }
    }
  }
  
  
  if (save == TRUE){
    save(univariate_pdp_data, file = paste("Data/Univar_effects_",suffix,".RData", sep = ""))
  }
}else{
  
  
  load(paste("Data/Univar_effects_",suffix,".RData", sep = ""))
  
}

# Testing boosting factors  -----------------------------------------------

### OBSOBS: Hanterar endast - numeriska - linjära ursprungliga faktorer (måste hantera godtyckligt definierad faktor sedermera!)

# INIT

loss_diff <- data.frame(facts = num_facts, diff = rep(NA, length(num_facts)) )

num_facts_loop <- num_facts
min_improvement <- 100
final_factors <- list()
final_update <- list()
fact_selected <- list()

# Test data predictions
test_pred_start <- predict.glm(object = models$glm_init, 
                               newdata = df$test , 
                               type = "response") 
train_pred_start <- predict.glm(object = models$glm_init, 
                               newdata = df$train , 
                               type = "response") 

names(new_factor)

for (k in 1:length(num_facts)){
  factor_update <- list()
  old_factor_effect <- list()
  train_pred_new <- list()
  test_pred_new <- list()
  balance_factor <- list()
  new_factor <- list()
  
  if (k > 1){  
    num_facts_loop <- num_facts_loop[num_facts_loop != fact_selected ] 
  }
  
  
  loss_diff <- data.frame(facts = num_facts_loop, diff = rep(NA, length(num_facts_loop)) )
  
  for (fact in num_facts_loop){
    
    # Factor range
    range <- min(df$all[fact]):max(df$all[fact])
    
    univariate_pdp_data[[fact]]$factor_val <- round(univariate_pdp_data[[fact]]$factor_val) # OBS: Hanterar endast factorer med nivåer nära heltal!
    
    
    old_factor_effect[[fact]] <- exp(models$glm_init$coefficients[fact]*range) 
    factor_update[[fact]] <-  as.numeric( approx(univariate_pdp_data[[fact]]$factor_val , univariate_pdp_data[[fact]]$Boost , xout=range)[["y"]])
    factor_update[[fact]][is.na(factor_update[[fact]])] <- 1
    
    new_factor[[fact]] <- old_factor_effect[[fact]] * factor_update[[fact]]
     
    #  Updating predictions with new effect
    train_pred_new[[fact]] <- (train_pred_start * factor_update[[fact]][df$train[[fact]]  - min(range) ,1] )
    balance_factor[[fact]] <- mean(train_pred_new[[fact]])/mean(df$train$freq)
    train_pred_new[[fact]] <- train_pred_new[[fact]] * balance_factor[[fact]]
    test_pred_new[[fact]] <- (test_pred_start * factor_update[[fact]][df$test[[fact]]  - min(range) ,1] )*balance_factor[[fact]]
    
    
    # Evaluate 
    loss_start <- deviance(pred_mu = test_pred_start, y =df$test$freq, res=FALSE )
    loss_new <- deviance(pred_mu = test_pred_new[[fact]], y =df$test$freq, res=FALSE )
    
    loss_diff[which(loss_diff$facts==fact), "diff"] <- loss_new-loss_start
  
  }
  
  
  # Include according to best prediction 
  
  if ( min(loss_diff$diff) < min_improvement){
    fact_selected[[k]] <- loss_diff$facts[which(loss_diff$diff == min(loss_diff$diff))]
    
    
    # Updating factor
    final_factors[[fact_selected[[k]]]] <- new_factor[[fact_selected[[k]]]]
    final_update[[fact]] <- factor_update[[fact]]
    
    
    # Updating predictions
    train_pred_start <- train_pred_new[[fact_selected[[k]]]]
    test_pred_start <- test_pred_new[[fact_selected[[k]]]]
    
    
    
  }else{
    print("Predictive performance exhausted. Breaking...")
    break
  }
}



## TO DO:
# Make algo data-independent (i.e. scalable)
# Cross validation - How?





# Finalizing models and predictions ---------------------------------------

# Predictions after final univariate boosting
pred$train$univar_boosted <- train_pred_start 
pred$test$univar_boosted <- test_pred_start 

# Updated model factors
for (fact in names(final_factors)){
univariate_pdp_data[[fact]] %>% 
  ggplot(aes(x=factor_val))+
  geom_line(aes(y=Tariff, color="black")) + 
  geom_line(aes(y=Boost*mean(df$train$freq), color="red"))+
  geom_line(aes(y=Ref_gbm, color="grey"), lty=2) +
  geom_line(aes(y=After_boost, color="blue")) +
  geom_abline(intercept = mean(df$train$freq),slope=0, color="grey", alpha=0.5)+
  xlim(xlim[1],xlim[2])+
  labs(x= fact,
       y="PDP",
       subtitle="Bosting factor (red) correspond to the multiplicative difference (according to PDP) 
             between original tariff (black) and boosted tariff (blue)")+
  scale_colour_manual(name = 'Model', 
                      values =c('black'='black','red'='red','grey'='grey','blue'='blue'), 
                      labels = c('Tariff (GLM)','Boosted tariff', 'Reference (pure GBM)', 'Boosting factor'))+
  scale_y_continuous(sec.axis = sec_axis( trans=~./mean(df$train$freq), name="Boosting factor")) +
  theme_classic() 
  
  #ggsave(paste(plot_folder,"/PDP_boost_",fact , ".png",sep=""))

}

# Predictive performance --------------------------------------------------

MSEP <- data.frame( Model=c("Reference", "Boosted", "Init-tariff", "Univar_boosted_GLM"),
                    
                    
                    MSEP=c( mean((pred$test$ref - boosting_df$test$freq) ^2), 
                            mean((pred$test$boost - boosting_df$test$freq) ^2),
                            mean((pred$test$init - boosting_df$test$freq) ^2),
                            mean((pred$test$univar_boosted - boosting_df$test$freq) ^2)))
