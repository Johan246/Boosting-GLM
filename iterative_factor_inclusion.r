

# INIT


min_improvement <- 1


loss_diff <- data.frame(facts = num_facts, diff = rep(NA, length(num_facts)) )

num_facts_loop <- num_facts
final_factors <- list()
final_update <- list()
fact_selected <- list()
loss_improvements <- list()

# Test data predictions
test_pred_start <- predict.glm(object = models$glm_init, 
                               newdata = boosting_df$test , 
                               type = "response") 

cal_pred_start <- predict.glm(object = models$glm_init, 
                              newdata = boosting_df$cal , 
                              type = "response") 

train_pred_start <- predict.glm(object = models$glm_init, 
                                newdata = boosting_df$train , 
                                type = "response") 




for (k in 1:length(num_facts)){
  factor_update <- list()
  old_factor_effect <- list()
  train_pred_new <- list()
  cal_pred_new <- list()
  test_pred_new <- list()
  balance_factor <- list()
  new_factor <- list()
  
  if (k > 1){  
    num_facts_loop <- num_facts_loop[!(num_facts_loop %in% fact_selected )] 
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
    train_pred_new[[fact]] <- (train_pred_start * factor_update[[fact]][boosting_df$train[[fact]]  - min(range)+1 ] )
    balance_factor[[fact]] <- mean(boosting_df$train$freq)/mean(train_pred_new[[fact]])
    train_pred_new[[fact]] <- train_pred_new[[fact]] * balance_factor[[fact]]
    
    cal_pred_new[[fact]] <- (cal_pred_start * factor_update[[fact]][boosting_df$cal[[fact]]  - min(range)+1 ] )*balance_factor[[fact]]
    test_pred_new[[fact]] <- (test_pred_start * factor_update[[fact]][boosting_df$test[[fact]]  - min(range)+1 ] )*balance_factor[[fact]]
    
    
    # Evaluate 
    loss_start <- deviance(pred_mu = cal_pred_start, y =boosting_df$cal$freq, res=FALSE )
    loss_new <- deviance(pred_mu = cal_pred_new[[fact]], y =boosting_df$cal$freq, res=FALSE )
    
    loss_diff[which(loss_diff$facts==fact), "diff"] <- loss_new - loss_start
    
  }
  
  
  # Include according to best prediction 
  
  if ( 1 + min(loss_diff$diff)/loss_start < min_improvement){
    loss_improvements[[k]] <- min(loss_diff$diff) 
    fact_selected[[k]] <- loss_diff$facts[which(loss_diff$diff == min(loss_diff$diff))]
    
    
    # Updating factor
    final_factors[[fact_selected[[k]]]] <- new_factor[[fact_selected[[k]]]]
    final_update[[fact]] <- factor_update[[fact]]
    
    
    # Updating predictions
    train_pred_start <- train_pred_new[[fact_selected[[k]]]]
    cal_pred_start <- cal_pred_new[[fact_selected[[k]]]]
    test_pred_start <- test_pred_new[[fact_selected[[k]]]]
    
    
    
  }else{
    
    # Predictions after final univariate boosting
    
    pred$train$univar_boosted <- train_pred_start 
    pred$cal$univar_boosted <- cal_pred_start 
    pred$test$univar_boosted <- test_pred_start 
    
    print("Predictive performance exhausted. Breaking...")
    break
  }
}



