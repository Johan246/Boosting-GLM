

# ======================================================================
## Summarising evaluations ##
# ======================================================================


setwd("//e75a0679/sakta/UTV/SU/Program/Analys/Boosting GLM/Boosting-GLM")

date <- "20220403"
suffix <- data

plot_folder <- paste("Plottar/_FINAL_",date, sep="")
dir.create(plot_folder)


eval_all <- data.frame()
final_factors <- list()
pdp_values_final <- list() 


k <- 1


# Loading data ------------------------------------------------------------
for (data in c( "REAL")){ # "norauto","beMTPL", "auspriv","freMTPL", "REAL"
 print(paste("Summary stats and plots for", data))
  
  suffix <- data
  load(file = paste("Data/evals_",suffix,".RData", sep = ""))  
  load(file = paste("Data/Predictions_",suffix,".RData", sep = ""))  
  load(file = paste("Data/Boost_data_",suffix,".RData", sep = ""))  
  load(file = paste("Data/Models_",suffix,".RData", sep = ""))  
  load(file = paste("Data/All_trees_",suffix,".RData", sep = "")) 
  load(file = paste("Data/Interaction_effects_",suffix,"_raw.RData", sep = ""))  
  load(paste("Data/Univar_effects_",suffix,".RData", sep = ""))


  eval_all[k,1] <- data
  eval_all[k,2] <- percent(evals$MSEP_test$MSEP[5]-1,0.01)
  eval_all[k,3] <- percent(evals$MSEP_test$Deviance[5]-1,0.01)
  eval_all[k,4] <- percent(evals$MSEP_test$Fidelity[5])
  eval_all[k,5] <- percent(evals$MSEP_test$MSEP[1]-1,0.01)
  eval_all[k,6] <- percent(evals$MSEP_test$MSEP[3]-1,0.01)
  eval_all[k,7] <- length(unique(pred$train$boosted_glm$lasso/boosting_df$train_factors_final$dur))




  coef(models$final$lasso)

  k <- k+1

  # Get final factors
  row_names <- rownames(coef(models$final$lasso))
  factors <- sub(".*\\((.*)\\).*", "\\1", row_names)
  final_factors[[data]] <- data.frame(factors=factors) %>% group_by(factors) %>% count()  %>% as.matrix()

  
  
  colnames(eval_all) <- c("Data","MSEP-Final","Deviance-Final","Fidelity-Final", "MSEP-Intercept", "MSEP-Linear")
  eval_all
  
  coef(models$final$lasso)
  
  
  p <- ggplot(tibble(x=0,y=0, tb=list(eval_all) )) +
    theme_void() + 
    geom_table(aes(x, y, label = tb),parse = TRUE)  
  
  ggsave(filename = paste(plot_folder,"/Summary.png",sep="") , plot = p, dpi = 300,width = 10, height = 8)
  
  
  # PDP ---------------------------------------------------------------------
  
  load(file = paste("Data/PDP_uni_complete",suffix,".RData", sep = ""))
  load(file = paste("Data/Interaction_effects_complete",suffix,"_raw.RData", sep = "")) 
   
  
  #for ( fact in models$final$Final_factors$num_facts){
   
  init_data_temp <- boosting_df$train
  pdp_data_temp <- boosting_df$train_factors
  final_data_temp <- boosting_df$train_factors_final
  pdp_values_temp <- univariate_pdp_data
  num_facts <- models$final$Final_factors$num_facts
  range_data <- univariate_pdp_data_complete
   
  
  for (fact in num_facts){
    print(paste("Final pdp for", fact))
    
    range <- sort(pdp_values_temp[[fact]]$factor_val)
    range_alt <- unique(range_data[[fact]]$factor_val)
    
    for (i in 1:length(range)){
      
    
      init_data_temp[1:length(range_alt),fact] <- range_alt ## Full range required, to enable model.matrix()
      init_data_temp[(length(range_alt)+1):nrow(init_data_temp),fact] <- range[i]
      
      
      # Get gbm-pdp-transfom
      pdp_data_temp[1:length(range_alt),fact] <- range_data[[fact]]$Ref_gbm_interpol
      pdp_data_temp[(length(range_alt)+1):nrow(init_data_temp),fact] <- pdp_values_temp[[fact]]$Ref_gbm[i]## Full range required, to enable model.matrix()
      
      final_data_temp[fact] <- predict(all_trees[[fact]],newdata = pdp_data_temp)
      
      pdp_values_temp[[fact]][i,"final_model_vanilla"] <- mean(sapply(as.numeric(predict.glm(models$final$vanilla,
                                                                                             newdat=final_data_temp, 
                                                                                             type="response", 
                                                                                             newoffset=final_data_temp$dur)) , function(x) min(x,2)))
    
      pdp_values_temp[[fact]][i,"final_model_lasso"] <- mean(sapply(as.numeric(predict(models$final$lasso,  
                                                                                       newx = model.matrix(models$final$functional_form_lasso , 
                                                                                                           final_data_temp),
                                                                                       type = "response", 
                                                                                       newoffset = log(final_data_temp$dur) )), 
                                                                    function(x) min(x,2)) ) 
    
      
    }
    
    balance <- mean(boosting_df$train$freq)
    pdp_values_temp[[fact]]$Ref_gbm <- pdp_values_temp[[fact]]$Ref_gbm /mean(pdp_values_temp[[fact]]$Ref_gbm)*balance
    pdp_values_temp[[fact]]$final_model_vanilla  <-pdp_values_temp[[fact]]$final_model_vanilla  /mean(pdp_values_temp[[fact]]$final_model_vanilla )*balance
    pdp_values_temp[[fact]]$final_model_lasso  <- pdp_values_temp[[fact]]$final_model_lasso /mean(pdp_values_temp[[fact]]$final_model_lasso)*balance
    
    p <-  pdp_values_temp[[fact]] %>% 
      ggplot(aes(x=factor_val))+
      geom_line(aes(y=final_model_lasso, color="red"))+
      geom_line(aes(y=Ref_gbm , color="black")) +
      geom_line(aes(y=final_model_vanilla, color="blue")) +
      geom_abline(intercept = mean(boosting_df$train$freq),slope=0, color="grey", lty=2, alpha=0.5)+
      #xlim(xlim[1],xlim[2])+
      labs(x= fact,
           y="PDP" )+
      scale_colour_manual(name = '', 
                          values =c('black'='black','red'='red','grey'='grey','blue'='blue'), 
                          labels = c('Initial GBM','After trees','Final model (lasso)' )
      )+  
      theme_classic() +
      theme(legend.position ="bottom")
    
    ggsave(filename = paste(plot_folder,"/PDP_final_",data,"_",fact , ".png",sep=""), plot = p, dpi = 300,width = 10, height = 8)
    
  } 
  
  
  pdp_values_final[[data]] <- pdp_values_temp  
   
}


save(pdp_values_final, file = paste("Data/Final_pdp_all.RData", sep = ""))



#TEMP


