

# ======================================================================
## Summarising evaluations ##
# ======================================================================


date <- "20220403"
suffix <- data

plot_folder <- paste("Plottar/_FINAL_",date, sep="")
dir.create(plot_folder)


eval_all <- data.frame()
final_factors <- list()
 
k <- 1

# Loading data ------------------------------------------------------------
for (data in c("norauto","beMTPL", "auspriv","freMTPL", "REAL")){ # 
  
  suffix <- data
  load(file = paste("Data/evals_",suffix,".RData", sep = ""))  
  load(file = paste("Data/Predictions_",suffix,".RData", sep = ""))  
  load(file = paste("Data/Boost_data_",suffix,".RData", sep = ""))  
  load(file = paste("Data/Models_",suffix,".RData", sep = ""))  
  
  eval_all[k,1] <- data
  eval_all[k,2] <- percent(evals$MSEP_test$MSEP[5]-1,0.01)
  eval_all[k,3] <- percent(evals$MSEP_test$Deviance[5]-1,0.01)
  eval_all[k,4] <- percent(evals$MSEP_test$Fidelity[5])
  eval_all[k,5] <- percent(evals$MSEP_test$MSEP[1]-1,0.01)
  eval_all[k,6] <- percent(evals$MSEP_test$MSEP[3]-1,0.01)
  eval_all[k,7] <- length(unique(pred$train$boosted_glm$lasso/boosting_df$train_factors_final$dur))
  
  

  #coef(models$final$lasso)
  
  k <- k+1
  
  # Get final factors
  row_names <- rownames(coef(models$final$lasso))
  factors <- sub(".*\\((.*)\\).*", "\\1", row_names) 
  final_factors[[data]] <- data.frame(factors=factors) %>% group_by(factors) %>% count()  %>% as.matrix() 
}


colnames(eval_all) <- c("Data","MSEP-Final","Deviance-Final","Fidelity-Final", "MSEP-Intercept", "MSEP-Linear")
eval_all

coef(models$final$lasso)


p <- ggplot(tibble(x=0,y=0, tb=list(eval_all) )) +
  theme_void() + 
  geom_table(aes(x, y, label = tb),parse = TRUE)  

ggsave(filename = paste(plot_folder,"/Summary.png",sep="") , plot = p, dpi = 300,width = 10, height = 8)


# PDP ---------------------------------------------------------------------



