#suffix <- "freMTPL"
# Helpfunctions -----------------------------------------------------------
source("helpfunctions.r")


# Data import -------------------------------------------------------------

load(paste("Data/Models_",suffix,".RData", sep = "")) 
load(paste("Data/Predictions_",suffix,".RData", sep = ""))
load(paste("Data/Univar_effects_",suffix,".RData", sep = ""))
load(paste("Data/Interaction_effects_",suffix,"_raw.RData", sep = ""))
load(paste("Data/Boost_data_",suffix,"_raw.RData", sep = ""))
load(paste("Data/PDP_uni_",suffix,".RData", sep = ""))

num_facts <-  colnames(boosting_df$train[,facts])[sapply(boosting_df$train %>% dplyr::select(facts),class) %in% c("numeric","integer")] 

# Partial Dependence Plots - final models (incl bench) ---------------------------------

# for (fact in num_facts){
#   
#   p <-  univariate_pdp_data_complete[[fact]] %>% 
#     ggplot(aes(x=factor_val))+
#     geom_step(aes(y=Final_model, color="red"))+
#     geom_step(aes(y=Ref_gbm_interpol , color="grey"), lty=2) +
#     geom_step(aes(y=Final_model_lasso, color="blue"))+
#     geom_abline(intercept = mean(boosting_df$train$freq),slope=0, color="grey", alpha=0.5)+
#     #xlim(xlim[1],xlim[2])+
#     labs(x= fact,
#          y="PDP" )+
#     scale_colour_manual(name = '', 
#                         values =c('black'='black','red'='red','grey'='grey','blue'='blue'), 
#                         labels = c('Linear','Final GLM','GBM (PDP)','After trees')
#     )+
#     scale_y_continuous(sec.axis = sec_axis( trans= ~./mean(boosting_df$train$freq), name="Boosting factor")) + 
#     theme_classic() +
#     theme(legend.position ="bottom")
#   
#   ggsave(filename = paste(plot_folder,"/PDP_boost_",fact , ".png",sep=""), plot = p, dpi = 300,width = 10, height = 8)
#   
# }   
# 
# 
# # Predictive performance --------------------------------------------------
# model_names <- c("Intercept","GBM", "Linear", "Final GLM (no lasso)", "Final GLM")
# MSEP_cal <- data.frame( Model= model_names,
#                         
#                         
#                         MSEP=round(c( mean((  mean(boosting_df$train$freq)/mean(boosting_df$train$dur)*boosting_df$cal$dur - boosting_df$cal$freq) ^2), 
#                                       mean((pred$cal$ref - boosting_df$cal$freq) ^2), 
#                                       mean((pred$cal$init - boosting_df$cal$freq) ^2),
#                                       mean((pred$cal$boosted_glm$vanilla - boosting_df$cal$freq)^2),
#                                       mean((pred$cal$boosted_glm$lasso - boosting_df$cal$freq)^2)),5),
#                         
#                         Deviance = round(c(deviance(rep(mean(boosting_df$train$freq), length(boosting_df$cal$freq)) , pred_phi=NULL, boosting_df$cal$freq, res = FALSE),
#                                            deviance(pred$cal$ref, pred_phi=NULL, boosting_df$cal$freq, res = FALSE),
#                                            deviance(pred$cal$init, pred_phi=NULL, boosting_df$cal$freq, res = FALSE),
#                                            deviance(pred$cal$boosted_glm$vanilla , pred_phi=NULL, boosting_df$cal$freq, res = FALSE),
#                                            deviance(pred$cal$boosted_glm$lasso , pred_phi=NULL, boosting_df$cal$freq, res = FALSE)),5),
#                         
#                         
#                         
#                         Fidelity= round(c(0,
#                                           cor(pred$cal$ref, pred$cal$ref), 
#                                           cor(pred$cal$init, pred$cal$ref),
#                                           cor(pred$cal$boosted_glm$vanilla , pred$cal$ref),
#                                           cor(pred$cal$boosted_glm$lasso ,pred$cal$ref)),2)
# )
# 
# MSEP_test <- data.frame( Model=model_names,
#                          
#                          
#                          MSEP=round(c(  mean((  mean(boosting_df$train$freq)/mean(boosting_df$train$dur)*boosting_df$test$dur - boosting_df$test$freq) ^2),
#                                        mean((pred$test$ref - boosting_df$test$freq) ^2),  
#                                        mean((pred$test$init - boosting_df$test$freq) ^2),
#                                        mean((pred$test$boosted_glm$vanilla - boosting_df$test$freq) ^2),
#                                        mean((pred$test$boosted_glm$lasso - boosting_df$test$freq) ^2)),5),
#                          
#                          Deviance = round(c(deviance(rep(mean(boosting_df$train$freq),length(boosting_df$cal$freq)) , pred_phi=NULL, boosting_df$test$freq, res = FALSE),
#                                             deviance(pred$test$ref, pred_phi=NULL, boosting_df$test$freq, res = FALSE),
#                                             deviance(pred$test$init, pred_phi=NULL, boosting_df$test$freq, res = FALSE),
#                                             deviance(pred$test$boosted_glm$vanilla , pred_phi=NULL, boosting_df$test$freq, res = FALSE),
#                                             deviance(pred$test$boosted_glm$lasso , pred_phi=NULL, boosting_df$test$freq, res = FALSE)),5) ,
#                          
#                          
#                          Fidelity= round(c(0,
#                                            cor(pred$test$ref, pred$test$ref),
#                                            cor(pred$test$init, pred$test$ref),
#                                            cor(pred$test$boosted_glm$vanilla , pred$test$ref),
#                                            cor(pred$test$boosted_glm$lasso ,pred$test$ref)),2)
# )
# 
# 
# MSEP_test$MSEP <- round(MSEP_test$MSEP/MSEP_test$MSEP[2],4)
# MSEP_cal$MSEP <- round(MSEP_cal$MSEP/MSEP_cal$MSEP[2],4)
# MSEP_test$Deviance <- round(MSEP_test$Deviance/MSEP_test$Deviance[2],4)
# MSEP_cal$Deviance <- round(MSEP_cal$Deviance/MSEP_cal$Deviance[2],4)
# 
# evals <- list(data= data,MSEP_test=MSEP_test, MSEP_cal=MSEP_cal)
# if (save == TRUE){
#   save(evals, file = paste("Data/evals_",suffix,".RData", sep = ""))
# }
# 
# 
#   
# p <- ggplot(tibble(x=0,y=0, tb=list(MSEP_test ))) +
#   theme_void() + 
#   geom_table(aes(x, y, label = tb),parse = TRUE)  
# 
# ggsave(filename = paste(plot_folder,"/Model_evaluation_test.png",sep="") , plot = p, dpi = 300,width = 10, height = 8)
# 
# # All predictions
# 
# pred_data <- data.frame(model="1. Linear", pred = pred$test$init ,dur =  boosting_df$test$dur, obs= boosting_df$test$freq) %>%
#   bind_rows(  data.frame(model="2. Final GLM (no lasso) ", pred = pred$test$boosted_glm$vanilla ,dur =  boosting_df$test$dur, obs= boosting_df$test$freq)) %>%
#   bind_rows(  data.frame(model="4.GBM" , pred = pred$test$ref,dur =  boosting_df$test$dur, obs= boosting_df$test$freq)) %>% 
#   bind_rows(  data.frame(model="3.Final GLM",  pred= pred$test$boosted_glm$lasso , dur =  boosting_df$test$dur, obs= boosting_df$test$freq) )
# 
# # MU
#  
# p <- pred_data %>% 
#   group_by(model) %>%
#   arrange(pred, .by_group = TRUE) %>%
#   mutate(bin = ceiling( (cumsum(dur)/sum(dur)*100) )) %>%
#   group_by(model,bin) %>%
#   summarise(
#     pred  = mean(pred),
#     obs = mean(obs)
#   ) %>%
#   ggplot(aes(x=bin)) + 
#   geom_point(aes(y=obs)) +  
#   geom_line(aes(y=pred), linewidth=1, colour="red" ) +
#   theme_classic() +
#   facet_grid(model ~. ) 
# 
# ggsave(filename = paste(plot_folder,"/Predictions.png",sep="") , plot = p, dpi = 300,width = 10, height = 8)
# 
# 
# 
# # Scatter -----------------------------------------------------------------
# 
# scatter_data <- data.frame(Final_pred=  pred$test$boosted_glm$lasso,
#                            GBM = pred$test$ref)
# 
# p <- scatter_data %>% ggplot(aes(x=log(Final_pred),y=log(GBM))) + geom_point() +
#   theme_classic()+
#   
#   labs(
#     y="GBM prediction (log-scale)",
#     x="Final model prediction (log-scale)", color="",shape="" )
# 
# ggsave(filename = paste(plot_folder,"/Scatter_",suffix,".png",sep="") , plot = p, dpi = 300,width = 10, height = 8)
# 
# 
# 
# 
# 
# # Game --------------------------------------------------------------------
# 
# 
# 
# game_data <- data.frame(init= pred$test$init, 
#                         Final_model_no_lasso =  pred$test$boosted_glm$vanilla,
#                         Final_model =  pred$test$boosted_glm$lasso,
#                         GBM = pred$test$ref,
#                         dur =  boosting_df$test$dur,
#                         obs =   boosting_df$test$freq
# )
# 
#                       
# p <- game_data %>% mutate(premium_Final_model = cumsum(Final_model*(Final_model < GBM)),
#                      profit_Final_model = cumsum((Final_model - obs)*(Final_model < GBM)),
#                      premium_gbm = cumsum(GBM*(Final_model > GBM)),
#                      profit_gbm = cumsum((GBM - obs)*(Final_model > GBM)),
#                      id=row_number()/n())     %>%
#   
#   ggplot(aes(x=id))+
#   geom_line(aes(y=profit_Final_model, colour="blue") )+
#   geom_line(aes(y=profit_gbm, colour="red") )+
#   theme_classic()+
#   theme(legend.position = c(.3, .1))+
#   labs(
#     y="Profit (accumulated)",
#     x="% of portfolio (lowest to highest risk)", color="",shape="" ) +
#   scale_colour_manual(name = '', 
#                       values =c('red'='red','blue'='blue'), 
#                       labels = c( 'Final Model','GBM'))              
# 
# ggsave(filename = paste(plot_folder,"/Game_final_vs_gbm.png",sep="") , plot = p, dpi = 300,width = 10, height = 8)
# 
#  
# p <- game_data %>% mutate(premium_linear = cumsum(init*(init < GBM)),
#                      profit_linear = cumsum((init - obs)*(init < GBM)),
#                      premium_gbm = cumsum(GBM*(init > GBM)),
#                      profit_gbm = cumsum((GBM - obs)*(init > GBM)),
#                      id=row_number()/n())     %>%
#   
#   ggplot(aes(x=id))+
#   geom_line(aes(y=profit_linear, colour="blue") )+
#   geom_line(aes(y=profit_gbm, colour="red") )+
#   theme_classic()+
#   theme(legend.position = c(.3, .1))+
#   labs( 
#     y="Profit (accumulated)",
#     x="% of portfolio (lowest to highest risk)", color="",shape="" ) +
#   scale_colour_manual(name = '', 
#                       values =c('red'='red','blue'='blue'), 
#                       labels = c('Linear', 'GBM')) 
# 
# ggsave(filename = paste(plot_folder,"/Game_linear_vs_gbm.png",sep=""), plot = p, dpi = 300,width = 10, height = 8)
#  
# 
# p <- game_data %>% mutate(premium_Final_model = cumsum(Final_model*(Final_model < init)),
#                      profit_Final_model = cumsum((Final_model- obs)*(Final_model < init)),
#                      premium_linear = cumsum(init*(Final_model> init)),
#                      profit_linear = cumsum((init - obs)*(Final_model > init)),
#                      id=row_number()/n())     %>%
#   
#   ggplot(aes(x=id)) +
#   geom_line(aes(y=profit_Final_model, colour="red"))+
#   geom_line(aes(y=profit_linear, colour="blue"))+
#   theme_classic()+
#   theme(legend.position = c(.3, .1))+
#   labs(
#     y="Profit (accumulated)",
#     x="% of portfolio (lowest to highest risk)", color="",shape="" ) +
#   scale_colour_manual(name = "", 
#                       values =c('red'='red','blue'='blue'), 
#                       labels = c( 'Linear','Final model'))  
# 
# ggsave(filename = paste(plot_folder,"/Game_final_vs_linear.png",sep=""), plot = p, dpi = 300,width = 10, height = 8)
# 
#  
# p <- game_data %>% mutate(premium_Final_model_no_lasso = cumsum(Final_model_no_lasso*(Final_model_no_lasso < Final_model)),
#                      profit_Final_model_no_lasso = cumsum((Final_model_no_lasso - obs)*(Final_model_no_lasso < Final_model)),
#                      premium_Final_model = cumsum(Final_model*(Final_model_no_lasso > Final_model)),
#                      profit_Final_model = cumsum((Final_model - obs)*(Final_model_no_lasso > Final_model)),
#                      id=row_number()/n())     %>%
#   
#   ggplot(aes(x=id))+
#   geom_line(aes(y=profit_Final_model_no_lasso, colour="blue") )+
#   geom_line(aes(y=profit_Final_model, colour="red") )+
#   theme_classic()+
#   theme(legend.position = c(.3, .1))+
#   labs(
#     y="Profit (accumulated)",
#     x="% of portfolio (lowest to highest risk)", color="",shape="" ) +
#   scale_colour_manual( name="",
#                       values =c('red'='red','blue'='blue'), 
#                       labels = c( 'Final Model (no lasso)','Final Model')) 
# 
# ggsave(filename = paste(plot_folder,"/Game_final_vs_nolasso.png",sep=""), plot = p, dpi = 300,width = 10, height = 8)
# 

# Variable importance -----------------------------------------------------

load(paste("Data/Boost_data_",suffix,".RData", sep = ""))



best_lambda <- unique(models$final$lasso$lambda[min(models$final$lasso$cvm) == models$final$lasso$cvm])

VI <- varImp(models$final$lasso$glmnet.fit, lambda= best_lambda)

coef_info <- coef(models$final$lasso, s = best_lambda)
num_parameters <- length(coef_info)-sum(as.numeric(coef_info)==0)
# Concatenate with VI

coef_df <- as.data.frame(as.matrix(coef_info))
names(coef_df) <- c("Coefficient") 
factors <- gsub("^[^.]+\\.([^.]+)\\..*$", "\\1", rownames(coef_df))

# Sum by factor-group
coef_df["factor_name"] <- factors
coef_df <- coef_df %>% group_by(factor_name) %>% summarise(sum_VI = sum(abs(Coefficient))) %>% filter(factor_name != "Intercept")
 

if(suffix == "freMTPL"){
  coef_df$factor_name <- c("Area","BonusMalus", "Density","DrivAge","DrivAge:Region","Region","VehAge","VehAge:VehBrand","VehBrand","VehBrand:Density","VehGas","VehPower","VehPower:VehBrand")
}

if(suffix == "norauto"){
  coef_df$factor_name <- c("DistLimit","DistLimit:GeoRegion", "GeoRegion","Male","Male:DistLimit","Male:GeoRegion","Young","Young:DistLimit","Young:GeoRegion")
}

if(suffix == "beMTPL"){
  coef_df$factor_name <- c("Agec","Ageph", "Ageph:Agec","Ageph:Power","Ageph:sex","Bm","Bm:Power","Fleet","Fuel","Postcode","Power","Powerfuel","Sex","Use")
}

if(suffix == "auspriv"){
  coef_df$factor_name <- c("DrivAge","Gender", "VehAge","VehAge:VehBody","VehBody","VehBody:DrivAge","VehValue","VehValue:DrivAge","VehValue:VehAge","VehValue:VehBody")
}

# Plotting
p <- ggplot(coef_df, aes(x = reorder(factor_name, sum_VI), y = sum_VI)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() + # This flips the axes to make it a horizontal bar plot
  xlab("Factor Name") +
  ylab("Variable Importance") +
  ggtitle("Variable Importance Plot") +
  theme_classic()
ggsave(filename = paste(plot_folder,"/",suffix,"_variable_importance.png",sep=""), plot = p, dpi = 300,width = 10, height = 8)

# Percentile "SHAP" -------------------------------------------------------
  
boosting_df$train["pred_final"] <-pred$train$boosted_glm$lasso

# Get characteristics

glmnet_data <- model.matrix(models$final$functional_form_lasso , boosting_df$train_factors_final )

# Getting percentiles
p25 <- quantile(boosting_df$train$pred_final , probs = 0.25)
p50 <- quantile(boosting_df$train$pred_final, probs = 0.5)
p75 <- quantile(boosting_df$train$pred_final, probs = 0.75)

closest_row_index_p25 <- which.min(abs(boosting_df$train$pred_final - p25))
p25_char <- boosting_df$train[closest_row_index_p25, ]

closest_row_index_p50 <- which.min(abs(boosting_df$train$pred_final - p50))
p50_char <- boosting_df$train[closest_row_index_p50, ]

closest_row_index_p75 <- which.min(abs(boosting_df$train$pred_final - p75))
p75_char <- boosting_df$train[closest_row_index_p75, ]

# Get factor values
dim(glmnet_data)
final_reg_function <-sweep(as.data.frame(glmnet_data), MARGIN = 2, STATS = as.numeric(coef_info[-2,]), FUN = "*")
# Plot waterfall

p25_function <- final_reg_function[closest_row_index_p25,]
p50_function <- final_reg_function[closest_row_index_p50,]
p75_function <- final_reg_function[closest_row_index_p75,]

names(p25_function) <- gsub("^[^.]+\\(([^.]+)\\).*$", "\\1", names(p25_function))
names(p50_function) <- gsub("^[^.]+\\(([^.]+)\\).*$", "\\1", names(p50_function))
names(p75_function) <- gsub("^[^.]+\\(([^.]+)\\).*$", "\\1", names(p75_function))

# multiplicative

p25 <- data.frame(factor= names(p25_function), value=exp(as.numeric(p25_function))-1 ) %>% 
  group_by(factor) %>% 
  summarise(value=sum(value)+1) %>% 
  left_join(data.frame(value=t(p25_char), factor=names(p25_char)),by="factor") 


p50 <- data.frame(factor= names(p50_function), value=exp(as.numeric(p50_function))-1 ) %>% 
  group_by(factor) %>% 
  summarise(value=sum(value)+1) %>% 
  left_join(data.frame(value=t(p50_char), factor=names(p50_char)),by="factor") 

p75 <- data.frame(factor= names(p75_function), value=exp(as.numeric(p75_function))-1 ) %>% 
  group_by(factor) %>% 
  summarise(value=sum(value)+1) %>% 
  left_join(data.frame(value=t(p75_char), factor=names(p75_char)),by="factor") 


if(suffix == "freMTPL"){
  p25$factor <- c("(Intercept)","Area","BonusMalus", "Density","DrivAge","DrivAge:Region","Region","VehAge","VehAge:VehBrand","VehBrand","VehBrand:Density","VehGas","VehPower","VehPower:VehBrand")
  p50$factor <- c("(Intercept)","Area","BonusMalus", "Density","DrivAge","DrivAge:Region","Region","VehAge","VehAge:VehBrand","VehBrand","VehBrand:Density","VehGas","VehPower","VehPower:VehBrand")
  p75$factor <- c("(Intercept)","Area","BonusMalus", "Density","DrivAge","DrivAge:Region","Region","VehAge","VehAge:VehBrand","VehBrand","VehBrand:Density","VehGas","VehPower","VehPower:VehBrand")
}

if(suffix == "norauto"){
 p25$factor <- c("(Intercept)","DistLimit","DistLimit:GeoRegion", "GeoRegion","Male","Male:DistLimit","Male:GeoRegion","Young","Young:DistLimit","Young:GeoRegion")
 p50$factor <- c("(Intercept)","DistLimit","DistLimit:GeoRegion", "GeoRegion","Male","Male:DistLimit","Male:GeoRegion","Young","Young:DistLimit","Young:GeoRegion")
 p75$factor <- c("(Intercept)","DistLimit","DistLimit:GeoRegion", "GeoRegion","Male","Male:DistLimit","Male:GeoRegion","Young","Young:DistLimit","Young:GeoRegion")
 }

if(suffix == "beMTPL"){
  p25$factor <- c("(Intercept)","Agec","Ageph", "Ageph:Agec","Ageph:Power","Ageph:sex","Bm","Bm:Power","Fleet","Fuel","Postcode","Power","Powerfuel","Sex","Use")
  p50$factor <- c("(Intercept)","Agec","Ageph", "Ageph:Agec","Ageph:Power","Ageph:sex","Bm","Bm:Power","Fleet","Fuel","Postcode","Power","Powerfuel","Sex","Use")
  p75$factor <- c("(Intercept)","Agec","Ageph", "Ageph:Agec","Ageph:Power","Ageph:sex","Bm","Bm:Power","Fleet","Fuel","Postcode","Power","Powerfuel","Sex","Use")
}

if(suffix == "auspriv"){
  p25$factor <- c("(Intercept)","DrivAge","Gender", "VehAge","VehAge:VehBody","VehBody","VehBody:DrivAge","VehValue","VehValue:DrivAge","VehValue:VehAge","VehValue:VehBody")
  p50$factor <- c("(Intercept)","DrivAge","Gender", "VehAge","VehAge:VehBody","VehBody","VehBody:DrivAge","VehValue","VehValue:DrivAge","VehValue:VehAge","VehValue:VehBody")
  p75$factor <- c("(Intercept)","DrivAge","Gender", "VehAge","VehAge:VehBody","VehBody","VehBody:DrivAge","VehValue","VehValue:DrivAge","VehValue:VehAge","VehValue:VehBody")
}

p25 <- p25 %>% filter(factor != "(Intercept)") 
p50 <- p50 %>% filter(factor != "(Intercept)") 
p75 <- p75 %>% filter(factor != "(Intercept)") 

names(p25) <- c("factor","value","orig")
names(p50) <- c("factor","value","orig")
names(p75) <- c("factor","value","orig")

p25$orig[is.na(p25$orig)] <- "."
p50$orig[is.na(p50$orig)] <- "."
p75$orig[is.na(p75$orig)] <- "."

xmin <- min(p25$value,p50$value,p75$value)-0.05
xmax <- max(p25$value,p50$value,p75$value)+0.05

fig_dat <- list(p25=p25,p50=p50,p75=p75)
i<-1
for (dat in fig_dat){
   print(names(fig_dat)[i])
  print(dat)
  p <- dat  %>% mutate(factor=paste(factor, " = ", orig)) %>%
    ggplot(aes(y=factor, x=value)) + geom_point(size=5) + geom_vline(xintercept = 1, linetype="dashed",color="gray")+
    theme_classic() +
    theme(text=element_text(size=14)) +
    xlab("Feature contributions") +
    ylab("") +
    xlim(xmin,xmax)

  ggsave(filename = paste(plot_folder,"/",suffix,"_FC_", names(fig_dat)[i],".png",sep=""), plot = p, dpi = 300,width = 10, height = 8)
  i <- i+1

  }

prod(p75$value)

