
# Partial Dependence Plots - final models (incl bench) ---------------------------------

for (fact in num_facts){
  
  p <-  univariate_pdp_data_complete[[fact]] %>% 
    ggplot(aes(x=factor_val))+
    geom_line(aes(y=Final_model, color="red"))+
    geom_line(aes(y=Ref_gbm_interpol , color="grey"), lty=2) +
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


