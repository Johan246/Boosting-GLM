

### LEAVE MULTVAR-EFFECTS FOR NOW ###

### When single effects are finished (do brief report for course) ###



# Friedman's H
n_fakt <- length(facts)


for (i in 1:n_fakt){
  if (i == 1 ){
    H_stat <- data.frame() 
    M_stat <- data.frame()
  }
  for (j in 1:n_fakt){
    
    # Friedmans H-stat för varje faktorkombination
    H_stat <- bind_rows(H_stat, data.frame(Faktor_1 = facts[i],
                                           Faktor_2 = facts[j],
                                           
                                           H_stat = interact.gbm(models$gbm_boost,
                                                                 data= boosting_df$train,
                                                                 n.trees = models$gbm_boost.ntrees,
                                                                 i.var = c(facts[i],facts[j]) )))
    
    # M_stat <- bind_rows(M_stat, data.frame(Faktor_1 = facts[i],
    #                                        Faktor_2 = facts[j],
    #                                        
    #                                        M_stat = m_stat_beta(models$gbm_boost,
    #                                                              data= boosting_df$train,
    #                                                              n.trees = models$gbm_boost.ntrees,
    #                                                              i.var = c(facts[i],facts[j]) )))
    
    
  }
  print(paste("Klar med interaktioner för", facts[i]))
}

if (plot_during_algo == TRUE){
  ggplot(H_stat, aes(Faktor_1, Faktor_2, fill= H_stat)) + 
    geom_tile(color="white")+
    geom_text(aes(label = round(H_stat,2)), color = "white", size = 4)  +
    ggsave(paste(plot_folder,"/Hstat_boost_",fact , ".png",sep=""))
  
  ggplot(M_stat, aes(Faktor_1, Faktor_2, fill= M_stat)) + 
    geom_tile(color="white")+
    geom_text(aes(label = round(H_stat,2)), color = "white", size = 4)  +
    ggsave(paste(plot_folder,"/Mstat_boost_beta_",fact , ".png",sep=""))
}


# Get Two-factor PDP ------------------------------------------------------

# # OBSOBS: Följande är pseudokod!
# pdp_two <- partial(model$gbm_boost, pred.var = c("Byggnadstyp" , "Brevobjekt"))
# pdp_norm <- univariate_pdp_data[["Byggnadstyp"]]*univariate_pdp_data[["Brevobjekt"]]
# 
# two_factor_effect <- pdp_two/pdp_norm


