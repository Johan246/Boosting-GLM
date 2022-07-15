
# Predictive performance (MSEP) --------------------------------------------------

data.frame(Pred = pred$test$boost  ,
           obs = boosting_df$test$freq) %>%
  
  mutate(bin = ntile(Pred, 500)) %>% group_by(bin) %>%
  summarise(Pred = mean(Pred),
            obs = mean(obs)) %>%
  ggplot(aes(x=bin)) + 
  geom_point(aes(y=obs)) +
  labs(title="Predictions Boosted Tariff",
       subtitle="policies are grouped into 500 risk groups, red line is mean prediction and 
       black dot is mean frequency within group")+
  geom_line(aes(y=Pred), size=1, colour="red") +
  theme_classic()+
  ggsave(paste("Plottar/",suffix,"_",date,"/Predictions_Boosted.png",sep=""))


data.frame(Pred = pred$test$init  ,
           obs = boosting_df$test$freq) %>%
  
  mutate(bin = ntile(Pred, 500)) %>% group_by(bin) %>%
  summarise(Pred = mean(Pred),
            obs = mean(obs)) %>%
  ggplot(aes(x=bin)) + 
  geom_point(aes(y=obs)) +
  labs(title="Predictions Initial Tariff",
       subtitle="policies are grouped into 500 risk groups, red line is mean prediction and 
       black dot is mean frequency within group")+
  geom_line(aes(y=Pred), size=1, colour="red") +
  theme_classic()+
  ggsave(paste("Plottar/",suffix,"_",date,"/Predictions_tariff.png",sep=""))

data.frame(Pred = pred$test$ref ,
           obs = boosting_df$test$freq) %>%
  
  mutate(bin = ntile(Pred, 500)) %>% group_by(bin) %>%
  summarise(Pred = mean(Pred),
            obs = mean(obs)) %>%
  ggplot(aes(x=bin)) + 
  geom_point(aes(y=obs)) +
  labs(title="Predictions Reference Tariff",
       subtitle="policies are grouped into 500 risk groups, red line is mean prediction and 
       black dot is mean frequency within group") +
  geom_line(aes(y=Pred), size=1, colour="red") +
  theme_classic()+
  ggsave(paste("Plottar/",suffix,"_",date,"/Predictions_reference.png",sep=""))


MSEP <- data.frame( Model=c("Reference", "Boosted", "Init-tariff"),
                    
                    
                    MSEP=c( mean((pred$test$ref - boosting_df$test$freq) ^2), 
                            mean((pred$test$boost - boosting_df$test$freq) ^2),
                            mean((pred$test$init - boosting_df$test$freq) ^2)))

p_msep <- ggplot(tibble(x=0,y=0, tb=list(MSEP ))) +
  theme_void() + 
  geom_table(aes(x, y, label = tb),parse = TRUE)  +
  theme(plot.title=element_text(size=12, hjust=0.5, face="bold", colour="black", vjust=-1),
        plot.subtitle=element_text(size=8, hjust=0.5, face="italic", color="black"))+
  ggsave(paste("Plottar/",suffix,"_",date,"/MSEP.png",sep=""), width=10, height=8)   

