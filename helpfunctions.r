
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

