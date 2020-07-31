expFit_intFree_asyMin <- function(x, p_id, pct_used, arg_asy){
  ## Fit the Eq. 1 to the time series!
  ## Return the characteristics of each event!
  # Keep in mind that lambda=1/exp(lrc) and standard error (SE) of lambda is SE(lambda)=lambda*SE(lrc)
  # Post processing is not included in these scripts!
  
  # Get the normalized date info!
  x$Date_num <- as.numeric(x$Date)
  x$Date_num <- x$Date_num - min(x$Date_num, na.rm = T)
  
  # Check if nlsLM model is going to converge!
  check <- try(nlsLM(Value ~ SSasymp(Date_num, Asym, R0, lrc), 
                     data = x, 
                     lower = c(arg_asy, 0, -6.579251), # min(lrc)=log(1/720) for 720days of lambda!
                     upper = c(arg_asy, 1, 0)), 
               silent = T)
  
  if (!(class(check) == "try-error")){
    # If converged, get the coefficients of the nlsLM model together with Nash-Sutcliffe efficiency!
    tmp <- summary(check)[["coefficients"]]
    
    tmp_total <- data.frame("Period_ID" = p_id, "pct_used" = pct_used, "Duration" = x$Duration[1],
                            "V0" = tmp[2,1], "V0_stderr" = tmp[2,2],
                            "lrc" = tmp[3,1], "lrc_stderr" = tmp[3,2],
                            "NSE" = NSE(sim = predict(check, x$Date_num), obs = x$Value))
    return(tmp_total)
  } 
}
