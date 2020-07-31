expFit_process <- function(data_points, asy_df){
  ## Complete procedure to define the dry down period and fit Eq. 1 to characterise the decay
  # data_points should have unique Cell_ID's, Date, Value and unique Period_ID's for each decay period!
  # asy_df should be a dataframe with two columns, Cell_ID's and corresponding asymptote values under column "asy"!
  
  ## Base preparations!
  # Calculate the dV!
  data_points <- data_points %>% group_by(Cell_ID) %>% mutate(Diff = c(NA, diff(Value)))
  # Smooth the dV!
  data_points <- data_points %>%
    group_by(Cell_ID) %>%
    mutate(dV_mean_31 = rollapplyr(Diff, width = 31, FUN = mean, align = "center", na.rm = T, fill = NA))
  # Free up some space!
  data_points <- attrCleaner(data_points)
  
  # Calculate d2V from smoothed dV!
  data_points <- data_points %>% group_by(Cell_ID) %>% mutate(d2V = c(NA, diff(dV_mean_31)))
  # Free up some space!
  data_points <- attrCleaner(data_points)
  
  # Smooth the d2V!
  data_points <- data_points %>%
    group_by(Cell_ID) %>%
    mutate(d2V_mean_31 = rollapplyr(d2V, width = 31, FUN = mean, align = "center", na.rm = T, fill = NA))
  
  # Filter out the rows with empty values!
  data_points <- subset(data_points, Value >= 0)
  
  # Filter out the observations that are out of the target!
  data_points <- subset(data_points, Period_ID > 0)
  # Free up some space!
  data_points <- attrCleaner(data_points)
  
  ## Rest of the procedure will be done in loops...
  # Have a vector of Cell_ID's!
  cellIDs <- unique(data_points$Cell_ID)
  # Get ready for the loop of the grids!
  fit_grids <- data.frame()
  
  for (i in 1:length(cellIDs)){  # Loop for gridCells!
    # Subset
    grid_focus <- subset(data_points, Cell_ID == cellIDs[i])
    
    # Get the asy from pixel_df!!!
    arg_asy <- subset(asy_df, Cell_ID == cellIDs[i])$asy
    
    ## Preparetions for loop for events!
    # List of Period_ID's
    tmp_events <- unique(grid_focus$Period_ID)
    # First save the fitted values per grid cell!
    fit_events <- data.frame()
    
    # j <-2
    for (j in 1:length(tmp_events)) {   # Loop for the events of the grid cell!
      # Subset for event!
      event_focus <- subset(grid_focus, Period_ID == tmp_events[j])
      
      #### Dry-down detection ####
      ## Get the local minima of dV_mean_31, i.e., inflection point, with a 1/3 of the full dataset as a spanlenght!
      # Define the odd number span beforehand!
      tmp_span <- round(length(event_focus$dV_mean_31)/3)
      tmp_span <- ifelse(tmp_span %% 2 == 0, tmp_span + 1, tmp_span)
      
      # Make sure the event is fine!
      if(dim(event_focus[complete.cases(event_focus), ])[1] > 0){
        inds_min <- which(peaks(-event_focus$dV_mean_31, span = tmp_span) == T)
        # Make sure that there's a minimum in dV_mean_31!
        if(length(inds_min) > 0){
          
          # Filter the observations before the inflection point!
          dat_focus <- event_focus[inds_min[1]:dim(event_focus)[1],]
          
          # Run expFit function only if more than half of the event is convex
          pct_used <- length(which(dat_focus$d2V_mean_31 > 0)) / length(dat_focus$d2V_mean_31)
          if(pct_used >= 0.5){
            # Make sure there's no concave observations, even after the inflection point
            dat_focus <- subset(dat_focus, d2V_mean_31 >= 0)
            
            ## Fit the curve and record the parameters!
            tmp_fit <- expFit_intFree_asyMin(dat_focus, dat_focus$Period_ID[1], pct_used, arg_asy)
            fit_events <- rbind(fit_events, tmp_fit)
          } # end of if_pctUsed
        } # end of if_indsMin
      } # end of if_eventIsComplete!
    } # end of events loop, j!
    
    # Add the parameters of the event to the dataframe for the parameters of all gridCells
    if(dim(fit_events)[1] > 0){
      # Add the missing elements of the fits df!
      fit_events$Cell_ID = grid_focus$Cell_ID[1]
      fit_events$asy = arg_asy
      # Merge this with fit_grids!
      fit_grids <- rbind(fit_grids, fit_events)
    }
  } # end of gridCell loop, i!
  return(fit_grids)
}
