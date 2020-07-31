exitCorrector <- function(data_points, scaleFactor=0.05){
  #### Push the end of each decay event to a certain amount of increase in the time series!
  ## data_points should be the output of the function smoothDetector!
  ## scaleFactor defines the amount of increase the decay event has to have in order to finish, as the ratio of the amplitude
  
  ## 1) Get the amplitude and last value of each event
  tmp <- data_points %>%
    group_by(Cell_ID, Period_ID, Trend) %>%
    summarise(eventAmp = max(mean_31, na.rm = T) - min(mean_31, na.rm = T),
              lastVal = mean_31[Date == max(Date, na.rm = T)])
  # Calculate the exit value!
  tmp$exitValue <- tmp$eventAmp * scaleFactor + tmp$lastVal
  # Remember that exit values are for the following event. So correct the ID's for Greening! 
  tmp$Period_ID <- ifelse(tmp$Trend == "Drying", tmp$Period_ID + 1, NA)
  
  ## 2) Merge it back with data points!
  data_points <- data_points %>% 
    left_join(dplyr::select(tmp, c(Cell_ID, Period_ID, exitValue)),
              by = c("Cell_ID", "Period_ID"))
  
  ## 3) Correct the Period!
  data_points$Period[data_points$mean_31 < data_points$exitValue] <- 0
  
  ## 4) Roll down to smooth the edited class codes with a window of 5!
  data_points <- data_points %>%
    group_by(Cell_ID) %>%
    mutate(Period_smoothed5 = rollapplyr(Period, width = 5, FUN = mean, align = "center", na.rm = T, fill = NA))
  # Free up the memory!
  data_points <- attrCleaner(data_points)
  
  ## 5) Find the beginning and end of the seasons!
  data_points$Start <- NA
  data_points$Start[data_points$Period_smoothed5 == 1] <- T
  data_points$Drying <- NA
  data_points$Drying[data_points$Period_smoothed5 >= 0] <- T
  
  ## Ok, set an ID for each change on column Start
  data_points$tmp_ID_begin <- NA
  # Get the index of the change in column "Start"!
  ind_tmp <- which(colnames(data_points) == "Start")
  # Below is a strange but memory efficient way of getting the indexes of the changes in the vectors!
  tmp_indexes <- data.table:::uniqlist(list(data_points[, ind_tmp]))
  # Free up the memory!
  data_points <- attrCleaner(data_points)
  
  data_points$tmp_ID_begin[tmp_indexes] <- 1:length(tmp_indexes)
  # Add the tmp_ID_begin's for everywhere
  data_points$tmp_ID_begin <- zoo::na.locf(data_points$tmp_ID_begin)
  
  ## Do the same procedure to detect the end of dry seasons!
  data_points$tmp_ID_end <- NA
  # Get the index of the change in column "Drying"!
  ind_tmp <- which(colnames(data_points) == "Drying")
  tmp_indexes <- data.table:::uniqlist(list(data_points[, ind_tmp]))
  # Free up the memory!
  data_points <- attrCleaner(data_points)
  
  data_points$tmp_ID_end[tmp_indexes] <- 1:length(tmp_indexes)
  # Add the tmp_ID's for everywhere
  data_points$tmp_ID_end <- zoo::na.locf(data_points$tmp_ID_end)
  
  ## 6) Summarise and define the start and end of the seasons!
  dry_start <- data_points %>% subset(Start == T) %>%
    group_by(Cell_ID, tmp_ID_begin, tmp_ID_end) %>%
    summarise(dry_begin = dplyr::first(Date))
  dry_finish <- data_points %>% subset(Drying == T) %>%
    group_by(Cell_ID, tmp_ID_end) %>%
    summarise(dry_end = dplyr::last(Date))
  
  # Free up the memory!
  data_points <- attrCleaner(data_points)
  dry_start <- attrCleaner(dry_start)
  dry_finish <- attrCleaner(dry_finish) 
  
  # Merge the start and end information with the original dataframe!
  data_points <- merge(data_points, dry_start, 
                       by = c("Cell_ID", "tmp_ID_begin", "tmp_ID_end"), 
                       all = T)
  data_points <- merge(data_points, dry_finish, 
                       by = c("Cell_ID", "tmp_ID_end"), 
                       all = T)
  
  # dry_begin should be the first of all observations with TRUE values!
  data_points <- data_points %>% group_by(Cell_ID, dry_end) %>%
    mutate(dry_begin = min(dry_begin, na.rm = T))
  
  # Finally, record the Trend!
  data_points$Trend <- "Greening"
  data_points$Trend[(data_points$Date >= data_points$dry_begin) & (data_points$Date <= data_points$dry_end)] <- "Drying"
  
  # Get the duration per trend!
  data_points$Duration <- difftime(data_points$dry_end, data_points$dry_begin, units = "days")
  # Make sure there's no redundant information about Duration for greening!
  data_points$Duration[data_points$Trend == "Greening"] <- NA
  
  # Free up some space!
  data_points <- dplyr::select(data_points, -c(tmp_ID_begin, tmp_ID_end, Start, Drying))
  data_points <- attrCleaner(data_points)
  
  ## 7) Assign the Period_ID's!
  # Make sure that the data is arranged well!
  data_points <- dplyr::arrange(data_points, Cell_ID, Date)
  data_points$Period_ID <- as.numeric(NA)
  
  # Get the indexes of changes in "Trend"
  ind_tmp <- which(colnames(data_points) == "Trend")
  tmp_indexes <- data.table:::uniqlist(list(data_points[, ind_tmp]))
  # Free up the memory!
  data_points <- attrCleaner(data_points)
  
  # Assign a Period_ID for each change in the column interested!
  data_points$Period_ID[tmp_indexes] <- 1:length(tmp_indexes)
  # Add the tmp_ID's for everywhere!
  data_points$Period_ID <- zoo::na.locf(data_points$Period_ID)
  # Free up some space!
  data_points <- attrCleaner(data_points)
  
  ## Lastly, clear up the Period_ID's for Greening as they're out of interest!
  data_points$Period_ID[data_points$Trend == "Greening"] <- NA
  
  ## Return a concise dataframe with:
  # 1) Trend: string as "Greening" or "Drying". Both classes have to end with Period values of 0!  
  # 2) ID of the Trends as a grip for the individual trends!
  ## THIS CHUNK SHOULD BE UPDATED!
  concise <- select(data_points, c("Cell_ID", "Date", "Value", "Period_ID", "Trend", "Duration"))
  return(concise)
}