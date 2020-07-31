smoothDetector <- function(data_points){
  ### data_points should be a dataframe with 3 columns: 
  ### 1) Cell_ID to manage multiple grid cells, 
  ### 2) Date of each observation
  ### 3) Value of each observation
  
  ## Make sure the data is arranged with Cell_ID's and then the Date!
  data_points <- dplyr::arrange(data_points, Cell_ID, Date)
  
  ## 1) Smooth the data with a moving window of 31 days!
  data_points <- data_points %>%
    group_by(Cell_ID) %>%
    mutate(mean_31 = rollapplyr(Value, width = 31, FUN = mean, align = "center", na.rm = T, fill = NA))
  # Free up the memory!
  data_points <- attrCleaner(data_points)
  
  ## 2) Get the difference within days in smoothed data!
  data_points <- data_points %>% group_by(Cell_ID) %>% mutate(Diff = c(NA, diff(mean_31)))
  # Free up the memory!
  data_points <- attrCleaner(data_points)
  
  ## Ok, set the treshold for beginning and end!
  data_points <- data_points %>% group_by(Cell_ID) %>%
    mutate(th_begin = quantile(Diff[Diff < 0], na.rm = T, probs = 0.75),
           th_end = quantile(Diff[Diff < 0], na.rm = T, probs = 0.70))
  # Free up the memory!
  data_points <- attrCleaner(data_points)
  
  ## 3) Assign classes to the dates wrt change! Threshold is given with suppporting argument!
  data_points$Period <- as.numeric(NA)
  data_points$Period[data_points$Diff < data_points$th_begin] <- 1
  data_points$Period[(data_points$Diff >= data_points$th_begin) & (data_points$Diff <= -data_points$th_end)] <- 0
  data_points$Period[data_points$Diff > -data_points$th_end] <- -1
  
  ## 4) Roll down to smooth the class codes with a window of 5!
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
  # Get the index of the change in "Period_smoothed5"!
  ind_tmp <- which(colnames(data_points) == "Start")
  # Below is a weird but memory efficient way of getting the indexes of the changes in the vectors!
  tmp_indexes <- data.table:::uniqlist(list(data_points[, ind_tmp]))
  # Free up the memory!
  data_points <- attrCleaner(data_points)

  data_points$tmp_ID_begin[tmp_indexes] <- 1:length(tmp_indexes)
  # Add the tmp_ID_begin's for everywhere
  data_points$tmp_ID_begin <- zoo::na.locf(data_points$tmp_ID_begin)
  
  ## Do the same procedure to detect the end of dry seasons!
  data_points$tmp_ID_end <- NA
  # Get the index of the change in "Period_smoothed5"!
  ind_tmp <- which(colnames(data_points) == "Drying")
  tmp_indexes <- data.table:::uniqlist(list(data_points[, colnames(data_points) == "Drying"]))
  # Free up the memory!
  data_points <- attrCleaner(data_points)

    data_points$tmp_ID_end[tmp_indexes] <- 1:length(tmp_indexes)
  # Add the tmp_ID's for everywhere
  data_points$tmp_ID_end <- zoo::na.locf(data_points$tmp_ID_end)
  
  # Let's remove some unnecessary columns to save from the memory!
  data_points <- dplyr::select(data_points, -c(Diff, th_begin, th_end))
  data_points <- attrCleaner(data_points)
  
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
  
  # Free up some space!
  data_points <- dplyr::select(data_points, -c(tmp_ID_begin, tmp_ID_end, Start, Drying))
  data_points <- attrCleaner(data_points)
  
  ## 7) Assign the Period_ID's!
  # Make sure that the data is arranged well!
  data_points <- dplyr::arrange(data_points, Cell_ID, Date)
  data_points$Period_ID <- as.numeric(NA)
  
  # Line below is only to avoid hard-coding. Tell me which column to inspect!
  ind_tmp <- which(colnames(data_points) == "Trend")
  # Get the indexes of changes
  tmp_indexes <- data.table:::uniqlist(list(data_points[, ind_tmp]))
  # Free up the memory!
  data_points <- attrCleaner(data_points)
  
  # Assign a Period_ID for each change in the column interested!
  data_points$Period_ID[tmp_indexes] <- 1:length(tmp_indexes)
  # Add the tmp_ID's for everywhere!
  data_points$Period_ID <- zoo::na.locf(data_points$Period_ID)
  # Free up some space!
  data_points <- attrCleaner(data_points)
  
  ## Return a concise version of the original dataframe with:
  # 1) mean_31: Smoothed version of the values with the mean of a 31-day moving window
  # 2) Period: -1, 0, 1 for increase, stable and decay in the time series
  # 3) Trend: string as "Greening" or "Drying". Both classes have to end with Period values of 0!  
  # 4) ID of the Trends as a grip for the individual trends!
  concise <- select(data_points, c("Cell_ID", "Date", "Value", "mean_31", "Period", "Period_ID", "Trend"))
  return(concise)
}
