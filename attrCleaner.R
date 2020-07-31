attrCleaner <- function(points_df){
  # Get rid of the attr!
  attr(points_df, "vars") <- NULL
  attr(points_df, "drop") <- NULL
  attr(points_df, "indices") <- NULL
  attr(points_df, "labels") <- NULL
  attr(points_df, "group_sizes") <- NULL
  attr(points_df, "biggest_group_size") <- NULL
  attr(points_df, "groups") <- NULL
  points_df <- data.frame(points_df)
  gc()
  return(points_df)
}
