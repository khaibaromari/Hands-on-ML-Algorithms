split_to_train_test <- function(df, train_prop){
  train_rows <- sample(1:nrow(df), size = train_prop * nrow(df))
  
  splitted_dfs <- list(
    train_data = df[train_rows,],
    test_data = df[-train_rows,]
  )
  
  return(splitted_dfs)
}