easyscale <- function(df, col, option){
  if(option == "minmax"){
    tmp <- as.data.frame(sapply(df[col], function(x){(x-min(x)) / (max(x)-min(x))}))
    df[col] <- tmp
  } else{
    tmp <- as.data.frame(scale(df[col]))
    df[col] <- tmp
  }
  return(df)
}
