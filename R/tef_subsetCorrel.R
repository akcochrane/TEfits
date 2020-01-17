

#' Find mean correlations of subsets of vectors
#' 
#' Selects subsets of x and y vectors,
#' and calculates the mean correlations between the two subsets.
#'
#' @param x vector to correlate
#' @param y vector to correlate
#' @param method  correlation method (e.g., "spearman" or "pearson")
#' @param returnVal return "mean" or (if subsample is random) "quantiles"
#' @param subsample should the subsamples be "even_odd" or "random" (half of the first half, half of the second half)
#' @param iter if subsample is "random", number of times the vectors are split, each half is subset, those subsets are correlated, and those correlations are averaged
#' 
#'
#' @export
#'
tef_subsetCorrel <- function(x,y,method='spearman',returnVal='mean',subsample='even_odd',iter=200){
  
  if(length(y)==length(x)){
    
    if(subsample=='random'){
  correls <- replicate(iter,
                       {
    sub_set1 <- sample(floor(length(x)/2),ceiling(length(x)/4))
    sub_set2 <- sample(ceiling(length(x)/2):length(x),ceiling(length(x)/4))
    mean(c(
      cor(x[sub_set1],y[sub_set1],use='complete.obs',method=method)
      ,
      cor(x[sub_set2],y[sub_set2],use='complete.obs',method=method)
    ))
  }
  )
    if(returnVal=='quantiles'){
    return(quantile(correls,c(.025,.5,.975)))
  }
  
    }
    if(subsample=='even_odd'){
      sub_set1 <- ((1:length(x))/2)==floor((1:length(x))/2)
      sub_set2 <- !sub_set1
      correls <- mean(c(
        cor(x[sub_set1],y[sub_set1],use='complete.obs',method=method)
        ,
        cor(x[sub_set2],y[sub_set2],use='complete.obs',method=method)
      ))
    }
    
  return(mean(correls))

  }else{cat('\ntef_subsetCorrel: the two vectors to correlate are not the same length.\n')}
}
