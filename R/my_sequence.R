#' @title Sequence Function
#'
#'@description This function takes account a vector input containing the first 3 numeric
#'of this sequence and a positive (>0) integer n denoting the final nth element of the sequence to calculate
#'
#' @param x Integers
#' @param n Length
#'
#' @return
#' @export  myseq_n
#'
#' @examples

myseq_n <- function(x, n){
  nums <- vector(mode = "integer", length = n)

  for (i in seq_along(nums)) {
    if(i <= 3){
      nums[i] <- x[i]
    }else {
      nums[i] <- nums[i-1] + (nums[i-3] - nums[i-2])/i
    }
    if(length(x) > 3){
      stop("input too long")
    }
  }
  return(nums[n])
}
