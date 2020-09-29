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
