my_seq_plot <- function(x) {
  output2 <- vector("double")
  is.data.frame(x) != T
  if (ncol(x) == 4) {
    for (i in 1:nrow(x)) {
      a <- as.numeric(x[i, 1:3])
      b <- as.integer(x[i, 4])
      output2[[i]] <- myseq_n(a, b)
    }
  }else{
    return("must be 4 inputs per row")
  }
  x[,4] %>%
    cbind(output2) %>%
    ggplot(aes(x = n, y = output2)) +
    geom_line() -> the_plot
  return(the_plot)
}
