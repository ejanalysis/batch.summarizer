## Wilcoxon Test Statistics
x <- c(white=0, black=0, native=0, multi=0, minority=9, ling.isol=1, 
  age0.4=41, age0.17=0, age65.up=57, no.hs.diploma=41, low.income=48)

## Sample Size (# of facilities w/ 3 mi. population >= 100)
n <- 13

## Calculate p-values for the Wilcoxon Signed Rank Test
wilcoxon.pvalues <- function(x,n) {
  m <- n*(n+1)/2
  p.values <- rep(NA, length(x))
  names(p.values) <- names(x)
  if (any(x <= m/2)) { 
    x1 <- which(x <= m/2)
    p.values[x1] <- psignrank(x[x1],n) + (1 - psignrank(m-x[x1],n))
  }
  if (any(x > m/2)) {
    x2 <- which(x > m/2)
    p.values[x2] <- psignrank(m-x[x2],n) + (1 - psignrank(x[x2],n))
  }
  return(round(p.values,4))
}

wilcoxon.pvalues(x,n)