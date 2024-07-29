mase_scale = function(x) {
  frq = floor(frequency(x))
  if (length(x) < frq) {
    frq = 1
  }
  mean(abs(head(as.vector(x), -frq) - tail(as.vector(x), -frq))**1)
}