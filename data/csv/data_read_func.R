data_read_func <- function(file) {
  data <- read.csv2(file)
  return(data)
}