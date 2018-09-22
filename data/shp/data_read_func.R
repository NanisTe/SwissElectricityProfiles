data_read_func <- function(file) {
  data <- readOGR(file)
  return(data)
}