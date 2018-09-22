data_read_func <- function(file) {
  library(gdata)
  library(readxl)
  
  DF1 <- gdata::read.xls(file,sheet = 1, nrows = 1)
  data <-readxl::read_excel(file,sheet = 1, skip = 0, col_names = paste(names(DF1), DF1))
  
  # data <- read_excel(file,sheet = "Zeitreihen0h15")
  return(data)
}