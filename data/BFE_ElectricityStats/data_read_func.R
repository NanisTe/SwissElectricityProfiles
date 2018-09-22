data_read_func <- function(file) {
  library(gdata)
  library(readxl)
  
  DF1 <- readxl::read_excel(file,sheet = 1, skip = 8, col_names = F)
  data <-readxl::read_excel(file,sheet = 1, skip = 9, col_names = unlist(DF1[1,]))
  
  # data <- read_excel(file,sheet = "Zeitreihen0h15")
  return(data)
}