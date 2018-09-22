data_read_func <- function(file) {
  # library(gdata)
  library(readxl)
  # csv <- gdata::xls2csv(file,sheet=3)
  # data <- read.csv(csv)
  
  DF1 <- readxl::read_excel(file,sheet = 3, n_max = 2)
  data <-readxl::read_excel(file,sheet = 3, skip = 2, col_names = paste(names(DF1), DF1))
  # 
  # data <- read_excel(file,sheet = "Zeitreihen0h15")
  return(data)
}