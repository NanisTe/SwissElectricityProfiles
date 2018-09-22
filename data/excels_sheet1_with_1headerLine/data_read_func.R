data_read_func <- function(file) {
  # library(gdata)
  library(readxl)
  # csv <- gdata::xls2csv(file,sheet=3)
  # data <- read.csv(csv)
  
  DF1 <- readxl::read_excel(file,sheet = 1, n_max = 1)
  data <-readxl::read_excel(file,sheet = 1, skip = 1, col_names = paste(names(DF1)),
                            col_types = c("numeric",
                                          "date",
                                          "date",
                                          rep("numeric",13)))
  # 
  # data <- read_excel(file,sheet = "Zeitreihen0h15")
  return(data)
}