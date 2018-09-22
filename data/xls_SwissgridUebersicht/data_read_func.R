data_read_func <- function(file) {
  header <- read.xlsx2(file = file,sheetName = "Zeitreihen0h15",startRow = 1,endRow = 2,colClasses = rep("character",times=51))
  header <- paste(names(header), header)
  data <- read.xlsx2(file = file,sheetName = "Zeitreihen0h15",startRow = 3,colClasses = rep("numeric",times=51),header = F)
  data[,1] <- as.POSIXct(data[,1] * (60*60*24), origin="1899-12-30", tz="GMT")
  colnames(data) <- header
  
  # data <- read_excel(file,sheet = "Zeitreihen0h15")
  return(data)
}